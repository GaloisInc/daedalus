-- Assumes borrow analysis has been done
-- Insert a copy when we need to pass a value as an owned argument.
{-# Language BlockArguments, OverloadedStrings #-}
module Daedalus.VM.InsertCopy (addCopyIs) where

import Control.Monad(zipWithM,ap,liftM)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(mapAccumL)

import Daedalus.PP(pp)
import Daedalus.Panic(panic)

import Daedalus.Core(FName)
import Daedalus.VM
import Daedalus.VM.BorrowAnalysis
import Daedalus.VM.TypeRep
import Daedalus.VM.FreeVars


addCopyIs :: Program -> Program
addCopyIs p = p { pBoot    = annotateBlock ro <$> pBoot p
                , pModules = map annModule (pModules p)
                }
  where
  annModule m = m { mFuns = map annFun (mFuns m) }
  annFun f    = f { vmfBlocks = annotateBlock ro <$> vmfBlocks f }
  ro          = buildRO p

buildRO :: Program -> RO
buildRO p = foldr addFun initRO [ f | m <- pModules p, f <- mFuns m ]
  where
  initRO = RO { funMap = Map.empty
              , labOwn = blockSig <$> pBoot p
              }

  addFun f i =
    let ls = blockSig <$> vmfBlocks f
        fs = ls Map.! vmfEntry f
    in RO { funMap = Map.insert (vmfName f) fs (funMap i)
          , labOwn = Map.union ls (labOwn i)
          }

  blockSig b = map getOwnership (blockArgs b)


annotateBlock :: RO -> Block -> Block
annotateBlock ro = rmRedundantCopy . insertFree . insertCopy ro


--------------------------------------------------------------------------------

rmRedundantCopy :: Block -> Block
rmRedundantCopy b = b { blockInstrs = is, blockTerm = c }
  where
  (is,c) = doRmRedundat Set.empty (blockInstrs b) (blockTerm b)

doRmRedundat :: Set VMVar -> [Instr] -> CInstr -> ([Instr],CInstr)
doRmRedundat prevFree is term =
  case is of
    -- Make a copy, and immediately dallocate source
    Let x e : Free xs : more
      | Just y <- eIsVar e, y `Set.member` xs
      , let xs' = Set.delete y xs ->
        doRmRedundat prevFree (doSubst x y (Free xs' : more)) (doSubst x y term)

    Free xs : more -> doRmRedundat (xs `Set.union` prevFree) more term

    i : more
      | Set.null prevFree -> (                i : js, t)
      | otherwise         -> (Free prevFree : i : js, t)
      where
      more'  = map (noFree (consumes i)) more
      (js,t) = doRmRedundat Set.empty more' term

    [] -> ([],term)

  where
  -- Variables "consumed" by an instruction
  consumes (Let _ _) = Set.empty
  consumes i = Set.fromList [ x | (e,Owned) <- iArgs i `zip` modeI i
                                , Just x <- [eIsVar e] ]

  -- Don't "free" these because we gave them away to another function.
  noFree xs i =
    case i of
      Free ys -> Free (Set.difference ys xs)
      _       -> i



-- | Replace a local variable with another variable.
class DoSubst t where
  doSubst :: BV -> VMVar -> t -> t

instance DoSubst t => DoSubst [t] where
  doSubst x e = fmap (doSubst x e)

instance DoSubst t => DoSubst (Maybe t) where
  doSubst x e = fmap (doSubst x e)

instance DoSubst Instr where
  doSubst x v i =
    case i of
      SetInput e      -> SetInput (doSubst x v e)
      Say {}          -> i
      Output e        -> Output (doSubst  x v e)
      Notify e        -> Notify (doSubst  x v e)
      CallPrim y p es -> CallPrim y p (doSubst x v es)
      GetInput {}     -> i
      Spawn y l       -> Spawn y (doSubst x v l)
      NoteFail        -> i
      Let y e         -> Let y (doSubst x v e)
      Free xs
        | x' `Set.member` xs -> Free (Set.insert v (Set.delete x' xs))
        | otherwise          -> i
        where x' = LocalVar x

instance DoSubst CInstr where
  doSubst x v ci =
    case ci of
      Jump l          -> Jump (doSubst x v l)
      JumpIf e l1 l2  -> JumpIf (doSubst x v e)
                                (doSubst x v l1) (doSubst x v l2)
      Yield           -> Yield
      ReturnNo        -> ReturnNo
      ReturnYes e     -> ReturnYes  (doSubst x v e)
      ReturnPure e    -> ReturnPure (doSubst x v e)
      Call f c l1 l2 es -> Call f c (doSubst x v l1) (doSubst x v l2)
                                                     (doSubst x v es)
      TailCall f c es -> TailCall f c (doSubst x v es)

instance DoSubst JumpPoint where
  doSubst x v (JumpPoint l es) = JumpPoint l (doSubst x v es)

instance DoSubst E where
  doSubst x v e =
    case e of
      EVar y | x == y -> case v of
                           LocalVar z -> EVar z
                           ArgVar z   -> EBlockArg z
      _               -> e



--------------------------------------------------------------------------------

-- | Insert `free` after the last use of owned variables that have references.
insertFree :: Block -> Block
insertFree b = b { blockInstrs = newIs }
  where
  (finLive,iss) = mapAccumL updI (freeVarSet (blockTerm b))
                                 (reverse (blockInstrs b))
  baSet      = Set.fromList [ ArgVar v | v <- blockArgs b ]
  topFreeIs  = mkFree (Set.difference baSet finLive)
  newIs      = topFreeIs ++ concat (reverse iss)

  updI :: Set VMVar -> Instr -> (Set VMVar, [Instr])
  updI live i = (newLive, i : freeIs)
    where
    used     = freeVarSet i
    newLive  = Set.union used live `Set.difference`
                                                (LocalVar `Set.map` defineSet i)
    freeIs   = mkFree (Set.difference used live)


  mkFree vs = [ Free vs' | let keep v = getOwnership v == Owned &&
                                        typeRep (getType v) == HasRefs
                               vs'    = Set.filter keep vs
                         , not (Set.null vs')
              ]



--------------------------------------------------------------------------------

insertCopy :: RO -> Block -> Block
insertCopy ro b = b { blockLocalNum = newLocalNum
                    , blockInstrs = is
                    , blockTerm = cinstr
                    }
  where
  (cinstr,newLocalNum,is) =
     runM ro (blockLocalNum b)
     do mapM_ doInstr (blockInstrs b)
        doCInstr (blockTerm b)

-- Insert a copy instructions whenever something exepcts and owned argument,
-- and the type contains refs.
doInstr :: Instr -> M ()
doInstr instr =
  case instr of
    SetInput e      -> owned SetInput e
    Say {}          -> emit instr
    Output e        -> owned Output e
    Notify {}       -> emit instr
    CallPrim x p es ->
      do es1 <- doArgs es (modePrimName p)
         emit (CallPrim x p es1)

    GetInput {}    -> emit instr
    Spawn x (JumpPoint l es) ->
      do es1 <- mapM copy es
         emit (Spawn x (JumpPoint l es1))
    NoteFail        -> emit instr
    Let {}          -> emit instr
    Free {}         -> emit instr

  where
  owned f e =
    do e1 <- copy e
       emit (f e1)

doArgs :: [E] -> [Ownership] -> M [E]
doArgs es ms = zipWithM doArg es ms

doArg :: E -> Ownership -> M E
doArg e m =
  case m of
    Owned    -> copy e
    Borrowed -> pure e


doCInstr :: CInstr -> M CInstr
doCInstr cinstr =
  case cinstr of
    Jump l                -> Jump <$> doJump l

    -- XXX: This is not quite right because exactly one of `l1` or `l2`
    -- (i.e., in LL terms we have (l1 + l2) situation).
    -- While we are treating this as if both will happen.

    JumpIf e l1 l2        -> JumpIf e <$> doJump l1 <*> doJump l2


    Yield                 -> pure cinstr
    ReturnNo              -> pure cinstr
    ReturnYes e           -> ReturnYes <$> copy e
    ReturnPure e          -> ReturnPure <$> copy e
    Call f c mbNo yes es  ->
      do no'  <- traverse doJump mbNo
         yes' <- doJump yes
         ms   <- lookupFunMode f
         Call f c no' yes' <$> doArgs es ms

    TailCall f c es       ->
      do ms <- lookupFunMode f
         TailCall f c <$> doArgs es ms

doJump :: JumpPoint -> M JumpPoint
doJump (JumpPoint l es) =
  do ms <- lookupLabelMode l
     JumpPoint l <$> doArgs es ms

copy :: E -> M E
copy e =
  case typeRep ty of
    NoRefs  -> pure e
    HasRefs ->
      do x <- newBV ty
         emit (Let x e)
         pure (EVar x)
  where
  ty = getType e


--------------------------------------------------------------------------------
newtype M a = M (RO -> RW -> (a,RW))

runM :: RO -> Int -> M a -> (a,Int,[Instr])
runM ro v (M m) = (a, nextName s1, reverse (revInstr s1))
  where
  (a,s1) = m ro RW { nextName = v, revInstr = [] }

data RO = RO { funMap :: Map FName [Ownership]
             , labOwn :: Map Label [Ownership]
             }

data RW = RW { nextName :: Int
             , revInstr :: [Instr]
             }

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure a = M \_ s -> (a,s)
  (<*>)  = ap

instance Monad M where
  M m >>= f = M \r s -> case m r s of
                          (a,s1) ->
                            case f a of
                              M m1 -> m1 r s1

emit :: Instr -> M ()
emit i = M \_ s -> ((), s { revInstr = i : revInstr s })

newBV :: VMT -> M BV
newBV t = M \_ s ->
  let x = nextName s
      v = BV x t
  in (v, s { nextName = x + 1 })

lookupFunMode :: FName -> M [Ownership]
lookupFunMode f = M \r s ->
  case Map.lookup f (funMap r) of
    Just ms -> (ms,s)
    Nothing -> panic "lookupFunMode" ["Missing mode for " ++ show (pp f)]

lookupLabelMode :: Label -> M [Ownership]
lookupLabelMode l = M \r s ->
  case Map.lookup l (labOwn r) of
    Just ms -> (ms,s)
    Nothing -> panic "lookupLabelMode" ["Missing signature for " ++ show (pp l)]
