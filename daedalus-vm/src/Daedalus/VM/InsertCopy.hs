-- | Assumes borrow analysis has been done
{-# Language BlockArguments, OverloadedStrings, RecordWildCards #-}
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
  (is,c) = doRmRedundat [] (blockInstrs b) (blockTerm b)



{-
Optimizaiton 1:
  x = copy y
  free y
  k
~>
  k [y/x]

Optimizaiton 2:
  x = copy y
  jump choice L R
     * are not both used in L
     * are not both used in R
  ~>
  jump choice L[y/x] R[y/x]  and remove "y" from free

  NOTE:  the order of the copies is arbitrary so the optimization should
  work even there are other copies between it and the jump

Example 1:

jump choice L(x) R(x)
~>
x1 = copy x
x2 = copy x
free x
jump choice L(x1,free(x2)) R(x2, free(x1))
~> [1st optimization]
x1 = copy x
jump choice L(x1,free(x)) R(x, free(x1))
~> [2nd optimization]
jump choice L(x) R(x)

Example 2:

jump choice L(x) R(x,x)
~>
x1 = copy x
x2 = copy x
x3 = copy x
free x
jump choice L(x1,free(x2,x3)) R(x2,x3, free(x1))
~> [1st optimization]
x1 = copy x
x2 = copy x
jump choice L(x1,free(x2,x)) R(x2,x, free(x1))
  * x2 and x are both in RHS so can't remove `x2 = copy x`
  * x1 and x are not in eithrt side so we can remove copy:
~>
x2 = copy
jump choice L(x,free(x2)) R(x2,x)
-}


doRmRedundat :: [Instr] -> [Instr] -> CInstr -> ([Instr],CInstr)
doRmRedundat doneIs is term =
  case is of
    -- Optimization 1: Make a copy, and immediately dallocate source
    Let x e : Free xs : more
      | Just y <- eIsVar e, y `Set.member` xs
      , let xs' = Set.delete y xs ->
        doRmRedundat doneIs (doSubst x y (Free xs' : more)) (doSubst x y term)

    Free xs : more
      | Set.null xs -> doRmRedundat doneIs more term
      | Free ys : ps <- doneIs ->
        doRmRedundat (Free (Set.union xs ys) : ps) more term

    i : more -> doRmRedundat (i : doneIs) more term

    [] -> checkTermCopies doneIs term

{- Check for Optimization 2 `x = copy y` can be eliminated if `x` and `y`
will never exist at the same time.

Assumes all "copy" was inserted by the previous pass.
In particular, we should never see:
let x = y
let z = x
-}
checkTermCopies :: [Instr] -> CInstr -> ([Instr], CInstr)
checkTermCopies is0 term0 = (reverse is1, term1)
  where
  (is1,term1) =
    case term0 of
      JumpIf e ls0 ->
        case checkIs okChoice is0 ls0 of
          (is,ls) -> (is, JumpIf e ls)

      _ -> (is0,term0)

  checkIs upd is jc =
    case is of
      i@(Let x e) : more ->
        case eIsVar e of
          Just v
            | Just jc1 <- upd x v jc -> checkIs upd more jc1
          _ -> let (is',jc1) = checkIs upd more jc
               in (i : is',jc1)

      _ -> (is, jc)

  okChoice :: BV -> VMVar -> JumpChoice -> Maybe JumpChoice
  okChoice x' v (JumpCase opts)
    | all ok optsVs = Just (JumpCase (elimJF <$> opts))
    | otherwise     = Nothing

    where
    x         = LocalVar x'

    ok vs     = not (x `Set.member` vs && v `Set.member` vs)

    optsVs = (freeVarSet . jumpTarget) <$> opts

    elimJF jf = JumpWithFree
                  { jumpTarget = doSubst x' v (jumpTarget jf)
                  , freeFirst  = Set.delete x
                               $ Set.delete v
                               $ freeFirst jf
                  }




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
      Free xs         -> Free (doFree x v xs)

-- NOTE:  we are subsitution `v` for a copy `x`.
-- we do not add `free` for copies, so no need to worry about them
-- we do not need to free the `v` because it is being passed on as
-- owned by someone else who will free it.
doFree :: BV -> VMVar -> Set VMVar -> Set VMVar
doFree x v xs = Set.delete (LocalVar x) $ Set.delete v xs

instance DoSubst CInstr where
  doSubst x v ci =
    case ci of
      Jump l          -> Jump (doSubst x v l)
      JumpIf e ls     -> JumpIf (doSubst x v e) (doSubst x v ls)
      Yield           -> Yield
      ReturnNo        -> ReturnNo
      ReturnYes e     -> ReturnYes  (doSubst x v e)
      ReturnPure e    -> ReturnPure (doSubst x v e)
      CallPure f l es -> CallPure f (doSubst x v l) (doSubst x v es)
      Call f c no yes es -> Call f c (doSubst x v no) (doSubst x v yes)
                                                      (doSubst x v es)
      TailCall f c es -> TailCall f c (doSubst x v es)

instance DoSubst JumpPoint where
  doSubst x v (JumpPoint l es) = JumpPoint l (doSubst x v es)

instance DoSubst JumpChoice where
  doSubst x v (JumpCase opts) = JumpCase (doSubst x v <$> opts)

instance DoSubst JumpWithFree where
  doSubst x v jf = JumpWithFree
                     { freeFirst = doFree x v (freeFirst jf)
                     , jumpTarget = doSubst x v (jumpTarget jf)
                     }


instance DoSubst E where
  doSubst x v e =
    case e of
      EVar y | x == y -> case v of
                           LocalVar z -> EVar z
                           ArgVar z   -> EBlockArg z
      _               -> e



--------------------------------------------------------------------------------

{- | Insert @free@ after the last use of owned variables that have references.
We do not insert @free@ instructions for copies inserted in the previous
pass as those are going to be freed by their new owners. -}
insertFree :: (Set BV, Block) -> Block
insertFree (copies,b) = b { blockInstrs = newIs, blockTerm = inTerm }
  where
  (finLive,iss) = mapAccumL updI (freeVarSet inTerm)
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


  mkFree vs = [ Free vs' | let vs'= filterFree vs, not (Set.null vs') ]

  filterFree = Set.filter \v -> getOwnership v == Owned &&
                                typeRep (getType v) == HasRefs &&
                                case v of
                                  LocalVar y -> not (y `Set.member` copies)
                                  ArgVar {}  -> True


  inTerm = case blockTerm b of
             JumpIf e ls    -> JumpIf e (freeChoice ls)
             t              -> t

  freeChoice (JumpCase opts) = JumpCase (Map.mapWithKey doCase opts)
    where
    allFree = getFree <$> opts
    doCase l it =
      let (this, rest) = doLookupRm l allFree
          others       = Set.unions (Map.elems rest)
      in changeFree (others `Set.difference` this) it

    changeFree vs x = x { freeFirst = filterFree vs }
    getFree         = freeVarSet . jumpTarget

doLookupRm :: Ord k => k -> Map k v -> (v,Map k v)
doLookupRm k mp = (v,mp1)
  where
  (Just v,mp1)  = Map.updateLookupWithKey del k mp
  del _ _       = Nothing

--------------------------------------------------------------------------------

insertCopy :: RO -> Block -> (Set BV, Block)
insertCopy ro b = ( vs
                  , b { blockLocalNum = newLocalNum
                      , blockInstrs = is
                      , blockTerm = cinstr
                      }
                  )
  where
  (cinstr,newLocalNum,vs,is) =
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
    Spawn x (JumpPoint l es) ->   -- this is a bit different because we
                                  -- are not jumping now
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
    JumpIf e ls           -> JumpIf e <$> doJumpChoice ls


    Yield                 -> pure cinstr
    ReturnNo              -> pure cinstr
    ReturnYes e           -> ReturnYes  <$> copy e
    ReturnPure e          -> ReturnPure <$> copy e

    CallPure f l es ->
      do sig <- lookupFunMode f
         es1 <- doArgs es sig
         l1  <- doJump l
         pure (CallPure f l1 es1)

    Call f c no yes es  ->
      do sig <- lookupFunMode f
         es1 <- doArgs es sig
         no1 <- doJump no
         yes1 <- doJump yes
         pure (Call f c no1 yes1 es1)

    TailCall f c es ->
      do sig <- lookupFunMode f
         es1 <- doArgs es sig
         pure (TailCall f c es1)


doJump :: JumpPoint -> M JumpPoint
doJump (JumpPoint l es) =
  do ms <- lookupLabelMode l
     JumpPoint l <$> doArgs es ms

-- | We will only take one of the two choices at run time.
-- So when we add a `copy` for the one side we add a `free` to the other.
-- A later pass removes redundant copies
doJumpChoice :: JumpChoice -> M JumpChoice
doJumpChoice (JumpCase opts) =
  do opts' <- mapM doSide opts
     let doOne l (it,_) =
          addVs (Set.unions $ map snd $ Map.elems $ Map.delete l opts') it
     pure (JumpCase (Map.mapWithKey doOne opts'))
  where
  addVs xs jf = jf { freeFirst = Set.union xs (freeFirst jf) }
  doSide jf =
    do (t,vs) <- observeCopies (doJump (jumpTarget jf))
       pure (jf { jumpTarget = t }, Set.fromList (map LocalVar vs))



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

runM :: RO -> Int -> M a -> (a,Int,Set BV,[Instr])
runM ro v (M m) = ( a
                  , nextName s1
                  , Set.fromList (concat (newVars s1))
                  , reverse (revInstr s1)
                  )
  where
  (a,s1) = m ro RW { nextName = v, newVars = [[]], revInstr = [] }

data RO = RO { funMap :: Map FName [Ownership]
             , labOwn :: Map Label [Ownership]
             }

data RW = RW { nextName :: Int
             , revInstr :: [Instr]
             , newVars  :: [[BV]]
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

observeCopies :: M a -> M (a, [BV])
observeCopies (M m) = M \r s ->
  case m r s { newVars = [] : newVars s } of
    (a,s1) -> let vs : y : xs = newVars s1
              in ((a,vs), s1 { newVars = (vs ++ y) : xs })

emit :: Instr -> M ()
emit i = M \_ s -> ((), s { revInstr = i : revInstr s })

newBV :: VMT -> M BV
newBV t = M \_ s ->
  let x = nextName s
      v = BV x t
  in (v, s { nextName = x + 1
           , newVars  = let a : as = newVars s
                        in (v:a) : as
           })

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
