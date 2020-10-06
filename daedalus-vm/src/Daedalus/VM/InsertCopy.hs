-- Assumes borrow analysis has been done
-- Insert a copy when we need to pass a value as an owned argument.
{-# Language BlockArguments #-}
module Daedalus.VM.InsertCopy (addCopyIs) where

import Control.Monad(zipWithM,ap,liftM)
import Data.Map(Map)
import qualified Data.Map as Map

import Daedalus.PP(pp)
import Daedalus.Panic(panic)

import Daedalus.Core(FName)
import Daedalus.VM
import Daedalus.VM.BorrowAnalysis
import Daedalus.VM.Backend.C.Types(Rep(..),typeRep)


addCopyIs :: Program -> Program
addCopyIs p = p { pModules = map annModule (pModules p) }
  where
  annModule m = m { mFuns = map annFun (mFuns m) }
  annFun f    = f { vmfBlocks = doBlock ro <$> vmfBlocks f }

  ro = foldr addFun emptyRO [ f | m <- pModules p, f <- mFuns m ]

  emptyRO = RO { funMap = Map.empty, labOwn = Map.empty }

  addFun f i =
    let ls = blockSig <$> vmfBlocks f
        fs = ls Map.! vmfEntry f
    in RO { funMap = Map.insert (vmfName f) fs (funMap i)
          , labOwn = Map.union ls (labOwn i)
          }

  blockSig b = map getOwnership (blockArgs b)


doBlock :: RO -> Block -> Block
doBlock ro b = b { blockLocalNum = newLocalNum
                 , blockInstrs = is
                 , blockTerm = cinstr
                 }
  where
  (cinstr,newLocalNum,is) =
     runM ro (blockLocalNum b)
     do mapM_ doInstr (blockInstrs b)
        doCInstr (blockTerm b)

-- Insert a copy instruction whenever something requires an owned argument.
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
    Let x e         -> do e1 <- copy e
                          emit (Let x e1)
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
    Unboxed -> pure e
    Boxed   ->
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
