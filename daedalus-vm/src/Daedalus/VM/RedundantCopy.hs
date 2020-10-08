-- | This pass is not optional!
module Daedalus.VM.RedundantCopy where

import Data.Set(Set)
import qualified Data.Set as Set

import Daedalus.VM
import Daedalus.VM.BorrowAnalysis(modeI)
import Daedalus.VM.Backend.C.Types(typeRep,Rep(..))

removeRedundantCopy :: Program -> Program
removeRedundantCopy prog = prog { pBoot    = simpBlock <$> pBoot prog
                                , pModules = annModule <$> pModules prog
                                }
  where
  annModule m = m { mFuns     = annFun    <$> mFuns m }
  annFun f    = f { vmfBlocks = simpBlock <$> vmfBlocks f }


simpBlock :: Block -> Block
simpBlock b = b { blockInstrs = is, blockTerm = t }
  where
  (is,t) = check Set.empty (blockInstrs b) (blockTerm b)

check :: Set VMVar -> [Instr] -> CInstr -> ([Instr],CInstr)
check prevFree is term =
  case is of
    -- Make a copy, and immediately dallocate source
    Let x e : Free xs : more
      | Just y <- eIsVar e, y `Set.member` xs
      , let xs' = Set.delete y xs ->
        check prevFree (doSubst x y (Free xs' : more)) (doSubst x y term)

    -- Remove `free` of unboxed and merge sequential `free`
    Free xs : more -> check (Set.union xs' prevFree) more term
      where xs' = Set.filter ((== Boxed) . typeRep . getType) xs

    i : more
      | Set.null prevFree -> (                i : js, t)
      | otherwise         -> (Free prevFree : i : js, t)
      where
      more'  = map (noFree (consumes i)) more
      (js,t) = check Set.empty more' term

    [] -> ([],term)

consumes :: Instr -> Set VMVar
consumes i =
  Set.fromList [ x | (e,Owned) <- iArgs i `zip` modeI i, Just x <- [eIsVar e] ]

noFree :: Set VMVar -> Instr -> Instr
noFree xs i =
  case i of
    Free ys -> Free (Set.difference ys xs)
    _       -> i


eIsVar :: E -> Maybe VMVar
eIsVar e =
  case e of
    EVar x -> Just (LocalVar x)
    EBlockArg x -> Just (ArgVar x)
    _ -> Nothing


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
