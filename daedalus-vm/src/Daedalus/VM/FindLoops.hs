{- | A group of mutually recursive functions is considered be a loop if:
     all calls to members of the group are tail calls

Suppose that `[F,G]` is a loop, where `F_E` and `G_E` and the entry points
for the members.    We can inline a call to `F no yes` like this:

  1. Rename the blocks of `F` and `G`
  2. Change tails calls to `F` and `G` with jumps to the renamed versions
    of `F_E` and `G_E` respectively
  3. Replace `ReturnNo` with `goto no`
  3. Same for `ReturnYes a b`
-}
module Daedalus.VM.FindLoops where

import qualified Data.Set as Set

import Daedalus.Rec
import Daedalus.VM


loopAnalysis :: Module -> Module
loopAnalysis m = m { mFuns = annotateLoops (mFuns m) }

annotateLoops :: [VMFun] -> [VMFun]
annotateLoops = foldr doComp [] . topoOrder deps
  where
  deps f      = (vmfName f,funDeps f)
  funDeps f   = foldr blockDeps Set.empty (vmfBlocks f)
  blockDeps b = case blockTerm b of
                  CallPure f _ _   -> Set.insert f
                  Call f _ _ _ _   -> Set.insert f
                  TailCall f _ _   -> Set.insert f
                  _                -> id

  doComp c xs =
    case c of
      NonRec f  -> f : xs
      MutRec fs -> isLoop fs ++ xs


isLoop :: [VMFun] -> [VMFun]
isLoop xs = [ x { vmfLoop = yes } | x <- xs ]
  where
  yes = all okFun xs

  names = map vmfName xs

  okFun f = all ok (vmfBlocks f)

  ok b = case blockTerm b of
           CallPure f _ _ -> not (f `elem` names)
           Call f _ _ _ _ -> not (f `elem` names)
           _ -> True

