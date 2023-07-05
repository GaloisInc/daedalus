{- | A group of mutually recursive functions is considered be a loop if:
     all calls to members of the group are tail calls

Suppose that `[F,G]` is a loop, where `F_E` and `G_E` and the entry points
for the members.    We can inline a call to `F no yes` like this:

  1. Rename the blocks of `F` and `G`
  2. Change tails calls to `F` and `G` with jumps to the renamed versions
    of `F_E` and `G_E` respectively
  3. Replace `ReturnNo` with `goto no`
  3. Same for `ReturnYes a b`

XXX: it is possible to have two mutually recursive functions, `F` and `G`
where `F` will either call itself *or* it will call `G`, and `G` will call `F`.
While this might not count as a *loop* we still want to turn tail self-calls
into jump, which is currently done in TailCallJump, but should probably fit
here somewhere instead.
-}
module Daedalus.VM.FindLoops where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Daedalus.Panic
import Daedalus.Rec
import Daedalus.VM


loopAnalysis :: Module -> Module
loopAnalysis m = m { mFuns = annotateLoops (mFuns m) }

annotateLoops :: [VMFun] -> [VMFun]
annotateLoops = foldr doComp [] . topoOrder deps
  where
  deps f      = (vmfName f,funDeps f)
  funDeps f   = case vmfDef f of
                  VMExtern {} -> Set.empty
                  VMDef b     -> foldr blockDeps Set.empty (vmfBlocks b)
  blockDeps b = case blockTerm b of
                  CallPure f _ _   -> Set.insert f
                  CallCapture f _ _ _  -> Set.insert f
                  CallNoCapture f _ _  -> Set.insert f
                  TailCall f _ _   -> Set.insert f
                  _                -> id

  doComp c xs =
    case c of
      NonRec f  -> f : xs
      MutRec fs -> isLoop fs ++ xs

isLoop :: [VMFun] -> [VMFun]
isLoop xs
  | all okFun xs = makeLoop
  | otherwise = xs
  where

  names = Map.fromList [ (vmfName x, vmfEntry b) | x <- xs
                                                 , VMDef b <- [vmfDef x] ]

  okFun f = case vmfDef f of
              VMDef b -> all ok (vmfBlocks b)
              VMExtern {} -> False -- shouldn't be part of loops anyway

  ok b = case blockTerm b of
           CallPure f _ _ -> not (f `Map.member` names)
           CallCapture f _ _ _ -> not (f `Map.member` names)
           CallNoCapture f _ _ -> not (f `Map.member` names)
           _ -> True


  rewBlock b = case blockTerm b of
                 TailCall f _ es | Just l <- Map.lookup f names ->
                    b { blockTerm = Jump JumpPoint { jLabel = l, jArgs = es } }
                 _ -> b

  makeLoop =
    case xs of
      [x] -> [ case vmfDef x of
                 VMDef b -> x { vmfLoop = True
                              , vmfDef = VMDef b { vmfBlocks =
                                                    rewBlock <$> vmfBlocks b } }
                 VMExtern {} -> x
             ]
      _   -> panic "isLoop" [ "XXX: multi-function loops" ]


