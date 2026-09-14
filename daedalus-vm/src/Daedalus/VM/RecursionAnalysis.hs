module Daedalus.VM.RecursionAnalysis
  ( Frame(..)
  , RetCont(..)
  , Group
  , BlockContext(..)
  , analyzeGroup
  , groupFunsByName
  , groupFramesByLabel
  , needsWrapper
  , isGroupMember
  , canReturnFor
  , retContContinuations
  , frameVars
  , continuationFrameFields
  , frameArgCount
  , funSCCs
  ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Daedalus.Panic(panic)
import Daedalus.PP
import Daedalus.Rec(Rec,topoOrder)
import Daedalus.VM qualified as VM

-- | The strongly connected components of a module's call graph, in
-- dependency order.  A recursive component includes a singleton function
-- with a self-call.
funSCCs :: [VM.VMFun] -> [Rec VM.VMFun]
funSCCs = topoOrder deps
  where
  deps f = (VM.vmfName f, funCalls f)

-- | A continuation frame needed at a non-tail call within a recursive group.
data Frame = Frame
  { frameCaller  :: VM.FName  -- ^ Calling function.
  , frameCallee  :: VM.FName  -- ^ Called function.
  , frameRetCont :: RetCont   -- ^ How to resume when the callee returns.
  }

-- | The possible return continuations stored in a 'Frame'.
-- The arguments already present in its continuations are the values that
-- must be saved while the callee is running.
data RetCont
  = PureCont VM.JumpWithFree
  | ParserCont VM.JumpWithFree VM.JumpWithFree
    -- ^ First continuation is the failure one.

-- | Information needed to compile one mutually recursive function group.
data Group = Group
  { groupFuns          :: Map VM.FName VM.VMFun
  , groupFrames        :: Map VM.Label Frame
  , groupWrapperFuns   :: Set.Set VM.FName
    -- ^ Group members callable from outside the group.
  , groupTailReachable :: Map VM.FName (Set.Set VM.FName)
    -- ^ For each function, the group members reachable from it using zero
    -- or more tail calls.  In particular, each function reaches itself.
  }

-- | The recursive-group context for compiling one block.
data BlockContext = BlockContext
  { blockGroup    :: Group      -- ^ Analysis for the recursive group.
  , blockFunction :: VM.FName   -- ^ Function containing the block.
  , blockLabel    :: VM.Label   -- ^ Block being compiled.
  }

-- | Analyze a mutually recursive function group in the context of all
-- functions that may call it.
analyzeGroup :: [VM.VMFun] -> [VM.VMFun] -> Group
analyzeGroup allFuns fus =
  Group
    { groupFuns = funs
    , groupFrames = Map.fromList frames
    , groupWrapperFuns =
        Set.fromList
          [ VM.vmfName f
          | f <- fus
          , VM.vmfIsEntry f ||
            VM.vmfName f `Set.member` callsFromOutside
          ]
    , groupTailReachable =
        Map.fromList [ (VM.vmfName f, tailClosure (VM.vmfName f))
                     | f <- fus ]
    }
  where
  funs = Map.fromList [ (VM.vmfName f, f) | f <- fus ]
  frames = collectFrames fus
  callsFromOutside =
    Set.unions
      [ funCalls f
      | f <- allFuns
      , VM.vmfName f `Map.notMember` funs
      ]

  -- The direct tail-call edges between members of the group.
  direct =
    Map.fromList
      [ ( VM.vmfName f
        , Set.fromList
            [ callee
            | VM.VMDef body <- [VM.vmfDef f]
            , bl <- Map.elems (VM.vmfBlocks body)
            , VM.TailCall callee _ _ <- [VM.blockTerm bl]
            , callee `Map.member` funs
            ]
        )
      | f <- fus
      ]

  -- Compute the reflexive, transitive closure of tail calls from `start`.
  tailClosure start = go Set.empty (Set.singleton start)
    where
    go seen pending =
      case Set.minView pending of
        Nothing -> seen
        Just (f,rest)
          | f `Set.member` seen -> go seen rest
          | otherwise ->
              go (Set.insert f seen)
                 (Map.findWithDefault Set.empty f direct `Set.union` rest)

-- | Functions called directly by a function body.
funCalls :: VM.VMFun -> Set.Set VM.FName
funCalls fu =
  case VM.vmfDef fu of
    VM.VMExtern {} -> Set.empty
    VM.VMDef body ->
      Set.fromList
        [ callee
        | bl <- Map.elems (VM.vmfBlocks body)
        , callee <- termCall (VM.blockTerm bl)
        ]
  where
  termCall term =
    case term of
      VM.CallPure callee _ _ _        -> [callee]
      VM.CallNoCapture callee _ _ _   -> [callee]
      VM.CallCapture callee _ _ _ _   -> [callee]
      VM.TailCall callee _ _           -> [callee]
      _                                -> []

-- | Functions in the recursive group, indexed by name.
groupFunsByName :: Group -> Map VM.FName VM.VMFun
groupFunsByName = groupFuns

-- | Continuation frames in the recursive group, indexed by caller block.
groupFramesByLabel :: Group -> Map VM.Label Frame
groupFramesByLabel = groupFrames

-- | Collect the frames for non-tail calls within the group.
collectFrames :: [VM.VMFun] -> [(VM.Label, Frame)]
collectFrames fus =
  [ (VM.blockName bl, frame)
  | fu <- fus
  , VM.VMDef body <- [VM.vmfDef fu]
  , bl <- Map.elems (VM.vmfBlocks body)
  , frame <- frameForBlock names (VM.vmfName fu) bl
  ]
  where
  names = Set.fromList (map VM.vmfName fus)

-- | Construct the frame, if any, required by a block's terminator.
frameForBlock :: Set.Set VM.FName -> VM.FName -> VM.Block -> [Frame]
frameForBlock names caller bl =
  case VM.blockTerm bl of
    VM.CallPure callee ret _ _
      | callee `Set.member` names ->
          [frame callee (PureCont ret)]

    VM.CallNoCapture callee (VM.JumpCase opts) _ _
      | callee `Set.member` names ->
          case (Map.lookup False opts, Map.lookup True opts) of
            (Just no, Just yes) ->
              [frame callee (ParserCont no yes)]
            _ ->
              panic "frameForBlock"
                ["Incomplete parser return continuations in",
                 show (pp (VM.blockName bl))]

    VM.CallCapture callee _ _ _ _
      | callee `Set.member` names ->
          panic "frameForBlock"
            ["Capturing call in recursive group",
             show (pp (VM.blockName bl))]

    _ -> []
  where
  frame callee ret =
    Frame
      { frameCaller = caller
      , frameCallee = callee
      , frameRetCont = ret
      }

-- | Test if a function is a member of the recursive group.
isGroupMember :: Group -> VM.FName -> Bool
isGroupMember group f =
  f `Map.member` groupFunsByName group

-- | Test if a group member needs an ordinary externally callable wrapper.
needsWrapper :: Group -> VM.FName -> Bool
needsWrapper group f =
  f `Set.member` groupWrapperFuns group

-- | Test if a return from @actual@ may serve as a return from @expected@.
--
-- For example, suppose @F@ makes a non-tail call to @G@, and @G@ tail-calls
-- @H@.  The call to @G@ pushes a frame, but the call to @H@ does not, so when
-- @H@ returns the top frame still expects a return from @G@.  In this case
-- @canReturnFor group G H@ is true.
canReturnFor :: Group -> VM.FName -> VM.FName -> Bool
canReturnFor group expected actual =
  actual `Set.member`
    Map.findWithDefault (Set.singleton expected)
                        expected
                        (groupTailReachable group)

-- | List the jumps represented by a return continuation.
retContContinuations :: RetCont -> [VM.JumpWithFree]
retContContinuations ret =
  case ret of
    PureCont jump          -> [jump]
    ParserCont no yes      -> [no,yes]

-- | Number of values that must be saved in a frame.
frameArgCount :: Frame -> Int
frameArgCount = length . frameVars

-- | Variables stored in a frame, in field order.
--
-- Parser success and failure continuations are alternatives, and may capture
-- the same variable.  Such a variable is stored only once so that constructing
-- the frame does not try to move it more than once.  Constant expressions need
-- not be saved, as they can be reconstructed when the continuation is taken.
frameVars :: Frame -> [VM.VMVar]
frameVars =
  collect Set.empty . concatMap (VM.jArgs . VM.jumpTarget) .
  retContContinuations . frameRetCont
  where
  collect _ [] = []
  collect seen (e : es) =
    case VM.eIsVar e of
      Just var
        | var `Set.notMember` seen -> var : collect (Set.insert var seen) es
      _ -> collect seen es

-- | Frame fields used by each argument of a continuation.
--
-- A constant argument has no field, as it can be reconstructed on return.
continuationFrameFields :: Frame -> VM.JumpWithFree -> [Maybe Int]
continuationFrameFields frame ret =
  [ case VM.eIsVar value of
      Nothing -> Nothing
      Just var ->
        Just
          (Map.findWithDefault
            (panic "continuationFrameFields"
              ["Missing continuation variable"])
            var
            fields)
  | value <- VM.jArgs (VM.jumpTarget ret)
  ]
  where
  fields = Map.fromList (zip (frameVars frame) [0 ..])
