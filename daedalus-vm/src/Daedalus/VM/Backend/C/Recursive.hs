{-# Language BlockArguments, ConstraintKinds, ImplicitParams #-}
{-# Language ImportQualifiedPost, OverloadedStrings #-}
module Daedalus.VM.Backend.C.Recursive
  ( Codegen(..)
  , GroupCodegen(..)
  , compileGroup
  , isRecFun
  , compileCallPure
  , compileCallNoCapture
  , compileReturnPure
  , compileReturnNo
  , compileReturnYes
  , compileThrow
  , compileExceptionPure
  , compileExceptionParser
  , compileTailCall
  ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Daedalus.Panic(panic)
import Daedalus.PP
import Daedalus.Core qualified as Src
import Daedalus.VM qualified as VM
import Daedalus.VM.RecursionAnalysis qualified as RA
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types

data Codegen = Codegen
  { compileExpr :: VM.E -> CExpr
  , compileFree :: Set VM.VMVar -> [CStmt]
  , compileFreeValue :: VM.VMT -> CExpr -> CStmt
  }

data GroupCodegen = GroupCodegen
  { groupAllFuns      :: Map VM.FName VM.VMFun
  , groupCompileMemo  :: VM.FName -> CDecl
  , groupParserStateType :: CType
  , groupCompileBlock ::
      Map VM.Label VM.Block ->
      VM.VMFun -> RA.BlockContext -> VM.Block -> CStmt
  }

-- | Compile a mutually recursive function group.
compileGroup :: NSUser => GroupCodegen -> [VM.VMFun] -> [CDecl]
compileGroup codegen fus =
  case fus of
    [] -> panic "compileGroup" ["Empty recursive group"]
    rep : _ ->
      case wrapperFuns of
        [] -> []
        _ ->
          map compileEntryStruct wrapperFuns ++
          [ compileEntryType rep wrapperFuns
          , compileWorker codegen group rep fus
          ] ++
          concatMap (compileWrapper codegen rep) wrapperFuns
  where
  group = RA.analyzeGroup (Map.elems (groupAllFuns codegen)) fus
  wrapperFuns =
    [ fu
    | fu <- Map.elems (RA.groupFunsByName group)
    , RA.needsWrapper group (VM.vmfName fu)
    ]

-- | Declare one alternative in the recursive worker's entry variant.
compileEntryStruct :: NSUser => VM.VMFun -> CDecl
compileEntryStruct fu =
  cStruct (cRecEntryAltName (VM.vmfName fu))
    [ cDeclareVar (cType (VM.getType arg)) (cField i)
    | (i,arg) <- zip [0 :: Int ..] (entryArgs fu)
    ]

-- | Declare the variant used to select a recursive worker entry point.
compileEntryType :: VM.VMFun -> [VM.VMFun] -> CDecl
compileEntryType rep fus =
  cUsingT (cRecEntryTypeName (VM.vmfName rep))
    (cInst "std::variant"
      [ cRecEntryAltName (VM.vmfName fu) | fu <- fus ])

-- | Compile the shared explicit-stack worker.
compileWorker ::
  NSUser =>
  GroupCodegen -> RA.Group -> VM.VMFun -> [VM.VMFun] -> CDecl
compileWorker codegen group rep fus =
  "static" <+>
  cDefineFun workerResultType (cRecWorkerName (VM.vmfName rep))
    ( [ groupParserStateType codegen <+> "&p"
      | not (VM.vmfPure rep)
      ] ++
      [ "void*" <+> recResultPtrName ] ++
      [ cPtrT (cSemType Src.TStream) <+> recInputPtrName
      | not (VM.vmfPure rep)
      ] ++
      [ cRecEntryTypeName (VM.vmfName rep) <+> recEntryName ]
    )
    body
  where
  workerResultType
    | VM.vmfPure rep = "void"
    | otherwise      = "DDL::ParserResult"

  blocks =
    Map.fromList
      [ (VM.blockName bb, bb)
      | fu <- fus
      , VM.VMDef def <- [VM.vmfDef fu]
      , bb <- Map.elems (VM.vmfBlocks def)
      ]

  frames = Map.toList (RA.groupFramesByLabel group)
  entryFuns =
    [ fu
    | fu <- Map.elems (RA.groupFunsByName group)
    , RA.needsWrapper group (VM.vmfName fu)
    ]
  frameTypes =
    [ cRecCallFrameName label (RA.frameCallee frame)
    | (label,frame) <- frames
    ]

  body =
    frameDecls ++
    map cDeclareBlockParams (Map.elems blocks) ++
    [ compileEntrySwitch blocks entryFuns ] ++
    [ groupCompileBlock codegen blocks fu
        RA.BlockContext
          { RA.blockGroup = group
          , RA.blockFunction = VM.vmfName fu
          , RA.blockLabel = VM.blockName bb
          }
        bb
    | fu <- fus
    , VM.VMDef def <- [VM.vmfDef fu]
    , bb <- Map.elems (VM.vmfBlocks def)
    ]

  frameDecls
    | null frames = []
    | otherwise =
        map (uncurry compileCallFrame) frames ++
        [ cUsingT cRecFrameTypeName (cInst "std::variant" frameTypes)
        , cDeclareVar (cInst "std::vector" [cRecFrameTypeName]) cRecStackName
        ]

-- | Generate a frame for one non-tail call within the recursive group.
compileCallFrame :: NSUser => VM.Label -> RA.Frame -> CDecl
compileCallFrame label frame =
  cStruct (cRecCallFrameName label (RA.frameCallee frame))
    [ cDeclareVar (cType (VM.getType var)) (cField i)
    | (i,var) <- zip [0 :: Int ..] (RA.frameVars frame)
    ]

-- | Select the initial function and enter its CFG.
compileEntrySwitch ::
  NSUser => Map VM.Label VM.Block -> [VM.VMFun] -> CStmt
compileEntrySwitch blocks fus =
  cSwitch (cCallMethod recEntryName "index" [])
    (zipWith compileEntry [0 :: Int ..] fus ++ [cDefault cUnreachable])
  where
  compileEntry index fu =
    case VM.vmfDef fu of
      VM.VMExtern {} ->
        panic "compileEntrySwitch"
          ["External function in recursive group", show (pp (VM.vmfName fu))]
      VM.VMDef def ->
        case Map.lookup (VM.vmfEntry def) blocks of
          Nothing ->
            panic "compileEntrySwitch"
              ["Missing entry block for", show (pp (VM.vmfName fu))]
          Just bb ->
            cCaseBlock (int index)
              ( [ cDeclareInitVar "auto&" recEntryAltName
                    (variantGet entryAlt recEntryName)
                ] ++
                [ cAssign (cArgUse bb arg)
                    (cSelect recEntryAltName (cField i))
                | (i,arg) <- zip [0 :: Int ..] (VM.blockArgs bb)
                ] ++
                [ cGoto (cBlockLabel (VM.vmfEntry def)) ]
              )
        where
        entryAlt = cRecEntryAltName (VM.vmfName fu)

-- | Generate an ordinary function wrapper for a recursive group member.
compileWrapper ::
  NSUser => GroupCodegen -> VM.VMFun -> VM.VMFun -> [CDecl]
compileWrapper codegen rep fu
  | VM.vmfPure fu
  , VM.vmfThrows fu == VM.NoThrows
  , null normalArgs =
      [ "static inline" $$ wrapperDef cFNameInit
      , groupCompileMemo codegen (VM.vmfName fu)
      ]
  | otherwise = [wrapperDef cFName]
  where
  fnm = VM.vmfName fu
  resultType = functionResultType fu
  semResultType = cSemType (Src.fnameType fnm)
  normalArgs =
    [ cType (VM.getType arg) <+> argName i
    | (i,arg) <- zip [1 :: Int ..] (entryArgs fu)
    ]

  wrapperDef name =
    cDefineFun wrapperResultType (name fnm) wrapperArgs wrapperBody

  wrapperResultType
    | VM.vmfPure fu = functionResultType fu
    | otherwise     = "DDL::ParserResult"

  wrapperArgs
    | VM.vmfPure fu = normalArgs
    | otherwise =
        [ groupParserStateType codegen <+> "&p"
        , cPtrT semResultType <+> recResultPtrName
        , cPtrT (cSemType Src.TStream) <+> recInputPtrName
        ] ++ normalArgs

  wrapperBody
    | VM.vmfPure fu =
        [ cDeclareVar resultType recResultName
        , cStmt (callWorker ["&" <.> recResultName])
        , cReturn recResultName
        ]
    | otherwise =
        [ cReturn
            (callWorker
              [ recResultPtrName
              , recInputPtrName
              ])
        ]

  callWorker resultArgs =
    cCall (cRecWorkerName (VM.vmfName rep))
      ( [ "p" | not (VM.vmfPure fu) ] ++
        resultArgs ++
        [ cCallCon (cRecEntryAltName fnm)
            [ argName i | i <- [1 :: Int .. length normalArgs] ]
        ]
      )

-- | Test if a callee belongs to the current recursive group.
isRecFun :: RA.BlockContext -> VM.FName -> Bool
isRecFun recCtx f =
  RA.isGroupMember (RA.blockGroup recCtx) f

-- | Push a continuation frame and enter a recursive pure callee.
compileCallPure ::
  NSUser =>
  Codegen -> RA.BlockContext -> VM.FName -> VM.JumpWithFree -> [VM.E] ->
  Set VM.VMVar -> [CStmt]
compileCallPure codegen recCtx callee ret args exnFree =
  compileCall codegen recCtx callee args
    (VM.freeFirst ret `Set.union` exnFree)

-- | Push a continuation frame and enter a recursive parser.
compileCallNoCapture ::
  NSUser =>
  Codegen -> RA.BlockContext -> VM.FName ->
  VM.JumpChoice Bool -> [VM.E] -> Set VM.VMVar -> [CStmt]
compileCallNoCapture codegen recCtx callee rets args exnFree =
  compileCall codegen recCtx callee args freeCandidates
  where
  freeCandidates =
    case rets of
      VM.JumpCase opts ->
        case (Map.lookup False opts, Map.lookup True opts) of
          (Just no, Just yes) ->
            VM.freeFirst no `Set.union`
            VM.freeFirst yes `Set.union`
            exnFree
          _ -> exnFree

compileCall ::
  NSUser =>
  Codegen -> RA.BlockContext -> VM.FName -> [VM.E] -> Set VM.VMVar -> [CStmt]
compileCall codegen recCtx callee args freeCandidates =
  case Map.lookup callerBlock (RA.groupFramesByLabel group) of
    Nothing ->
      panic "compileCall"
        ["Missing frame for recursive call in", show (pp callerBlock)]
    Just frame
      | RA.frameCallee frame /= callee ->
          panic "compileCall"
            ["Frame callee mismatch in", show (pp callerBlock)]
      | otherwise ->
          let saved = Set.fromList (RA.frameVars frame)
              freeBeforeCall =
                compileFree codegen (freeCandidates `Set.difference` saved)
          in
          cStmt
            (cCallMethod cRecStackName "push_back"
              [ cCallCon
                  (cRecCallFrameName callerBlock callee)
                  [ compileExpr codegen (VM.eVar var)
                  | var <- RA.frameVars frame
                  ]
              ])
          : compileEnterWith codegen recCtx callee args freeBeforeCall
  where
  group = RA.blockGroup recCtx
  callerBlock = RA.blockLabel recCtx

-- | Compile a tail call, entering an internal callee directly or converting
-- an external call's result into a return through the explicit stack.
compileTailCall ::
  NSUser => Codegen -> RA.BlockContext -> VM.VMFun -> [VM.E] -> [CStmt]
compileTailCall codegen recCtx callee args
  | isRecFun recCtx fname = compileEnter codegen recCtx fname args
  | VM.vmfPure callee =
      if VM.vmfThrows callee == VM.Throws
        then
          [ cDeclareInitVar (functionResultType callee) recCallResultName call
          , cIf
              (cCallMethod recCallResultName "isOk" [])
              (compileReturn codegen recCtx
                (ReturnPure
                  (cCallMethod recCallResultName "getValue" [])))
              (compileExceptionPure codegen recCtx
                (cCallMethod recCallResultName "getException" []))
          ]
        else compileReturn codegen recCtx (ReturnPure call)
  | otherwise =
      [ cDeclareVar semResultType recCallValueName
      , cDeclareVar (cSemType Src.TStream) recCallInputName
      , cDeclareInitVar "DDL::ParserResult" recCallResultName
          (cCall (cFName fname)
            ( [ "p"
              , "&" <.> recCallValueName
              , "&" <.> recCallInputName
              ] ++ map (compileExpr codegen) args
            ))
      , cSwitch recCallResultName
          [ cCaseBlock "DDL::ParserResult::Ok"
              (compileReturn codegen recCtx
                (ReturnYes recCallValueName recCallInputName))
          , cCaseBlock "DDL::ParserResult::Failure"
              (compileReturn codegen recCtx ReturnNo)
          , cCaseBlock "DDL::ParserResult::Exception"
              (compileExceptionParser codegen recCtx)
          ]
      ]
  where
  fname = VM.vmfName callee
  semResultType = cSemType (Src.fnameType fname)
  call = cCall (cFName fname) (map (compileExpr codegen) args)

-- | Transfer control directly to a function in the recursive group.
compileEnter ::
  NSUser => Codegen -> RA.BlockContext -> VM.FName -> [VM.E] -> [CStmt]
compileEnter codegen recCtx callee args =
  compileEnterWith codegen recCtx callee args []

-- | Assign entry arguments, perform any caller cleanup, and enter a function.
-- Cleanup follows argument assignment so call arguments remain live while
-- their values are transferred into the callee's entry block.
compileEnterWith ::
  NSUser =>
  Codegen -> RA.BlockContext -> VM.FName -> [VM.E] -> [CStmt] -> [CStmt]
compileEnterWith codegen recCtx callee args beforeJump =
  case Map.lookup callee (RA.groupFunsByName (RA.blockGroup recCtx)) of
    Nothing ->
      panic "compileEnter"
        ["Function is not in recursive group", show (pp callee)]
    Just fu ->
      case VM.vmfDef fu of
        VM.VMExtern {} ->
          panic "compileEnter"
            ["External function in recursive group", show (pp callee)]
        VM.VMDef def ->
          case Map.lookup (VM.vmfEntry def) (VM.vmfBlocks def) of
            Nothing ->
              panic "compileEnter"
                ["Missing entry block for", show (pp callee)]
            Just bb ->
              [ cAssign (cArgUse bb arg) (compileExpr codegen value)
              | (arg,value) <- zip (VM.blockArgs bb) args
              ] ++ beforeJump ++
              [cGoto (cBlockLabel (VM.vmfEntry def))]

data RecReturn
  = ReturnPure CExpr
  | ReturnNo
  | ReturnYes CExpr CExpr

-- | Return a pure value through the explicit stack.
compileReturnPure ::
  NSUser => Codegen -> RA.BlockContext -> VM.E -> [CStmt]
compileReturnPure codegen recCtx result =
  compileReturn codegen recCtx (ReturnPure (compileExpr codegen result))

-- | Return parser failure through the explicit stack.
compileReturnNo ::
  NSUser => Codegen -> RA.BlockContext -> [CStmt]
compileReturnNo codegen recCtx =
  compileReturn codegen recCtx ReturnNo

-- | Return parser success through the explicit stack.
compileReturnYes ::
  NSUser => Codegen -> RA.BlockContext -> VM.E -> VM.E -> [CStmt]
compileReturnYes codegen recCtx result input =
  compileReturn codegen recCtx
    (ReturnYes (compileExpr codegen result) (compileExpr codegen input))

-- | Complete the entry directly, or pop and dispatch a continuation frame.
compileReturn ::
  NSUser => Codegen -> RA.BlockContext -> RecReturn -> [CStmt]
compileReturn codegen recCtx ret =
  if null calls
    then completeEntry
    else
      [ cIf
          (cCallMethod cRecStackName "empty" [])
          completeEntry
          [ cDeclareInitVar cRecFrameTypeName cRecCurrentFrameName
              (cCall "std::move" [cCallMethod cRecStackName "back" []])
          , cStmt (cCallMethod cRecStackName "pop_back" [])
          , cSwitch (cCallMethod cRecCurrentFrameName "index" [])
              (callCases ++ [cDefault cUnreachable])
          ]
      ]
  where
  group = RA.blockGroup recCtx
  current = RA.blockFunction recCtx
  currentFun = lookupGroupFun recCtx current

  callCases =
    [ compileReturnFrame index label frame
    | (index,(label,frame)) <- zip [0 :: Int ..] calls
    , RA.canReturnFor group (RA.frameCallee frame) current
    ]

  completeEntry = compileEntryReturn currentFun ret
  calls = Map.toList (RA.groupFramesByLabel group)

  compileReturnFrame index label frame =
    case (RA.frameRetCont frame,ret) of
      (RA.PureCont jump, ReturnPure value) ->
        compileFrameJump index frame jump [value]
      (RA.ParserCont no _, ReturnNo) ->
        compileFrameJump index frame no []
      (RA.ParserCont _ yes, ReturnYes value input) ->
        compileFrameJump index frame yes [value,input]
      _ ->
        cCaseBlock (int index) [cUnreachable]
    where
    alt = cRecCallFrameName label (RA.frameCallee frame)

    compileFrameJump index' frame' jump extra =
      let bb = lookupBlock group (VM.jLabel (VM.jumpTarget jump))
          used = Set.fromList
                   [ i
                   | Just i <- RA.continuationFrameFields frame' jump
                   ]
          freeUnused =
            [ compileFreeValue codegen (VM.getType var)
                (cSelect recSavedFrameName (cField i))
            | (i,var) <- zip [0 :: Int ..] (RA.frameVars frame')
            , i `Set.notMember` used
            ]
      in
      cCaseBlock (int index')
        ( [ cDeclareInitVar "auto&" recSavedFrameName
              (variantGet alt cRecCurrentFrameName)
          ] ++
          zipWith
            (\arg value -> cAssign (cArgUse bb arg) value)
            (VM.blockArgs bb)
            (extra ++ savedArgs frame' jump) ++
          freeUnused ++
          [cGoto (cBlockLabel (VM.blockName bb))]
        )

  savedArgs frame jump =
    [ case mbField of
        Just i  -> cSelect recSavedFrameName (cField i)
        Nothing -> compileExpr codegen value
    | (value,mbField) <-
        zip (VM.jArgs (VM.jumpTarget jump))
            (RA.continuationFrameFields frame jump)
    ]

compileEntryReturn :: NSUser => VM.VMFun -> RecReturn -> [CStmt]
compileEntryReturn fu ret =
  case ret of
    ReturnPure value
      | VM.vmfPure fu ->
          [ cAssign
              (resultPointer (functionResultType fu))
              (if VM.vmfThrows fu == VM.Throws
                 then cCall (functionResultType fu <.> "::ok") [value]
                 else value)
          , cStmt "return"
          ]
    ReturnNo
      | not (VM.vmfPure fu) ->
          [ cReturn "DDL::ParserResult::Failure" ]
    ReturnYes value input
      | not (VM.vmfPure fu) ->
          [ cAssign
              (resultPointer
                (cSemType (Src.fnameType (VM.vmfName fu))))
              value
          , cAssign
              ("*" <.> recInputPtrName)
              input
          , cReturn "DDL::ParserResult::Ok"
          ]
    _ -> [cUnreachable]

-- | Throw an exception from a recursive function.
compileThrow ::
  NSUser =>
  Codegen -> RA.BlockContext -> CExpr -> CExpr -> [CStmt]
compileThrow codegen recCtx loc msg
  | currentIsPure recCtx =
      compileExceptionPure codegen recCtx
        (cCallCon "DDL::Exception" [loc,msg])
  | otherwise =
      cStmt (cCall "p.setException" [loc,msg]) :
      compileExceptionParser codegen recCtx

-- | Propagate an exception value from a pure call.
compileExceptionPure ::
  NSUser => Codegen -> RA.BlockContext -> CExpr -> [CStmt]
compileExceptionPure codegen recCtx exception =
  if currentIsPure recCtx
    then compileException codegen recCtx (Just exception)
    else
      cStmt
        (cCall "p.setException"
          [ cCallMethod exception "getLocation" []
          , cCallMethod exception "getMessage" []
          ]) :
      compileException codegen recCtx Nothing

-- | Propagate an exception already recorded in the parser state.
compileExceptionParser ::
  NSUser => Codegen -> RA.BlockContext -> [CStmt]
compileExceptionParser codegen recCtx =
  compileException codegen recCtx Nothing

-- | Discard all continuation frames and complete the original entry.
compileException ::
  NSUser => Codegen -> RA.BlockContext -> Maybe CExpr -> [CStmt]
compileException codegen recCtx exception =
  cleanupStack ++
  completeEntry
  where
  group = RA.blockGroup recCtx
  entries =
    [ fu
    | fu <- Map.elems (RA.groupFunsByName group)
    , RA.needsWrapper group (VM.vmfName fu)
    ]
  calls = Map.toList (RA.groupFramesByLabel group)

  cleanupCases =
    [ cCaseBlock (int index)
        ( cDeclareInitVar "auto&" recSavedFrameName
            (variantGet alt recDiscardedFrameName) :
          [ compileFreeValue codegen (VM.getType var)
              (cSelect recSavedFrameName (cField i))
          | (i,var) <- zip [0 :: Int ..] (RA.frameVars frame)
          ] ++
          [cBreak]
        )
    | (index,(label,frame)) <- zip [0 :: Int ..] calls
    , let alt = cRecCallFrameName label (RA.frameCallee frame)
    ]

  cleanupStack
    | null calls = []
    | otherwise =
        [ cForRange "auto&" recDiscardedFrameName cRecStackName
            (cBlock
              [ cSwitch (cCallMethod recDiscardedFrameName "index" [])
                  (cleanupCases ++ [cDefault cUnreachable])
              ])
        ]

  exceptionCases =
    [ cCaseBlock (int index)
        (compileEntryException fu ++ [cStmt "return"])
    | (index,fu) <- zip [0 :: Int ..] entries
    ]

  completeEntry
    | currentIsPure recCtx =
        [ cSwitch (cCallMethod recEntryName "index" [])
            (exceptionCases ++ [cDefault cUnreachable])
        ]
    | otherwise =
        [ cReturn "DDL::ParserResult::Exception" ]

  compileEntryException fu
    = case exception of
        Just value ->
          [ cAssign
              (resultPointer (functionResultType fu))
              (cCall (functionResultType fu <.> "::failure") [value])
          ]
        Nothing -> [cUnreachable]

currentIsPure :: RA.BlockContext -> Bool
currentIsPure recCtx = VM.vmfPure (lookupGroupFun recCtx (RA.blockFunction recCtx))

lookupGroupFun :: RA.BlockContext -> VM.FName -> VM.VMFun
lookupGroupFun recCtx fname =
  case Map.lookup fname
                  (RA.groupFunsByName (RA.blockGroup recCtx)) of
    Just fu -> fu
    Nothing ->
      panic "lookupGroupFun"
        ["Function is not in recursive group",
         show (pp fname)]

-- | Look up a block belonging to a recursive group.
lookupBlock :: RA.Group -> VM.Label -> VM.Block
lookupBlock group label =
  case [ bb
       | fu <- Map.elems (RA.groupFunsByName group)
       , VM.VMDef def <- [VM.vmfDef fu]
       , Just bb <- [Map.lookup label (VM.vmfBlocks def)]
       ] of
    bb : _ -> bb
    [] -> panic "lookupBlock" ["Missing block", show (pp label)]

-- | Parameters of a function's entry block.
entryArgs :: VM.VMFun -> [VM.BA]
entryArgs fu =
  case VM.vmfDef fu of
    VM.VMExtern {} ->
      panic "entryArgs"
        ["External function in recursive group", show (pp (VM.vmfName fu))]
    VM.VMDef def ->
      case Map.lookup (VM.vmfEntry def) (VM.vmfBlocks def) of
        Just bb -> VM.blockArgs bb
        Nothing ->
          panic "entryArgs"
            ["Missing entry block for", show (pp (VM.vmfName fu))]

cDeclareBlockParams :: NSUser => VM.Block -> CStmt
cDeclareBlockParams bb
  | null params = empty
  | otherwise =
      vcat
        ( ("\n// Parameters for" <+> pp (VM.blockName bb)) :
          params
        )
  where
  params =
    [ cDeclareVar (cType (VM.getType arg)) (cArgUse bb arg)
    | arg <- VM.blockArgs bb
    ]

resultPointer :: CType -> CExpr
resultPointer ty =
  "*" <.> cCall (cInst "static_cast" [cPtrT ty]) [recResultPtrName]

functionResultType :: NSUser => VM.VMFun -> CType
functionResultType fu
  | VM.vmfPure fu, VM.vmfThrows fu == VM.Throws =
      cInst "DDL::Result" [semResultType]
  | otherwise = semResultType
  where
  semResultType = cSemType (Src.fnameType (VM.vmfName fu))

cStruct :: CIdent -> [CDecl] -> CDecl
cStruct name fields =
  cStmt
    (vcat
      [ "struct" <+> name <+> "{"
      , nest 2 (vcat fields)
      , "}"
      ])

variantGet :: CType -> CExpr -> CExpr
variantGet ty value =
  cCall (cInst "std::get" [ty]) [value]

argName :: Int -> CIdent
argName i = "a" <.> int i

recEntryName :: CIdent
recEntryName = "rec_entry"

recEntryAltName :: CIdent
recEntryAltName = "rec_entry_alt"

recResultName :: CIdent
recResultName = "rec_result"

recResultPtrName :: CIdent
recResultPtrName = "result"

recInputPtrName :: CIdent
recInputPtrName = "result_input"

recCallResultName :: CIdent
recCallResultName = "rec_call_result"

recCallValueName :: CIdent
recCallValueName = "rec_call_value"

recCallInputName :: CIdent
recCallInputName = "rec_call_input"

recDiscardedFrameName :: CIdent
recDiscardedFrameName = "discarded_frame"

recSavedFrameName :: CIdent
recSavedFrameName = "saved"
