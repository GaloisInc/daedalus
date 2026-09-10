{-# Language ImportQualifiedPost, OverloadedStrings #-}
module Daedalus.VM.Backend.Rust.Recursive
  ( Codegen(..)
  , GroupCodegen(..)
  , compileGroup
  , isRecFun
  , compileCall
  , compileEnter
  , compileReturnPure
  , compileReturnNo
  , compileReturnYes
  , compileThrow
  , compileException
  , compileTailCall
  ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text

import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.VM qualified as VM
import Daedalus.VM.RecursionAnalysis qualified as RA
import Daedalus.VM.Backend.Rust.Lang qualified as Rust
import Daedalus.VM.Backend.Rust.Names
import Daedalus.VM.Backend.Rust.Type(unsupported)

data Codegen = Codegen
  { compileExpr :: VM.Ownership -> VM.E -> Rust.Expr ()
  , funSigs     :: Map VM.FName [VM.Ownership]
  , isPure      :: Bool
  }

data GroupCodegen = GroupCodegen
  { groupAllFuns          :: Map VM.FName VM.VMFun
  , groupCompileBlock     ::  VM.VMFun -> RA.BlockContext -> VM.Block ->
                              (Rust.Ident, [Rust.Ty ()], Rust.Arm ())
  , groupResultType       :: VM.VMFun -> Rust.Ty ()
  , groupParserStateType  :: Rust.Ty ()
  , groupCompileVMT       ::  VM.FName -> VM.Ownership -> VM.VMT -> Rust.Ty ()
  }

-- | Compile all items needed to implement a mutually recursive group.
compileGroup :: GroupCodegen -> [VM.VMFun] -> [Rust.Item ()]
compileGroup codegen fus =
  case fus of
    [] -> panic "compileGroup" ["Empty recursive group"]
    rep : _ ->
      case wrapperFuns of
        [] -> []
        _ ->
          compileEntryType codegen rep wrapperFuns :
          compileRecFun codegen group rep fus wrapperFuns :
          map (compileWrapper codegen rep) wrapperFuns
  where
  group = RA.analyzeGroup (Map.elems (groupAllFuns codegen)) fus
  wrapperFuns =
    [ fu
    | fu <- fus
    , RA.needsWrapper group (VM.vmfName fu)
    ]

-- | Declare the type used to select a recursive group entry point.
compileEntryType ::
  GroupCodegen -> VM.VMFun -> [VM.VMFun] -> Rust.Item ()
compileEntryType codegen rep fus =
  Rust.mkEnum [] Rust.InheritedV
    (compileRecEntryTypeName (VM.vmfName rep))
    (Rust.mkGenerics [Rust.lifetimeParam entryLifetimeName]
                     Rust.noWhereClause)
    (map compileEntry fus)
  where
  entryLifetimeName = "result"
  entryLifetime = Rust.lifetime entryLifetimeName

  compileEntry fu =
    ( compileRecEntryConName (VM.vmfName fu)
    , map snd (compileEntryArgs codegen fu) ++
      [compileResultSlotType codegen entryLifetime fu]
    )

-- | Compile the shared explicit-stack worker for a recursive group.
compileRecFun ::
  GroupCodegen -> RA.Group -> VM.VMFun -> [VM.VMFun] -> [VM.VMFun] ->
  Rust.Item ()
compileRecFun codegen recCtx rep fus wrapperFuns =
  Rust.mkFnItem Nothing [] [] Rust.InheritedV
    (compileRecFunName fnm)
    generics
    [ (parserStateName, Rust.tMutRef (groupParserStateType codegen))
    , (recEntryName, entryType)
    ]
    (Rust.tTuple [])
    (Rust.block
      [ frameType
      , gotoType
      , Rust.localLetMut [] recStackName (Just stackType)
          (Rust.call
            (Rust.pathExpr (Rust.simplePath' ["Vec", "new"]))
            [])
      , pcDecl
      , mainLoop
      ])
  where
  recEntryName = "__entry"
  recResultName = "__result"
  fnm = VM.vmfName rep
  resultLifetimeName = "result"
  resultLifetime = Rust.lifetime resultLifetimeName
  generics =
    Rust.mkGenerics [Rust.lifetimeParam resultLifetimeName]
                    Rust.noWhereClause
  entryType =
    Rust.pathType
      (Rust.pathWithGen [compileRecEntryTypeName fnm]
                        [Rust.LifetimeArg resultLifetime])
  frameType =
    Rust.itemStmt
      (Rust.mkEnum [] Rust.InheritedV recFrameTypeName generics
        ( [ ( compileRecFrameConName (VM.vmfName fu)
            , [compileResultSlotType codegen resultLifetime fu]
            )
          | fu <- wrapperFuns
          ] ++
          [ ( compileRecCallFrameConName label
                                         (RA.frameCallee frame)
            , compileFrameTypes
                (groupCompileVMT codegen)
                frame
            )
          | (label,frame) <- recFrames
          ]
        ))
  gotoType =
    Rust.itemStmt
      (Rust.mkEnum [] Rust.InheritedV contTypeName Rust.noGenerics
        [ (con,tys) | (con,tys,_) <- Map.elems blockCode ])
  stackType =
    Rust.tVec
      (Rust.pathType
        (Rust.pathWithGen [recFrameTypeName]
                          [Rust.LifetimeArg resultLifetime]))
  pcDecl =
    Rust.localLetMut [] pcName Nothing
      (Rust.matchExpr
        (Rust.identExpr recEntryName)
        (map compileEntryArm wrapperFuns))
  mainLoop =
    Rust.expr
      (Rust.loopExpr (Just funLoopName)
        (Rust.block
          [ Rust.expr
              (Rust.matchExpr
                (Rust.identExpr pcName)
                [ arm | (_,_,arm) <- Map.elems blockCode ])
          ]))

  recFrames = Map.toList (RA.groupFramesByLabel recCtx)
  blockCode =
    Map.fromList
      [ ( VM.blockName bl
        , groupCompileBlock codegen fu
            RA.BlockContext
              { RA.blockGroup = recCtx
              , RA.blockFunction = VM.vmfName fu
              , RA.blockLabel = VM.blockName bl
              }
            bl
        )
      | fu <- fus
      , VM.VMDef body <- [VM.vmfDef fu]
      , bl <- Map.elems (VM.vmfBlocks body)
      ]

  compileEntryArm fu =
    case VM.vmfDef fu of
      VM.VMExtern {} ->
        panic "compileRecFun"
          ["External function in recursive group", show (pp (VM.vmfName fu))]
      VM.VMDef body ->
        case Map.lookup (VM.vmfEntry body) blockCode of
          Nothing ->
            panic "compileRecFun"
              ["Missing entry block for", show (pp (VM.vmfName fu))]
          Just (entryCon,_,_) ->
            Rust.matchArm
              (Rust.conPat
                (Rust.simplePath'
                  [ compileRecEntryTypeName fnm
                  , compileRecEntryConName (VM.vmfName fu)
                  ])
                (map (Rust.identPat . fst) args ++
                 [Rust.identPat recResultName]))
              (Rust.blockExpr
                [ Rust.expr_
                    (Rust.callMethod
                      (Rust.identExpr recStackName)
                      "push"
                      [ Rust.callCon
                          (Rust.simplePath'
                            [ recFrameTypeName
                            , compileRecFrameConName (VM.vmfName fu)
                            ])
                          [Rust.identExpr recResultName]
                      ])
                , Rust.expr
                    (Rust.callCon
                      (Rust.simplePath' [contTypeName,entryCon])
                      (map (Rust.identExpr . fst) args))
                ])
    where
    args = compileEntryArgs codegen fu

-- | Compile a wrapper preserving a recursive function's ordinary interface.
compileWrapper ::
  GroupCodegen -> VM.VMFun -> VM.VMFun -> Rust.Item ()
compileWrapper codegen rep fu =
  case VM.vmfCaptures fu of
    VM.Capture -> unsupported (fnMsg <+> "captures the stack")
    VM.Unknown ->
      panic "compileWrapper" [show (pp fnm), "`Unknown` capture"]
    VM.NoCapture ->
      Rust.mkFnItem Nothing [] [Rust.inlineAlwaysAttribute] vis
        (compileFName fnm)
        Rust.noGenerics
        ((parserStateName, Rust.tMutRef (groupParserStateType codegen)) : args)
        resT
        body
  where
  recResultName = "__result"
  fnm = VM.vmfName fu
  fnMsg = backticks (pp fnm)
  vis = if VM.vmfIsEntry fu then Rust.PublicV else Rust.InheritedV
  resT = groupResultType codegen fu
  args = compileEntryArgs codegen fu
  resultType = maybeUninitType resT

  body =
    Rust.block
      [ Rust.localLetMut [] recResultName (Just resultType)
          (Rust.call
            (Rust.pathExpr
              (Rust.simplePath' ["std", "mem", "MaybeUninit", "uninit"]))
            [])
      , Rust.expr_
          (Rust.call
            (Rust.identExpr (compileRecFunName (VM.vmfName rep)))
            [ Rust.identExpr parserStateName
            , Rust.callCon
                (Rust.simplePath'
                  [ compileRecEntryTypeName (VM.vmfName rep)
                  , compileRecEntryConName fnm
                  ])
                (map (Rust.identExpr . fst) args ++
                 [Rust.addrOfMut (Rust.identExpr recResultName)])
            ])
      , Rust.ret
          (Rust.unsafeBlockExpr
            [ Rust.expr
                (Rust.callMethod
                  (Rust.identExpr recResultName)
                  "assume_init"
                  [])
            ])
      ]

-- | Construct a Rust @MaybeUninit@ type.
maybeUninitType :: Rust.Ty () -> Rust.Ty ()
maybeUninitType ty =
  Rust.pathType (Rust.pathWithTypes ["std", "mem", "MaybeUninit"] [ty])

-- | Compile the type of an entry point's output slot.
compileResultSlotType ::
  GroupCodegen -> Rust.Lifetime () -> VM.VMFun -> Rust.Ty ()
compileResultSlotType codegen resultLifetime fu =
  Rust.tMutRefL resultLifetime
    (maybeUninitType (groupResultType codegen fu))

-- | Compile the parameters of a function's entry block.
compileEntryArgs ::
  GroupCodegen -> VM.VMFun -> [(Rust.Ident, Rust.Ty ())]
compileEntryArgs codegen fu =
  case VM.vmfDef fu of
    VM.VMExtern {} ->
      panic "compileEntryArgs"
        ["External function in recursive group", show (pp (VM.vmfName fu))]
    VM.VMDef body ->
      case Map.lookup (VM.vmfEntry body) (VM.vmfBlocks body) of
        Nothing ->
          panic "compileEntryArgs"
            ["Missing entry block for", show (pp (VM.vmfName fu))]
        Just bl ->
          [ ( Rust.mkIdent ("fa" ++ show i)
            , groupCompileVMT codegen
                (VM.vmfName fu)
                (VM.getOwnership arg)
                (VM.getType arg)
            )
          | (i,arg) <- [0 :: Int ..] `zip` VM.blockArgs bl
          ]

-- | Compile the types of values stored in a continuation frame.
-- Frame fields use the owned representation because they survive the call;
-- this is also the appropriate representation for unmanaged values.
compileFrameTypes ::
  (VM.FName -> VM.Ownership -> VM.VMT -> Rust.Ty ()) ->
  RA.Frame ->
  [Rust.Ty ()]
compileFrameTypes compileTy frame =
  [ compileTy caller VM.Owned (VM.getType var)
  | var <- RA.frameVars frame
  ]
  where
  caller = RA.frameCaller frame

-- | Test if a callee belongs to the current recursive group.
isRecFun :: RA.BlockContext -> VM.FName -> Bool
isRecFun recCtx f =
  RA.isGroupMember (RA.blockGroup recCtx) f

-- | Push a continuation frame and enter a non-tail recursive callee.
compileCall ::
  Codegen -> RA.BlockContext -> VM.FName -> [VM.E] -> [Rust.Stmt ()]
compileCall codegen recCtx callee args =
  case Map.lookup callerBlock (RA.groupFramesByLabel group) of
    Nothing ->
      panic "compileCall"
        ["Missing frame for recursive call in", show (pp callerBlock)]
    Just frame
      | RA.frameCallee frame /= callee ->
          panic "compileCall"
            ["Frame callee mismatch in", show (pp callerBlock)]
      | otherwise ->
          Rust.expr_
            (Rust.callMethod
              (Rust.identExpr recStackName)
              "push"
              [ Rust.callCon
                  (Rust.simplePath'
                    [ recFrameTypeName
                    , compileRecCallFrameConName callerBlock callee
                    ])
                  (compileFrameExprs codegen frame)
              ])
          : compileEnter codegen recCtx callee args
  where
  group = RA.blockGroup recCtx
  callerBlock = RA.blockLabel recCtx

-- | Transfer control directly to a function in the recursive group.
compileEnter ::
  Codegen -> RA.BlockContext -> VM.FName -> [VM.E] -> [Rust.Stmt ()]
compileEnter codegen recCtx callee args =
  case Map.lookup callee (RA.groupFunsByName (RA.blockGroup recCtx)) of
    Nothing ->
      panic "compileEnter"
        ["Function is not in recursive group", show (pp callee)]
    Just fu ->
      case VM.vmfDef fu of
        VM.VMExtern {} ->
          panic "compileEnter"
            ["External function in recursive group", show (pp callee)]
        VM.VMDef body ->
          [ Rust.assign
              (Rust.identExpr pcName)
              (Rust.callCon
                (Rust.simplePath'
                  [contTypeName, compileBlockLabel (VM.vmfEntry body)])
                (zipWith (compileExpr codegen) sig args))
          , Rust.continueLab funLoopName
          ]
  where
  sig =
    case Map.lookup callee (funSigs codegen) of
      Just s -> s
      Nothing ->
        panic "compileEnter"
          ["Missing ownership signature for", show (pp callee)]

-- | Compile the values captured by a continuation frame.
-- These move into the frame, so they are compiled as owned values.
compileFrameExprs :: Codegen -> RA.Frame -> [Rust.Expr ()]
compileFrameExprs codegen frame =
  [ compileExpr codegen VM.Owned (VM.eVar var)
  | var <- RA.frameVars frame
  ]

data RecReturn
  = ReturnPure (Rust.Expr ())
  | ReturnNo
  | ReturnYes (Rust.Expr ()) (Rust.Expr ())

-- | Return a value from a pure function through the explicit stack.
compileReturnPure ::
  Codegen -> RA.BlockContext -> VM.E -> [Rust.Stmt ()]
compileReturnPure codegen recCtx res =
  compileReturn codegen recCtx
    (ReturnPure (compileExpr codegen VM.Owned res))

-- | Return parser failure through the explicit stack.
compileReturnNo :: Codegen -> RA.BlockContext -> [Rust.Stmt ()]
compileReturnNo codegen recCtx =
  compileReturn codegen recCtx ReturnNo

-- | Return parser success through the explicit stack.
compileReturnYes ::
  Codegen -> RA.BlockContext -> VM.E -> VM.E -> [Rust.Stmt ()]
compileReturnYes codegen recCtx res inp =
  compileReturn codegen recCtx
    (ReturnYes
      (compileExpr codegen VM.Owned res)
      (compileExpr codegen VM.Owned inp))

-- | Pop and dispatch the continuation for a function return.
compileReturn :: Codegen -> RA.BlockContext -> RecReturn -> [Rust.Stmt ()]
compileReturn codegen recCtx ret =
  [ Rust.expr
      (Rust.matchExpr poppedFrame matchArms)
  ]
  where
  group = RA.blockGroup recCtx
  current = RA.blockFunction recCtx
  compatibleArms = doneArms ++ continuationArms

  -- Rust requires an exhaustive match, but an unnecessary wildcard is
  -- itself reported as unreachable when the concrete arms are exhaustive.
  matchArms
    | length compatibleArms == frameConstructorCount group = compatibleArms
    | otherwise = compatibleArms ++ [badArm]
  poppedFrame =
    Rust.callMethod
      (Rust.callMethod (Rust.identExpr recStackName) "pop" [])
      "unwrap"
      []

  doneArms =
    [ Rust.matchArm
        (Rust.conPat
          (Rust.simplePath'
            [recFrameTypeName, compileRecFrameConName (VM.vmfName fu)])
          [Rust.identPat recOutputName])
        (Rust.blockExpr
          [ Rust.expr_
              (Rust.callMethod
                (Rust.identExpr recOutputName)
                "write"
                [returnResult fu ret])
          , Rust.ret_
          ])
    | fu <- Map.elems (RA.groupFunsByName group)
    , RA.needsWrapper group (VM.vmfName fu)
    , RA.canReturnFor group (VM.vmfName fu) current
    ]

  continuationArms =
    [ compileReturnFrame codegen label frame ret
    | (label,frame) <- Map.toList (RA.groupFramesByLabel group)
    , RA.canReturnFor group (RA.frameCallee frame) current
    ]

  badArm =
    Rust.matchArm Rust.wildPat
      (Rust.callMacro (Rust.simplePath "unreachable") [])

-- | Number of constructors in the explicit-stack frame type.
frameConstructorCount :: RA.Group -> Int
frameConstructorCount group =
  Map.foldl'
    (\n fu ->
      if RA.needsWrapper group (VM.vmfName fu) then n + 1 else n)
    (Map.size (RA.groupFramesByLabel group))
    (RA.groupFunsByName group)

-- | Compile the return-dispatch arm for one continuation frame.
compileReturnFrame ::
  Codegen -> VM.Label -> RA.Frame -> RecReturn -> Rust.Arm ()
compileReturnFrame codegen label frame ret =
  Rust.matchArm
    (Rust.conPat
      (Rust.simplePath'
        [ recFrameTypeName
        , compileRecCallFrameConName label
                                     (RA.frameCallee frame)
        ])
      (map Rust.identPat fieldNames))
    (Rust.blockExpr (compileFrameJump target saved extra))
  where
  fieldNames =
    [ Rust.mkIdent ("_saved_" ++ show i)
    | i <- [0 .. RA.frameArgCount frame - 1]
    ]

  (target,saved,extra) =
    case (RA.frameRetCont frame,ret) of
      (RA.PureCont jump, ReturnPure value) ->
        (jump, savedFor jump, [value])

      (RA.ParserCont no _, ReturnNo) ->
        (no, savedFor no, [])

      (RA.ParserCont _ yes, ReturnYes value inp) ->
        (yes, savedFor yes, [value,inp])

      _ -> panic "compileReturnFrame" ["Return/frame kind mismatch"]

  savedFor jump =
    map compileSaved
        (zip (VM.jArgs (VM.jumpTarget jump))
             (RA.continuationFrameFields frame jump))

  compileSaved (value,mbField) =
    case mbField of
      Just i  -> Rust.identExpr (fieldNames !! i)
      Nothing -> compileExpr codegen VM.Owned value

-- | Restore a saved continuation and resume the shared worker loop.
compileFrameJump ::
  VM.JumpWithFree -> [Rust.Expr ()] -> [Rust.Expr ()] -> [Rust.Stmt ()]
compileFrameJump ret saved extra =
  case VM.jumpTarget ret of
    VM.JumpPoint label _ ->
      [ Rust.assign
          (Rust.identExpr pcName)
          (Rust.callCon
            (Rust.simplePath' [contTypeName, compileBlockLabel label])
            (extra ++ saved))
      , Rust.continueLab funLoopName
      ]

-- | Convert an internal return to the result expected by a wrapper.
returnResult :: VM.VMFun -> RecReturn -> Rust.Expr ()
returnResult fu ret =
  case ret of
    ReturnPure value
      | VM.vmfThrows fu == VM.Throws ->
          Rust.call
            (Rust.pathExpr
              (Rust.simplePath' [ddlModName, "PureResult", "Ok"]))
            [value]
      | otherwise -> value
    ReturnNo ->
      Rust.pathExpr
        (Rust.simplePath' [ddlModName, "ParserResult", "Failure"])
    ReturnYes value inp ->
      Rust.call
        (Rust.pathExpr
          (Rust.simplePath' [ddlModName, "ParserResult", "Ok"]))
        [value,inp]

recOutputName :: Rust.Ident
recOutputName = "__output"

-- | Propagate a Daedalus exception out of a recursive group.
compileThrow ::
  Codegen -> RA.BlockContext -> Text.Text -> Text.Text -> [Rust.Stmt ()]
compileThrow codegen recCtx loc msg
  | isPure codegen =
      compileException codegen recCtx
        (Rust.call
          (Rust.pathExpr
            (Rust.simplePath' [ddlModName, "PureResult", "Exception"]))
          [ Rust.litExpr (Rust.strLit (Text.unpack loc))
          , Rust.litExpr (Rust.strLit (Text.unpack msg))
          ])
  | otherwise =
      Rust.expr_
        (Rust.callMethod (Rust.identExpr parserStateName) "set_exception"
          [ Rust.litExpr (Rust.strLit (Text.unpack loc))
          , Rust.litExpr (Rust.strLit (Text.unpack msg))
          ])
      : compileException codegen recCtx parserException
  where
  parserException =
    Rust.pathExpr
      (Rust.simplePath' [ddlModName, "ParserResult", "Exception"])

-- | Write an exception to the root output slot and leave the worker.
--
-- Exceptions bypass ordinary call frames and use the bottom frame directly.
compileException ::
  Codegen -> RA.BlockContext -> Rust.Expr () -> [Rust.Stmt ()]
compileException codegen recCtx result =
  [ Rust.expr
      (Rust.matchExpr bottomFrame matchArms)
  ]
  where
  group = RA.blockGroup recCtx
  pureFun = isPure codegen
  matchArms
    | length doneArms == frameConstructorCount group = doneArms
    | otherwise = doneArms ++ [badArm]
  bottomFrame =
    Rust.callMethod
      (Rust.callMethod
        (Rust.callMethod
          (Rust.identExpr recStackName)
          "into_iter"
          [])
        "next"
        [])
      "unwrap"
      []

  doneArms =
    [ Rust.matchArm
        (Rust.conPat
          (Rust.simplePath'
            [recFrameTypeName, compileRecFrameConName (VM.vmfName fu)])
          [Rust.identPat recOutputName])
        (Rust.blockExpr
          [ Rust.expr_
              (Rust.callMethod
                (Rust.identExpr recOutputName)
                "write"
                [result])
          , Rust.ret_
          ])
    | fu <- Map.elems (RA.groupFunsByName group)
    , RA.needsWrapper group (VM.vmfName fu)
    , VM.vmfPure fu == pureFun
    , not pureFun || VM.vmfThrows fu == VM.Throws
    ]

  badArm =
    Rust.matchArm Rust.wildPat
      (Rust.callMacro (Rust.simplePath "unreachable") [])

-- | Return the result of a tail call leaving the recursive group.
compileTailCall ::
  Codegen ->
  RA.BlockContext ->
  Bool ->
  Rust.Expr () ->
  [Rust.Stmt ()]
compileTailCall codegen recCtx calleeThrows call
  | isPure codegen, calleeThrows =
      [ Rust.expr
          (Rust.matchExpr call
            [ Rust.matchArm
                (Rust.conPat
                  (Rust.simplePath' [ddlModName, "PureResult", "Ok"])
                  [Rust.identPat "x"])
                (Rust.blockExpr
                  (compileReturn codegen recCtx
                    (ReturnPure (Rust.identExpr "x"))))
            , Rust.matchArm
                (Rust.conPat
                  (Rust.simplePath'
                    [ddlModName, "PureResult", "Exception"])
                  [Rust.identPat "el", Rust.identPat "em"])
                (Rust.blockExpr
                  (compileException codegen recCtx
                    (Rust.call
                      (Rust.pathExpr
                        (Rust.simplePath'
                          [ddlModName, "PureResult", "Exception"]))
                      [Rust.identExpr "el", Rust.identExpr "em"])))
            ])
      ]
  | isPure codegen =
      compileReturn codegen recCtx (ReturnPure call)
  | otherwise =
      [ Rust.expr
          (Rust.matchExpr call
            [ Rust.matchArm
                (Rust.conPat
                  (Rust.simplePath' [ddlModName, "ParserResult", "Ok"])
                  [Rust.identPat "x", Rust.identPat "i"])
                (Rust.blockExpr
                  (compileReturn codegen recCtx
                    (ReturnYes
                      (Rust.identExpr "x")
                      (Rust.identExpr "i"))))
            , Rust.matchArm
                (Rust.conPat
                  (Rust.simplePath'
                    [ddlModName, "ParserResult", "Failure"])
                  [])
                (Rust.blockExpr (compileReturn codegen recCtx ReturnNo))
            , Rust.matchArm
                (Rust.conPat
                  (Rust.simplePath'
                    [ddlModName, "ParserResult", "Exception"])
                  [])
                (Rust.blockExpr
                  (compileException codegen recCtx parserException))
            ])
      ]
  where
  parserException =
    Rust.pathExpr
      (Rust.simplePath' [ddlModName, "ParserResult", "Exception"])
