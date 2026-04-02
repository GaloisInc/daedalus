{-# Language ImportQualifiedPost, OverloadedStrings, ImplicitParams, ConstraintKinds #-}
module Daedalus.VM.Backend.Rust (
  compileProgram,
  Config(..)
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.List(foldl')
import Data.ByteString qualified as BS

import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.Core qualified as Core
import Daedalus.VM qualified as VM
import Daedalus.VM.Backend.Rust.Lang qualified as Rust
import Daedalus.VM.Backend.Rust.Names
import Daedalus.VM.BorrowAnalysis
import Daedalus.VM.Backend.Rust.Type

newtype Config = Config {
  cfgUserModule :: String
}

type ProgCtx = (
  ?funSigs :: Map VM.FName [VM.Ownership],
  ?blockSigs :: Map VM.Label [VM.Ownership]
  )

type FnCtx = (ProgCtx, ?isPure :: Bool, ?fnMsg :: Doc)

compileProgram :: Config -> VM.Program -> (String, [Doc])
compileProgram cfg vm = (show (Rust.pretty' result), []) -- XXX
  where
  result :: Rust.SourceFile ()
  result =
    let ?funSigs = funSigs
        ?blockSigs = blockSigs
    in
    Rust.SourceFile Nothing [] (uses ++ concatMap compileModule (VM.pModules vm))

  unusedOk = [Rust.disableWarning "unused_imports"]
  uses = [
    Rust.use' unusedOk (Rust.useOne (Rust.simplePath "daedalus_rts_rust") (Just "ddl")),
    Rust.use' unusedOk (Rust.useSelect (Rust.simplePath "ddl") [ Rust.useOne x Nothing | x <- map Rust.simplePath [ "Type", "Clo" ] ]),
    Rust.use' unusedOk (Rust.useOne (Rust.simplePath "serde") Nothing) ]
  (funSigs,blockSigs) = foldl' sigsOfMod (mempty,mempty) (VM.pModules vm)

  sigsOfMod s m = foldl' sigsOfFun s (VM.mFuns m)

  sigsOfFun (fs,bs) f =
    case VM.vmfDef f of
      VM.VMExtern ba -> (Map.insert (VM.vmfName f) (getSig ba) fs, bs)
      VM.VMDef body -> (fs1, bs1)
        where
        bs1 = foldl' sigsOfBlock bs (Map.elems (VM.vmfBlocks body))
        fs1 =
          case Map.lookup (VM.vmfEntry body) bs1 of
            Just sig -> Map.insert (VM.vmfName f) sig fs
            Nothing ->
              panic "compileProgram"
                ["Missing entry block for", show (pp (VM.vmfName f))]
        
  sigsOfBlock bs b  = Map.insert (VM.blockName b) (getSig (VM.blockArgs b)) bs
  getSig            = map VM.getOwnership


compileModule :: ProgCtx => VM.Module -> [Rust.Item ()]
compileModule m = concatMap compileUserType (VM.mTypes m) ++
                  map compileFun (VM.mFuns m)
  
compileFun :: ProgCtx => VM.VMFun -> Rust.Item ()
compileFun fu =
  case VM.vmfCaptures fu of
    VM.Capture   -> unsupported (fnMsg <+> "captures the stack")
    VM.Unknown   -> panic "compileFun" [ show (pp fnm), "`Unknwon` capture" ]
    VM.NoCapture ->
      Rust.mkFnItem Nothing [] [] vis nm Rust.noGenerics args resT def
  where
  vis             = if VM.vmfIsEntry fu then Rust.PublicV else Rust.InheritedV
  fnMsg           = backticks (pp fnm)
  fnm             = VM.vmfName fu
  nm              = compileFName fnm
  resT
    | VM.vmfPure fu = compileType VM.Owned (Core.fnameType fnm)
    | otherwise     = Rust.tOption (Rust.tTuple [compileType VM.Owned (Core.fnameType fnm), compileType VM.Owned Core.TStream])
    where ?fnMsg = fnMsg

  (args,def) =
    let ?isPure = VM.vmfPure fu
        ?fnMsg  = fnMsg
    in compileFunDef (VM.vmfDef fu)



compileFunDef :: FnCtx => VM.VMFDef -> ([(Rust.Ident,Rust.Ty ())], Rust.Block ())
compileFunDef def =
  case def of
    VM.VMExtern _ -> unsupported (?fnMsg <+> "is an externally defined function")
    VM.VMDef body -> compileFunBody body


compileFunBody ::
  FnCtx => VM.VMFBody -> ([(Rust.Ident,Rust.Ty ())], Rust.Block ())
compileFunBody body = (args, Rust.block [contT, pcDecl, mainLoop])
  where
  blocks    = VM.vmfBlocks body
  entry     = VM.vmfEntry body

  contT     = Rust.itemStmt (Rust.mkEnum [] Rust.InheritedV contTypeName Rust.noGenerics
                [ (c,ts) | (c,ts,_) <- Map.elems blockCode ])

  (args, pcDecl) =
    case Map.lookup entry blockCode of
      Just (l,ts,_) ->
        let argName i = Rust.mkIdent ("fa" ++ show i)
            as        = [ (argName i, t) | (i,t) <- [0 :: Int ..] `zip` ts ]
            start     = Rust.call (Rust.pathExpr (Rust.simplePath' [contTypeName,l])) (map (Rust.identExpr . fst) as)
        in (as, Rust.localLetMut ["unused_mut"] pcName Nothing start)
      _ -> panic "compileFunBody" ["Mssing entry block code"]

  mainLoop =
    Rust.expr (
      Rust.loopExpr (Rust.block [
        Rust.expr (Rust.matchExpr
                    (Rust.pathExpr (Rust.simplePath pcName))
                    [ arm | (_,_,arm) <- Map.elems blockCode ])
      ]))

  blockCode = compileBlock <$> blocks


compileBlock :: FnCtx => VM.Block -> (Rust.Ident, [Rust.Ty ()], Rust.Arm ())
compileBlock bl = (lab, argTs, alt)
  where
  args      = VM.blockArgs bl
  argNames  = map compileBAName args
  argTs     = [ compileVMT (VM.getType ba) (VM.getOwnership ba) | ba <- args ]
  lab       = compileBlockLabel (VM.blockName bl)
  alt       = Rust.matchArm
                (Rust.conPat (Rust.simplePath' [contTypeName,lab]) (map Rust.identPat argNames))
                (Rust.blockExpr code)
  code = concatMap compileBlockInstr (VM.blockInstrs bl) ++
         compileCInstr (VM.blockTerm bl)

compileBlockInstr :: FnCtx => VM.Instr -> [Rust.Stmt ()]
compileBlockInstr instr =
  case instr of
    VM.Say str          -> notYet
    VM.Output {}        -> bad
    VM.Notify {}        -> bad
    VM.CallPrim x f es  -> compilePrim x f es
    VM.Spawn {}         -> bad
    VM.Let x e ->
      [ Rust.localLet [] (compileBVName x) Nothing (Rust.callMethod (compileExpr VM.Borrowed e) "clo" [])]
    VM.Free _                   -> [] -- Rust should drop things on its own
    VM.NoteFail loc str inp msg -> [] -- XXX: error messages
    VM.PushDebug {}             -> [] -- XXX: stack trace
    VM.PopDebug                 -> [] -- XXX
  where
  bad     = panic "compileBlockInstr" ["Unexpected instruction", show (pp instr)]
  notYet  = unsupported (?fnMsg <+> "instruction:" <+> pp instr)


callRTS :: Rust.Ident -> [Rust.Expr ()] -> Rust.Expr ()
callRTS f = Rust.call (Rust.pathExpr (ddlPath f))


compilePrim :: FnCtx => VM.BV -> VM.PrimName -> [VM.E] -> [Rust.Stmt ()]
compilePrim x prim es =
  case prim of
    VM.ByteArray bs ->
      def ty (callRTS "new_array_slice" [Rust.litExpr (Rust.bytesLit bs)])
        where ty = if BS.null bs then Just (compileType VM.Owned (Core.TArray Core.TByte)) else Nothing

    VM.NewBuilder ty -> 
      def (Just (compileType VM.Owned (Core.TBuilder ty))) (callRTS "new_builder" [])

    VM.Integer i -> xxx

    VM.StructCon ut ->
      case Core.tnameFlav nm of
        Core.TFlavStruct ls ->
          def Nothing (Rust.struct (Rust.simplePath (compileTName False nm))
              [ (compileFieldLabel l, e) | (l,e) <- zip ls compiled ])
        _ -> panic "compilePrim" ["StructCon bad flavor"]
      
      where nm = Core.utName ut
    
    VM.Op1 op ->
      case (compiled,es) of
        ([e1],[e]) -> compileOp1 x op e1 (VM.getType e)
        _          -> bad 1

    VM.Op2 op ->
      case compiled of
        [e1,e2] -> compileOp2 x op e1 e2
        _       -> bad 2

    VM.Op3 op ->
      case compiled of
        [e1,e2,e3] -> compileOp3 x op e1 e2 e3
        _          -> bad 3

    VM.OpN op -> compileOpN x op compiled
  
  where
  compiled = zipWith compileExpr (modePrimName prim) es
  bad n = panic "compilePrim" ["Expected " ++ show (n::Int) ++ "argument, but have " ++ show (length es), show (pp prim) ]
  def mb re = [Rust.localLet [] (compileBVName x) mb re]
  tmp = show (pp prim <> parens (commaSep (map pp es)))
  xxx =
    [ Rust.localLet [] (compileBVName x) (Just (compileVMT (VM.getType x) VM.Owned))
      (Rust.callMacro (Rust.simplePath "todo") [Rust.litExpr (Rust.strLit tmp)])
    ]

compileOp1 :: FnCtx => VM.BV -> Core.Op1 -> Rust.Expr () -> VM.VMT -> [Rust.Stmt ()]
compileOp1 x op e argTy =
  case op of
    -- Streams
    Core.Head -> def (Rust.callMethod e "head" [])
    Core.IsEmptyStream -> def (Rust.callMethod e "is_empty" [])
    Core.StreamOffset -> def (Rust.cast val (compileType VM.Owned (Core.tWord 64)))
      where val = Rust.callMethod e "len" []
    Core.BytesOfStream -> def (Rust.callMethod e "bytes" [])

    -- Arrays
    Core.ArrayLen ->
      def (Rust.cast (Rust.callMethod e "len" [])
                     (compileType VM.Owned (Core.tWord 64)))
    Core.Concat -> def (Rust.callMethod e "concat" [])
    Core.FinishBuilder -> def (Rust.callMethod e "build" [])

    Core.CoerceTo ty ->
      [Rust.localLet [] (compileBVName x) (Just (compileType VM.Owned ty)) (Rust.callMethod e "cast_to" [])]

    Core.Neg    -> def (Rust.uni Rust.Neg e)
    Core.BitNot -> def (Rust.uni Rust.Not e)
    Core.Not    -> def (Rust.uni Rust.Not e)
    
    Core.NewIterator ->
      case argTy of
        VM.TSem (Core.TArray {}) -> def (callRTS "new_array_iterator" [e])
        VM.TSem (Core.TMap {}) -> xxx
        _ -> panic "compileOp1" ["NewIterator: unexpected argument type"]

    Core.IteratorDone -> def (Rust.callMethod e "ddl_done" [])
    Core.IteratorKey -> def (fromSize (Rust.callMethod e "ddl_key" []))
    Core.IteratorVal -> def (Rust.callMethod e "ddl_val" [])
    Core.IteratorNext -> def (Rust.callMethod e "ddl_next" [])

    Core.EJust -> def (Rust.callCon (Rust.simplePath' [ddlModName, "Maybe", "Just"]) [e])
    Core.FromJust -> def (Rust.callMethod e "unwrap" [])

    -- XXX: bitdata
    Core.SelStruct ty lab ->
      def
        (Rust.callMethod
          (Rust.callMethod (Rust.fieldAccess e (compileFieldLabel lab)) "bor" []) "clo" [])

    Core.InUnion ut lab ->
      def (mkBox (
        case Core.tnameFlav nm of
          Core.TFlavEnum {} -> noArg
          Core.TFlavUnion ls ->
            case lookup lab ls of
              Nothing -> panic "compileOp1" ["Missing lable InUnion", show (pp lab)]
              Just Core.HasData -> withArg
              Just Core.NoData  -> noArg
          Core.TFlavStruct {} -> panic "compileOp1" ["InUinion on struct"]
      ))
      where
      (hasPref,mkBox)
        | Core.tnameRec nm = (True,\val -> callRTS "new" [val])
        | otherwise = (False,id)
      nm = Core.utName ut
      noArg = Rust.pathExpr (Rust.simplePath' [compileTName hasPref nm,compileConLabel lab])
      withArg = Rust.call noArg [e]
      
    Core.FromUnion _ty lab -> def (Rust.matchExpr e [ arm1, arm2 ])
      where
      arm2 =
        Rust.matchArm Rust.wildPat
          (Rust.callMacro (Rust.simplePath "assert")
            (map Rust.litExpr [ Rust.boolLit False,
                                Rust.strLit ("Not " ++ show (pp lab))]))
      unm =
        case argTy of
          VM.TSem (Core.TUser ut) -> Core.utName ut
          _ -> bad "not a sematnic type"
      isRec = Core.tnameRec unm
      con   = Rust.simplePath' [compileTName isRec unm, compileConLabel lab]
      noArg = Rust.matchArm (Rust.conPat con []) (Rust.pathExpr (ddlPath "Unit"))
      arm1  =
        case Core.tnameFlav unm of
          Core.TFlavEnum {} -> noArg
          Core.TFlavUnion ls ->
            case lookup lab ls of
              Nothing -> bad "Missing label"
              Just Core.HasData ->
                Rust.matchArm (Rust.conPat con [Rust.identPat "a"]) (Rust.identExpr "a")
              Just Core.NoData -> noArg
          Core.TFlavStruct {} -> bad "struct"
        
      bad msg = panic "compileOp1" ["FromUnion",msg]
                  
       
    Core.WordToFloat -> def (Rust.cast e (Rust.tF 32))
    Core.WordToDouble -> def (Rust.cast e (Rust.tF 64))
    Core.IsNaN -> def (Rust.callMethod e "is_nan" [])
    Core.IsInfinite -> def (Rust.callMethod e "is_infinite" [])
    Core.IsDenormalized -> def (Rust.callMethod e "is_subnormal" [])
    Core.IsNegativeZero -> def (Rust.bin Rust.AndOp
                                  (Rust.callMethod e "is_sign_negative" [])
                                  (Rust.bin Rust.EqOp e (Rust.litExpr (Rust.floatLit 0))))
    
  where
  def re = [Rust.localLet [] (compileBVName x) Nothing re]
  tmp = show (pp op <> parens (commaSep [text (show (Rust.pretty' e))]))
  xxx =
    [ Rust.localLet [] (compileBVName x) (Just (compileVMT (VM.getType x) VM.Owned))
      (Rust.callMacro (Rust.simplePath "todo") [Rust.litExpr (Rust.strLit tmp)])
    ]


compileOp2 :: FnCtx => VM.BV -> Core.Op2 -> Rust.Expr () -> Rust.Expr () -> [Rust.Stmt ()]
compileOp2 x op e1 e2 =
  case op of
    -- Streams
    Core.Drop ->
      def (Rust.callMethod e2 "advance" [ toSize e1 ])

    Core.ArrayStream -> xxx
    Core.IsPrefix -> xxx
    Core.DropMaybe -> xxx
    Core.Take -> xxx
 
    -- Comparisons
    Core.Eq     -> bin Rust.EqOp
    Core.NotEq  -> bin Rust.NeOp
    Core.Leq    -> bin Rust.LeOp
    Core.Lt     -> bin Rust.LtOp
 
    -- Arithmetic
    Core.Add    -> bin Rust.AddOp
    Core.Sub    -> bin Rust.SubOp
    Core.Mul    -> bin Rust.MulOp
    Core.Div    -> bin Rust.DivOp
    Core.Mod    -> bin Rust.RemOp  -- XXX: rem/mod?
 
    -- Bits
    Core.BitAnd -> bin Rust.BitAndOp
    Core.BitOr  -> bin Rust.BitOrOp 
    Core.BitXor -> bin Rust.BitXorOp
    Core.Cat    -> xxx
    Core.LCat   -> xxx
    Core.LShift -> xxx
    Core.RShift -> xxx
 
    -- Arrays
    Core.ArrayIndex -> def (Rust.callMethod val "clo" [])
      where val = Rust.index e1 (Rust.cast e2 Rust.tUsize)

    -- Builders
    Core.Emit -> def (Rust.callMethod e1 "push" [ e2 ])
    Core.EmitArray -> def (Rust.callMethod e1 "push_array" [ e2 ])
    Core.EmitBuilder -> unsupported ("emit builder is not yet supported")
    
    -- Maps
    Core.MapLookup -> xxx
    Core.MapMember -> xxx
 
  where
  def e = [Rust.localLet [] (compileBVName x) Nothing e]
  bin rop = def (Rust.bin rop e1 e2)
  ppR e = text (show (Rust.pretty' e))
  tmp = show (pp op <> parens (commaSep [ppR e1, ppR e2]))
  xxx =
    [ Rust.localLet [] (compileBVName x) (Just (compileVMT (VM.getType x) VM.Owned))
      (Rust.call (Rust.identExpr "todo") [Rust.litExpr (Rust.strLit tmp)])
    ]



compileOp3 :: FnCtx => VM.BV -> Core.Op3 -> Rust.Expr () -> Rust.Expr () -> Rust.Expr () -> [Rust.Stmt ()]
compileOp3 x op e1 e2 e3 =
  case op of
    Core.RangeUp -> xxx
    Core.RangeDown -> xxx
    Core.MapInsert -> xxx
  where
  def e = [Rust.localLet [] (compileBVName x) Nothing e]
  ppR e = text (show (Rust.pretty' e))
  tmp = show (pp op <> parens (commaSep [ppR e1, ppR e2, ppR e3]))
  xxx =
    [ Rust.localLet [] (compileBVName x) (Just (compileVMT (VM.getType x) VM.Owned))
      (Rust.call (Rust.identExpr "todo") [Rust.litExpr (Rust.strLit tmp)])
    ]

compileOpN :: FnCtx => VM.BV -> Core.OpN -> [Rust.Expr ()] -> [Rust.Stmt ()]
compileOpN x op es =
  case op of
    Core.ArrayL t ->
      def (if null es then Just (compileType VM.Owned t) else Nothing)
          (callRTS "new_array" [Rust.arrExpr es])
    Core.CallF {} -> panic "compileOpN" ["Unexpected CallF"]
  where
  def mbT e = [Rust.localLet [] (compileBVName x) mbT e]

toSize :: Rust.Expr () -> Rust.Expr ()
toSize e = Rust.call (Rust.typeQualifiedExpr Rust.tUsize (Rust.simplePath "from")) [e]

fromSize :: FnCtx => Rust.Expr () -> Rust.Expr ()
fromSize e = Rust.call (Rust.typeQualifiedExpr ty (Rust.simplePath "from")) [e]
  where ty = compileType VM.Owned (Core.tWord 64)

compileExpr :: FnCtx => VM.Ownership -> VM.E -> Rust.Expr ()
compileExpr how expr =
  case expr of
    VM.EUnit         -> Rust.pathExpr (ddlPath "Unit")
    VM.ENum n ty     -> compileNumLit n ty
    VM.EBool b       -> Rust.litExpr (Rust.boolLit b)
    VM.EFloat d ty   -> Rust.litExpr (Rust.floatLit d)
    VM.EMapEmpty k v -> unsupported (?fnMsg <+> "empty map expression") -- XXX
    VM.ENothing {}   -> Rust.pathExpr (Rust.simplePath' [ddlModName, "Maybe", "Nothing"])
    VM.EBlockArg x   -> mbBorrow (VM.getOwnership x) (Rust.identExpr (compileBAName x))
    VM.EVar x        -> mbBorrow (VM.getOwnership x) (Rust.identExpr (compileBVName x))
  where
  mbBorrow own e =
    case (how, own) of
      (VM.Borrowed, VM.Owned) -> Rust.callMethod e "bor" []
      _ -> e

compileNumLit :: FnCtx => Integer -> Core.Type -> Rust.Expr ()
compileNumLit n ty =
  case ty of
    Core.TUInt _ -> intoWord False
    Core.TSInt _ -> intoWord True
    Core.TFloat  -> Rust.litExpr (Rust.floatLit (fromInteger n))
    Core.TDouble -> Rust.litExpr (Rust.floatLit (fromInteger n))
    _ -> unsupported (?fnMsg <+> "numeric literal at type" <+> pp ty)
  where
  -- Use .into() on the literal, relying on type inference from context
  intoWord sign = 
    Rust.call (Rust.typeQualifiedExpr (compileType VM.Owned ty) (Rust.simplePath "from"))
      [Rust.litExpr (Rust.intLit' suf n)]
    where
    suf = if sign then Rust.I64 else Rust.U64

compileCInstr :: FnCtx => VM.CInstr -> [Rust.Stmt ()]
compileCInstr cinstr =
  case cinstr of
    VM.Jump jp -> compileJump jp []
    VM.JumpIf e opts ->
      [Rust.expr (Rust.matchExpr (deref (compileExpr VM.Borrowed e)) (compileJumpChoice ty opts))]
      where
      ty    = VM.getType e
      deref = if isRecTy ty then Rust.uni Rust.Deref else id
      
    VM.Yield             -> bad
    VM.ReturnNo          -> [Rust.ret (Rust.identExpr "None")]
    VM.ReturnYes res inp -> [Rust.ret (Rust.call (Rust.identExpr "Some") [Rust.tupleExpr (map (compileExpr VM.Owned) [res,inp])])]
    VM.ReturnPure res    -> [Rust.ret (compileExpr VM.Owned res)]
    VM.CallPure f j es   -> compileJumpWithFree j [doCall f es]
    VM.CallNoCapture f (VM.JumpCase opts) es ->
      [ Rust.expr (Rust.matchExpr (doCall f es) 
          [ Rust.matchArm (Rust.somePat (Rust.tuplePat [Rust.identPat "x", Rust.identPat "i"]))
                          (opt True [Rust.identExpr "x", Rust.identExpr "i"]),
            Rust.matchArm Rust.nonePat (opt False [])
          ]
        )
      ]
      where
      opt x y =
        case Map.lookup x opts of
          Just a -> Rust.blockExpr (compileJumpWithFree a y)
          Nothing -> panic "compileCInstr" ["Missing option", show x]
      
      
    VM.CallCapture {}    -> bad
    VM.TailCall f c es ->
      case c of
        VM.NoCapture -> [ Rust.ret (doCall f es) ]
        _            ->  bad
  where
  bad = panic "compileCInstr" ["Unexpected instruction", show (pp cinstr)]
  doCall f es = Rust.call (Rust.identExpr (compileFName f))
                          (zipWith compileExpr sig es)
    where
    sig =
      case Map.lookup f ?funSigs of
        Just s -> s
        Nothing -> panic "compileCInstr" ["Missing ownership signature for", show (pp f)]

 
compileJump :: FnCtx => VM.JumpPoint -> [Rust.Expr ()] -> [Rust.Stmt ()]
compileJump (VM.JumpPoint l es) extra =
 [ Rust.assign
    (Rust.identExpr pcName)
    (Rust.callCon (Rust.simplePath' [contTypeName, compileBlockLabel l])
               (extra ++ zipWith compileExpr (drop (length extra) sig) es)),
    Rust.continue
  ]
  where
  sig =
    case Map.lookup l ?blockSigs of
      Just s  -> s
      Nothing -> panic "compileJump" ["Missing ownership signature for block", show (pp l)]

compileJumpWithFree :: FnCtx => VM.JumpWithFree -> [Rust.Expr ()] -> [Rust.Stmt ()]
compileJumpWithFree = compileJump . VM.jumpTarget -- Rust will do the freeing

compileJumpChoice :: FnCtx => VM.VMT -> VM.JumpChoice Core.Pattern -> [Rust.Arm ()]
compileJumpChoice ty (VM.JumpCase opts) =
  [ Rust.matchArm (compilePat ty p) (Rust.blockExpr (compileJumpWithFree k []))
    -- XXX: for big integers we should use a decision tree instead of `match`
  | (p,k) <- Map.toList opts
  ]

isRecTy :: VM.VMT -> Bool
isRecTy ty =
  case ty of
    VM.TSem (Core.TUser ut) -> Core.tnameRec (Core.utName ut)
    _ -> False


compilePat :: VM.VMT -> Core.Pattern -> Rust.Pat ()
compilePat ty p =
  case p of
    Core.PBool b    -> Rust.litPat (Rust.boolLit b)
    Core.PNothing   -> Rust.conPat (Rust.simplePath' [ddlModName, "Maybe", "Nothing"]) []
    Core.PJust      -> Rust.conPat (Rust.simplePath' [ddlModName, "Maybe", "Just"]) [Rust.wildPat]
    Core.PNum n     -> Rust.litPat (Rust.intLit n)
    Core.PBytes {}  -> panic "compilePat" ["Unexpecte PBytes"]
    Core.PCon uc    ->
      case ty of
        VM.TSem (Core.TUser ut) -> Rust.conPat nm [Rust.wildPat | hasData ]
          where
          nm    = Rust.simplePath' [qual, compileConLabel uc]
          unm   = Core.utName ut
          qual  = compileTName (isRecTy ty) unm
          hasData =
            case Core.tnameFlav unm of
              Core.TFlavEnum {} -> False
              Core.TFlavUnion ls -> lookup uc ls == Just Core.HasData
              Core.TFlavStruct {} -> panic "compilePat" ["struct"]


        _  -> panic "compilePat" ["Con pat for not TUser"]

    Core.PAny       -> Rust.wildPat

