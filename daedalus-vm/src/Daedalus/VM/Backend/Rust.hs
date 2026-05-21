{-# Language ImportQualifiedPost, OverloadedStrings, ImplicitParams, ConstraintKinds #-}
module Daedalus.VM.Backend.Rust (
  compileProgram,
  Config(..)
) where

import Data.Text qualified as Text
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List(foldl',sortOn)
import Data.ByteString qualified as BS
import Data.Word(Word8,Word64)
import Data.Int(Int64)
import Data.Bits

import Daedalus.PP
import Daedalus.Rec(forgetRecs)
import Daedalus.Panic(panic)
import Daedalus.Core qualified as Core
import Daedalus.VM qualified as VM
import Daedalus.VM.Backend.Rust.Lang qualified as Rust
import Daedalus.VM.Backend.Rust.Names
import Daedalus.VM.BorrowAnalysis
import Daedalus.VM.Backend.Rust.Type

data Config = Config

type ProgCtx = (
  ?funSigs :: Map VM.FName [VM.Ownership],
  ?blockSigs :: Map VM.Label [VM.Ownership],
  ?tyDecls :: Map Core.TName Core.TDecl,
  ?allFuns :: Map VM.FName VM.VMFun
  )

type FnCtx = (ProgCtx, ?isPure :: Bool, ?fnMsg :: Doc, ?curFunThrows :: VM.Throws)

compileProgram :: Config -> VM.Program -> String
compileProgram _cfg vm = show (Rust.pretty' result)
  where
  result :: Rust.SourceFile ()
  result =
    let ?funSigs = funSigs
        ?blockSigs = blockSigs
        ?tyDecls = Map.fromList [ (Core.tName t, t)
                                | m <- VM.pModules vm,
                                  t <- forgetRecs (VM.mTypes m) ]
        ?allFuns = Map.fromList [ (VM.vmfName f, f)
                                | m <- VM.pModules vm,
                                  f <- VM.mFuns m ]
    in
    Rust.SourceFile Nothing [] (uses ++ concatMap compileModule (VM.pModules vm))

  unusedOk = [Rust.disableWarning "unused_imports"]
  uses = [
    Rust.use' unusedOk (Rust.useOne (Rust.simplePath "daedalus_rts_rust") (Just "ddl")),
    Rust.use' unusedOk (Rust.useSelect (Rust.simplePath "ddl") [ Rust.useOne x Nothing | x <- map Rust.simplePath [ "Type", "Clo" ] ]),
    Rust.use' unusedOk (Rust.useOne (Rust.simplePath "serde") Nothing)
    ]
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
  throws          = VM.vmfThrows fu
  valTy           = compileType VM.Owned (Core.fnameType fnm)
    where ?fnMsg = fnMsg
  resT
    | VM.vmfPure fu, throws == VM.Throws =
        Rust.pathType (Rust.pathWithTypes [ddlModName, "PureResult"] [valTy])
    | VM.vmfPure fu = valTy
    | otherwise =
        Rust.pathType (Rust.pathWithTypes [ddlModName, "ParserResult"] [valTy])
    where ?fnMsg = fnMsg

  args = (parserStateName, Rust.tMutRef (Rust.pathType (ddlPath "ParserState"))) : args'
  (args',def) =
    let ?isPure = VM.vmfPure fu
        ?fnMsg  = fnMsg
        ?curFunThrows = throws
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
            start     = Rust.callCon (Rust.simplePath' [contTypeName,l]) (map (Rust.identExpr . fst) as)
        in (as, Rust.localLetMut ["unused_mut"] pcName Nothing start)
      _ -> panic "compileFunBody" ["Mssing entry block code"]

  mainLoop =
    Rust.expr (
      Rust.loopExpr (Just funLoopName) (Rust.block [
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
  argTs     = [ compileVMT (VM.getOwnership ba) (VM.getType ba) | ba <- args ]
  lab       = compileBlockLabel (VM.blockName bl)
  alt       = Rust.matchArm
                (Rust.conPat (Rust.simplePath' [contTypeName,lab]) (map Rust.identPat argNames))
                (Rust.blockExpr code)
  code = concatMap compileBlockInstr (VM.blockInstrs bl) ++
         compileCInstr (VM.blockTerm bl)

compileBlockInstr :: FnCtx => VM.Instr -> [Rust.Stmt ()]
compileBlockInstr instr =
  case instr of
    VM.Say msg          -> pstate "say" [ str msg ]
    VM.Output {}        -> bad
    VM.Notify {}        -> bad
    VM.CallPrim x f es  -> compilePrim x f es
    VM.CallPrim2 x y f es -> compilePrim2 x y f es
    VM.Spawn {}         -> bad
    VM.Let x e ->
      [ Rust.localLet [] (compileBVName x) Nothing (Rust.callMethod (compileExpr VM.Borrowed e) "clo" [])]
    VM.Free _                   -> [] -- Rust should drop things on its own
    VM.NoteFail loc s inp msg ->
      pstate "note_fail" [ Rust.litExpr (Rust.boolLit (loc == Core.ErrorFromUser))
                         , str s
                         , compileExpr VM.Borrowed inp
                         , compileExpr VM.Borrowed msg
                         ]
    VM.PushDebug t f            -> pstate "push" [ is_tail, str (Text.unpack f) ]
      where is_tail = Rust.litExpr (Rust.boolLit (t == VM.DebugTailCall))
    VM.PopDebug                 -> pstate "pop" []
  where
  bad  = panic "compileBlockInstr" ["Unexpected instruction", show (pp instr)]
  pstate x ys =
    [ Rust.expr_ (Rust.callMethod (Rust.identExpr parserStateName) x ys) ]
  str x = Rust.litExpr (Rust.strLit x)


callRTS :: Rust.Ident -> [Rust.Expr ()] -> Rust.Expr ()
callRTS f = Rust.call (Rust.pathExpr (ddlPath f))

-- | Convert an Integer to big-endian signed bytes
integerToBytes :: Integer -> [Word8]
integerToBytes i
  | i == 0 = [0]
  | i > 0 =
      let bytes = reverse (go i)
      in if most bytes
         then 0 : bytes  -- Add leading 0 if high bit is set
         else bytes
  | otherwise =  -- i < 0
      let bytes = reverse (go (abs i - 1))
          inverted = map complement bytes
      in if most inverted
         then inverted
         else 0xff : inverted  -- Add leading 0xff if high bit is clear
  where
  go 0 = []
  go n = fromInteger (n .&. 0xff) : go (n `shiftR` 8)

  most x =
    case x of
      b : _ -> testBit b 7
      [] -> panic "integerToBytes" ["[]"]


compilePrim2 :: FnCtx => VM.BV -> VM.BV -> VM.PrimName -> [VM.E] -> [Rust.Stmt ()]
compilePrim2 x y f es =
  case (f, compiled) of
    (VM.Op2 Core.Add, [e1,e2]) -> checkedArith "op_add" e1 e2
    (VM.Op2 Core.Sub, [e1,e2]) -> checkedArith "op_sub" e1 e2
    (VM.Op2 Core.Mul, [e1,e2]) -> checkedArith "op_mul" e1 e2
    _ -> panic "compilePrim2" ["not yet implemented", show (pp f)]
  where
  compiled = zipWith compileExpr (modePrimName f) es
  checkedArith method e1 e2 =
    [ Rust.localLetPat
        (Rust.tuplePat [Rust.identPat (compileBVName y), Rust.identPat (compileBVName x)])
        Nothing
        (Rust.callMethod e1 method [e2])
    ]

compilePrim :: FnCtx => VM.BV -> VM.PrimName -> [VM.E] -> [Rust.Stmt ()]
compilePrim x prim es =
  case prim of
    VM.ByteArray bs ->
      def ty (callRTS "new_byte_array" [Rust.litExpr (Rust.bytesLit bs)])
        where ty = if BS.null bs then Just (compileType VM.Owned (Core.TArray Core.TByte)) else Nothing

    VM.NewBuilder ty -> 
      def (Just (compileType VM.Owned (Core.TBuilder ty))) (callRTS "new_builder" [])

    VM.Integer i
      | i == 0 -> def Nothing (Rust.typeQualifiedExpr ty (Rust.simplePath "ZERO"))
      | otherwise -> def (Just ty) expr
      where
      ty = compileType VM.Owned Core.TInteger
      expr
        | toInteger (minBound :: Int64) <= i && i <= toInteger (maxBound :: Int64) =
          Rust.callMethod (Rust.litExpr (Rust.intLit' Rust.I64 i)) "into" []
        | 0 <= i && i <= toInteger (maxBound :: Word64) =
          Rust.callMethod (Rust.litExpr (Rust.intLit' Rust.U64 i)) "into" []
        | otherwise =
          Rust.call (Rust.typeQualifiedExpr ty (Rust.simplePath "from_signed_bytes_be"))
            [Rust.addrOf (Rust.arrExpr [Rust.litExpr (Rust.intLit' Rust.U8 (toInteger b)) | b <- integerToBytes i])]

    VM.StructCon ut
      | Core.tnameBD nm ->
        case Map.lookup nm ?tyDecls of
          Just decl
            | Core.TBitdata _ (Core.BDStruct fs0) <- Core.tDef decl ->
              let addData fs args =
                    case fs of
                      [] ->

                        case args of
                          [] -> []
                          _  -> panic "compilePrim" ["Bad bitdata constructor"]

                      f : more ->
                        case Core.bdFieldType f of
                          Core.BDWild -> addData more args

                          Core.BDTag n -> 
                            Rust.tupleExpr
                              [ Rust.litExpr (Rust.intLit (toInteger (Core.bdOffset f)))
                              , compileNumLit n (Core.TUInt (Core.TSize (toInteger (Core.bdWidth f))))
                              ] : addData more args

                          Core.BDData {} ->
                            case args of
                              e : more_es ->
                                Rust.tupleExpr
                                  [ Rust.litExpr (Rust.intLit (toInteger (Core.bdOffset f)))
                                  , e
                                  ] : addData more more_es
                              _ -> panic "compilePrim" ["Malformed bitdata struct constructor"]

              in def Nothing (Rust.callMacro (ddlPath "bitdata_con")
                    (Rust.identExpr (compileTName False nm) : addData fs0 compiled))

          _ -> panic "compilePrim" ["Missing bitdata type", show (pp nm)]
      | otherwise ->
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
      case (compiled,es) of
        ([e1,e2],[a,b]) -> compileOp2 x op e1 e2 (VM.getType a) (VM.getType b)
        _       -> bad 2

    VM.Op3 op ->
      case (compiled,es) of
        ([e1,e2,e3],[a,b,c]) ->
          compileOp3 x op e1 e2 e3 (VM.getType a) (VM.getType b) (VM.getType c)
        _          -> bad 3

    VM.OpN op -> compileOpN x op compiled
  
  where
  compiled  = zipWith compileExpr (modePrimName prim) es
  bad n     = panic "compilePrim" ["Expected " ++ show (n::Int) ++ "argument, but have " ++ show (length es), show (pp prim) ]
  def mb re = [Rust.localLet [] (compileBVName x) mb re]
  
compileOp1 :: FnCtx => VM.BV -> Core.Op1 -> Rust.Expr () -> VM.VMT -> [Rust.Stmt ()]
compileOp1 x op e argTy =
  case op of
    -- Streams
    Core.Head -> def (Rust.callMethod e "head" [])
    Core.IsEmptyStream -> def (Rust.callMethod e "is_empty" [])
    Core.StreamOffset -> def (fromSize val)
      where val = Rust.callMethod e "len" []
    Core.BytesOfStream -> def (Rust.callMethod e "bytes" [])

    -- Arrays
    Core.ArrayLen       -> def (fromSize (Rust.callMethod e "len" []))
    Core.Concat         -> def (Rust.callMethod e "concat" [])
    Core.FinishBuilder  -> def (Rust.callMethod e "build" [])

    Core.CoerceTo ty
      | srcTy == ty -> def e

      | Just {} <- Core.isBits srcTy ->
        case () of
          _
            | Just _ <- Core.isBits ty ->
              [Rust.localLet [] (compileBVName x) (Just (compileType VM.Owned ty)) (Rust.callMethod e "cast_to" [])]
            | Core.TInteger <- ty -> use_into e
            | Core.TFloat  <- ty -> def (Rust.callMethod e "to_f32" [])
            | Core.TDouble <- ty -> def (Rust.callMethod e "to_f64" [])
            | Core.TUser {} <- ty -> mk_bd e
            | otherwise -> badTgt

      | Core.TInteger <- srcTy ->
        case () of
          _
            | Just {} <- Core.isBits ty -> use_into e
            | Core.TFloat  <- ty -> def (Rust.callMethod e "to_f32" [])
            | Core.TDouble <- ty -> def (Rust.callMethod e "to_f64" [])
            | Core.TUser {} <- ty -> mk_bd (Rust.callMethod e "into" [])
            | otherwise -> badTgt

      | Core.TFloat <- srcTy ->
        case () of
          _
            | Just (False, _) <- Core.isBits ty -> defTy (fromTgt "from_f32" e)
            | Just (True, _)  <- Core.isBits ty -> defTy (fromTgt "from_f32" e)
            | Core.TInteger <- ty -> def (callRTS "Int::from_f32" [e])
            | Core.TDouble  <- ty -> def (Rust.cast e (Rust.tF 64))
            | otherwise -> badTgt

      | Core.TDouble <- srcTy ->
        case () of
          _
            | Just (False, _) <- Core.isBits ty -> defTy (fromTgt "from_f64" e)
            | Just (True, _)  <- Core.isBits ty -> defTy (fromTgt "from_f64" e)
            | Core.TInteger <- ty -> def (callRTS "Int::from_f64" [e])
            | Core.TFloat   <- ty -> def (Rust.cast e (Rust.tF 32))
            | otherwise -> badTgt

      | Core.TUser {} <- srcTy ->
        case ty of
          Core.TUInt {} -> def (Rust.callMethod e "to_bits" [])
          Core.TInteger -> use_into (Rust.callMethod e "to_bits" [])
          _ -> badTgt
      | otherwise -> panic "compileOp1" ["Bad source type in coerce"]
      where
      use_into v = [Rust.localLet [] (compileBVName x) (Just (compileType VM.Owned ty)) (Rust.callMethod v "into" [])]
      defTy re = [Rust.localLet [] (compileBVName x) (Just (compileType VM.Owned ty)) re]
      fromTgt meth arg = Rust.call (Rust.typeQualifiedExpr (compileType VM.Owned ty) (Rust.simplePath meth)) [arg]

      -- XXX: This is where we should normalize things to fix #395.
      mk_bd v = def (Rust.call
                      (Rust.typeQualifiedExpr (compileType VM.Owned ty) (Rust.simplePath "from_bits_unchecked")) [v]
                    )
      badTgt = panic "compileOp1" ["Bad target type in coerce"]

      srcTy = case argTy of
                VM.TSem t -> t
                _ -> panic "compileOp1" ["CoerceTo with non-semantic type"]

    Core.Neg    -> def (Rust.uni Rust.Neg e)
    Core.BitNot -> def (Rust.uni Rust.Not e)
    Core.Not    -> def (Rust.uni Rust.Not e)
    
    Core.NewIterator ->
      case argTy of
        VM.TSem (Core.TArray {}) -> def (callRTS "new_array_iterator" [e])
        VM.TSem (Core.TMap {}) -> def (callRTS "new_map_iterator" [e])
        _ -> panic "compileOp1" ["NewIterator: unexpected argument type"]

    Core.IteratorDone -> def (Rust.callMethod e "ddl_done" [])
    Core.IteratorKey -> def (fromSize (Rust.callMethod e "ddl_key" []))
    Core.IteratorVal -> def (Rust.callMethod e "ddl_val" [])
    Core.IteratorNext -> def (Rust.callMethod e "ddl_next" [])

    Core.EJust -> def (Rust.callCon (Rust.simplePath' [ddlModName, "Maybe", "Just"]) [e])
    Core.FromJust -> def (Rust.callMethod (Rust.callMethod e "unwrap" []) "clo" [])

    Core.SelStruct ty lab ->
      case argTy of
        VM.TSem (Core.TUser ut)
          | Core.tnameBD (Core.utName ut) ->
            def (Rust.callMethod e (compileBDFieldLabel lab) [])
          | otherwise ->
            def
              (Rust.callMethod
                (Rust.callMethod (Rust.fieldAccess e (compileFieldLabel lab)) "bor" []) "clo" [])
        _ -> panic "compileOp1" ["Unexpected type in field selection", show (pp ty)]

    Core.InUnion ut lab
      | Core.tnameBD nm ->
        def (
          Rust.call
          (Rust.typeQualifiedExpr (compileVMT VM.Owned (VM.getType x)) (Rust.simplePath "from_bits_unchecked"))
          [Rust.callMethod e "to_bits" []]
        )
      | otherwise ->
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
      
    Core.FromUnion ty lab
      | Core.tnameBD unm ->
        def (Rust.call (Rust.typeQualifiedExpr (compileType VM.Owned ty) (Rust.simplePath "from_bits_unchecked")) [Rust.callMethod e "to_bits" []])
      | otherwise -> def (Rust.matchExpr e [ arm1, arm2 ])
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
                  
       
    Core.WordToFloat -> def (Rust.call (Rust.pathExpr (Rust.simplePath' ["f32", "from_bits"]))
                              [Rust.call (Rust.pathExpr (Rust.simplePath' ["u32", "from"])) [e]])
    Core.WordToDouble -> def (Rust.call (Rust.pathExpr (Rust.simplePath' ["f64", "from_bits"]))
                              [Rust.call (Rust.pathExpr (Rust.simplePath' ["u64", "from"])) [e]])
    Core.IsNaN -> def (Rust.callMethod e "is_nan" [])
    Core.IsInfinite -> def (Rust.callMethod e "is_infinite" [])
    Core.IsDenormalized -> def (Rust.callMethod e "is_subnormal" [])
    Core.IsNegativeZero -> def (Rust.bin Rust.AndOp
                                  (Rust.callMethod e "is_sign_negative" [])
                                  (Rust.bin Rust.EqOp e (Rust.litExpr (Rust.floatLit 0))))
    
  where
  def re = [Rust.localLet [] (compileBVName x) Nothing re]

compileOp2 :: FnCtx => VM.BV -> Core.Op2 -> Rust.Expr () -> Rust.Expr () -> VM.VMT -> VM.VMT -> [Rust.Stmt ()]
compileOp2 x op e1 e2 t1 t2 =
  case op of
    -- Streams
    Core.Drop         -> def (Rust.callMethod e2 "advance" [ toSize e1 ])
    Core.ArrayStream  -> def (callRTS "new_input" [e1,e2])
    Core.IsPrefix     -> def (Rust.callMethod e2 "is_prefix" [e1])
    Core.DropMaybe    -> def (Rust.callMethod e2 "advance_maybe" [ toSize e1 ])
    Core.Take         -> def (Rust.callMethod e2 "restrict" [ toSize e1 ])
 
    -- Comparisons
    Core.Eq     -> bin Rust.EqOp
    Core.NotEq  -> bin Rust.NeOp
    Core.Leq    -> bin Rust.LeOp
    Core.Lt     -> bin Rust.LtOp
 
    -- Arithmetic (only Integer; bounded types must use CallPrim2)
    Core.Add
      | VM.TSem Core.TInteger <- t1 -> bin Rust.AddOp
      | otherwise -> panic "compileOp2" ["use CallPrim2 for checked arithmetic", show (pp op)]
    Core.Sub
      | VM.TSem Core.TInteger <- t1 -> bin Rust.SubOp
      | otherwise -> panic "compileOp2" ["use CallPrim2 for checked arithmetic", show (pp op)]
    Core.Mul
      | VM.TSem Core.TInteger <- t1 -> bin Rust.MulOp
      | otherwise -> panic "compileOp2" ["use CallPrim2 for checked arithmetic", show (pp op)]
    Core.Div    -> bin Rust.DivOp
    Core.Mod    -> bin Rust.RemOp
 
    -- Bits
    Core.BitAnd -> bin Rust.BitAndOp
    Core.BitOr  -> bin Rust.BitOrOp 
    Core.BitXor -> bin Rust.BitXorOp
    Core.Cat
      | Just m <- okT t1
      , Just n <- okT t2
      -> def (Rust.callMacro (ddlPath "cat") [ lit m, lit n, e1, e2 ])
      | otherwise -> panic "compileOp2" ["Unexpected types for `cat`"]
      where
      lit = Rust.litExpr . Rust.intLit
      okT a =
        case a of
          VM.TSem t -> Core.isUInt t
          _ -> Nothing

    Core.LCat   ->
      case t1 of
        VM.TSem Core.TInteger -> def (Rust.callMethod e1 "lcat" [e2])
        _ -> def (callRTS "lcat" [ e1, e2 ])
    Core.LShift -> bin' Rust.ShlOp e1 (toSize e2)
    Core.RShift -> bin' Rust.ShrOp e1 (toSize e2)
 
    -- Arrays
    Core.ArrayIndex -> def (Rust.callMethod val "clo" [])
      where val = Rust.index e1 (toSize e2)

    -- Builders
    Core.Emit -> def (Rust.callMethod e1 "push" [ e2 ])
    Core.EmitArray -> def (Rust.callMethod e1 "push_array" [ e2 ])
    Core.EmitBuilder -> unsupported ("emit builder is not yet supported")
    
    -- Maps
    Core.MapLookup -> def (Rust.callMethod e1 "lookup" [e2])
    Core.MapMember -> def (Rust.callMethod e1 "contains" [e2])
 
  where
  def e         = [Rust.localLet [] (compileBVName x) Nothing e]
  bin rop       = bin' rop e1 e2
  bin' rop l r  = def (Rust.bin rop l r)
  


compileOp3 :: FnCtx => VM.BV -> Core.Op3 -> Rust.Expr () -> Rust.Expr () -> Rust.Expr () ->
              VM.VMT -> VM.VMT -> VM.VMT -> [Rust.Stmt ()]
compileOp3 x op e1 e2 e3 t1 _t2 _t3 =
  case op of

    Core.RangeUp ->
      def (
        callRTS "new_array_iter" [
          case t1 of
            VM.TSem (Core.TUInt _) -> callRTS "rng_up_u" [e1,e2,e3]
            VM.TSem (Core.TSInt _) -> callRTS "rng_up_i" [e1,e2,e3]
            VM.TSem Core.TInteger  -> unsupported "RangeUp on Integer"
            _ -> bad
        ])

    Core.RangeDown ->
      def (
        callRTS "new_array_iter" [
          case t1 of
            VM.TSem (Core.TUInt _) -> callRTS "rng_down_u" [e1,e2,e3]
            VM.TSem (Core.TSInt _) -> callRTS "rng_down_i" [e1,e2,e3]
            VM.TSem Core.TInteger  -> unsupported "RangeDown on Integer"
            _ -> bad
        ])

    Core.MapInsert -> def (Rust.callMethod e1 "insert" [e2,e3])
  where
  bad   = panic "compileOp3" ["Unexpected",show (pp op)]
  def e = [Rust.localLet [] (compileBVName x) Nothing e]
  
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
    VM.EFloat d ty   -> Rust.litExpr (Rust.floatLit' suff d)
      where
      suff =
        case ty of
          Core.TFloat -> Rust.F32
          Core.TDouble -> Rust.F64
          _ -> panic "compileExpr" ["Unepxeted EFloat type"]
        
    VM.EMapEmpty k v -> Rust.call (Rust.pathExpr (Rust.pathWithTypes [ddlModName, "empty_map"] [rk,rv])) []
      where rk = compileType VM.Owned k
            rv = compileType VM.Owned v
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
    Core.TFloat  -> Rust.litExpr (Rust.floatLit' Rust.F32 (fromInteger n))
    Core.TDouble -> Rust.litExpr (Rust.floatLit' Rust.F64 (fromInteger n))
    _ -> panic "compileNumLit" [show (?fnMsg <+> "numeric literal at type" <+> pp ty)]
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
    VM.JumpIf e opts
      | VM.TSem Core.TInteger <- ty ->
        compileIntCase (compileExpr VM.Borrowed e) opts
      | otherwise ->
      [Rust.expr (Rust.matchExpr (tweak (compileExpr VM.Borrowed e)) (compileJumpChoice ty opts))]
      where
      ty    = VM.getType e
      tweak
        | isRecTy ty                   = Rust.uni Rust.Deref
        | VM.TSem (Core.TUInt _) <- ty = \r -> Rust.callMethod r "into" []
        | VM.TSem (Core.TSInt _) <- ty = \r -> Rust.callMethod r "into" []
        | VM.TSem (Core.TUser ut) <- ty, Core.tnameBD (Core.utName ut) = \r -> Rust.callMethod r "to_enum" []
        | otherwise                    = id
      
    VM.Yield             -> bad
    VM.ReturnNo          ->
      [Rust.ret (Rust.pathExpr (Rust.simplePath' [ddlModName, "ParserResult", "Failure"]))]
    VM.ReturnYes res inp ->
      [Rust.ret (Rust.call
        (Rust.pathExpr (Rust.simplePath' [ddlModName, "ParserResult", "Ok"]))
        (map (compileExpr VM.Owned) [res,inp]))]
    VM.ReturnPure res
      | ?curFunThrows == VM.Throws ->
        [Rust.ret (Rust.call
          (Rust.pathExpr (Rust.simplePath' [ddlModName, "PureResult", "Ok"]))
          [compileExpr VM.Owned res])]
      | otherwise -> [Rust.ret (compileExpr VM.Owned res)]

    VM.Throw loc msg
      | ?isPure ->
        [Rust.ret (Rust.call
          (Rust.pathExpr (Rust.simplePath' [ddlModName, "PureResult", "Exception"]))
          [Rust.litExpr (Rust.strLit (Text.unpack loc)), Rust.litExpr (Rust.strLit (Text.unpack msg))])]
      | otherwise ->
        [ Rust.expr_ (Rust.callMethod (Rust.identExpr parserStateName) "set_exception"
            [Rust.litExpr (Rust.strLit (Text.unpack loc)), Rust.litExpr (Rust.strLit (Text.unpack msg))])
        , Rust.ret (Rust.pathExpr (Rust.simplePath' [ddlModName, "ParserResult", "Exception"]))
        ]

    VM.CallPure f j es _exnFree
      | calleeThrows ->
        [ Rust.expr (Rust.matchExpr (doCall f es)
            [ Rust.matchArm
                (Rust.conPat (Rust.simplePath' [ddlModName, "PureResult", "Ok"])
                             [Rust.identPat "x"])
                (Rust.blockExpr (compileJumpWithFree j [Rust.identExpr "x"]))
            , Rust.matchArm
                (Rust.conPat (Rust.simplePath' [ddlModName, "PureResult", "Exception"])
                             [Rust.identPat "el", Rust.identPat "em"])
                (Rust.blockExpr (propagateExnFromPure))
            ])
        ]
      | otherwise -> compileJumpWithFree j [doCall f es]
      where
      calleeThrows = case Map.lookup f ?allFuns of
                       Just fun -> VM.vmfThrows fun == VM.Throws
                       Nothing  -> False

    VM.CallNoCapture f (VM.JumpCase opts) es _exnFree ->
      [ Rust.expr (Rust.matchExpr (doCall f es)
          [ Rust.matchArm
              (Rust.conPat (Rust.simplePath' [ddlModName, "ParserResult", "Ok"])
                           [Rust.identPat "x", Rust.identPat "i"])
              (opt True [Rust.identExpr "x", Rust.identExpr "i"])
          , Rust.matchArm
              (Rust.conPat (Rust.simplePath' [ddlModName, "ParserResult", "Failure"]) [])
              (opt False [])
          , Rust.matchArm
              (Rust.conPat (Rust.simplePath' [ddlModName, "ParserResult", "Exception"]) [])
              (Rust.blockExpr (propagateExnFromParser))
          ]
        )
      ]
      where
      opt x y =
        case Map.lookup x opts of
          Just a -> Rust.blockExpr (compileJumpWithFree a y)
          Nothing -> panic "compileCInstr" ["Missing option", show x]


    VM.CallCapture {} -> bad
    VM.TailCall f c es ->
      case c of
        VM.NoCapture -> [ Rust.ret (doCall f es) ]
        _            ->  bad
  where
  bad = panic "compileCInstr" ["Unexpected instruction", show (pp cinstr)]

  propagateExnFromPure
    | ?isPure =
      [Rust.ret (Rust.call
        (Rust.pathExpr (Rust.simplePath' [ddlModName, "PureResult", "Exception"]))
        [Rust.identExpr "el", Rust.identExpr "em"])]
    | otherwise =
      [ Rust.expr_ (Rust.callMethod (Rust.identExpr parserStateName) "set_exception"
          [Rust.identExpr "el", Rust.identExpr "em"])
      , Rust.ret (Rust.pathExpr (Rust.simplePath' [ddlModName, "ParserResult", "Exception"]))
      ]

  propagateExnFromParser =
    [Rust.ret (Rust.pathExpr (Rust.simplePath' [ddlModName, "ParserResult", "Exception"]))]

  doCall f es = Rust.call (Rust.identExpr (compileFName f))
                          (Rust.identExpr parserStateName : zipWith compileExpr sig es)
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
    Rust.continueLab funLoopName
  ]
  where
  sig =
    case Map.lookup l ?blockSigs of
      Just s  -> s
      Nothing -> panic "compileJump" ["Missing ownership signature for block", show (pp l)]

compileJumpWithFree :: FnCtx => VM.JumpWithFree -> [Rust.Expr ()] -> [Rust.Stmt ()]
compileJumpWithFree = compileJump . VM.jumpTarget -- Rust will do the freeing


compileIntCase ::
  FnCtx => Rust.Expr () -> VM.JumpChoice Core.Pattern -> [Rust.Stmt ()]
compileIntCase e (VM.JumpCase ps0) = classify [] [] [] [] [] (Map.toList ps0)
  where
  classify dflt useI useUI useU useBig ps =
    case ps of
      (p,k) : more ->
        case p of
          Core.PAny -> classify (k : dflt) useI useUI useU useBig more
          Core.PNum n
            | n < 0 ->
              if toInteger (minBound :: Int64) <= n
                then classify dflt ((n,k) : useI) useUI useU useBig more
                else classify dflt useI useUI useU ((n,k) : useBig) more
            | n <= toInteger (maxBound :: Int64) ->
              classify dflt useI ((n,k) : useUI) useU useBig more
            | n <= toInteger (maxBound :: Word64) ->
                classify dflt useI useUI ((n,k) : useU) useBig more
            | otherwise -> classify dflt useI useUI useU ((n,k) : useBig) more
          _ -> panic "compuleJumpChoice" ["Unexpected pattern for TInteger"]
      [] -> [ Rust.expr $
                Rust.matchExpr
                  (Rust.callMethod e meth [])
                  (map smallAlt smallCases ++ [bigAlt bigCases])
            ]
        where
        smallAlt (p,k) =
          Rust.matchArm (Rust.somePat (Rust.litPat (Rust.intLit' smallSuff p)))
                        (Rust.blockExpr (compileJumpWithFree k []))
        
        bigAlt opts =
          Rust.matchArm Rust.wildPat $
          case opts of
            [] -> dfltK
            _  ->
              Rust.blockExpr
                [ Rust.expr_ $ Rust.blockExprLab intDecisionTreeDfltName $
                    Rust.block [
                      Rust.expr_ (decisionTree (length opts) opts)
                    ]
                , Rust.expr dfltK
                ]

        dfltK =
          case dflt of
            [] -> Rust.callMacro (Rust.simplePath "unreachable") []
            [k] -> Rust.blockExpr (compileJumpWithFree k [])
            _   -> panic "compileIntCase" ["Multiple default cases"]

        
        decisionTree len opts =
          case splitAt n opts of
            (as,(b,k):bs) ->
              Rust.matchExpr (Rust.callMethod e "cmp" [lit b])
                [ Rust.matchArm (p "Less") (decisionTree n as)
                , Rust.matchArm (p "Equal") (Rust.blockExpr (compileJumpWithFree k []))
                , Rust.matchArm (p "Greater") (decisionTree (len - n - 1) bs)
                ]
            _ -> Rust.break (Just intDecisionTreeDfltName)
            where
            n   = div len 2
            p x = Rust.conPat (Rust.simplePath' ["std","cmp","Ordering",x]) []
            ty  = compileType VM.Owned Core.TInteger
            lit x =
              Rust.addrOf $
              Rust.call (Rust.typeQualifiedExpr ty (Rust.simplePath "from_signed_bytes_be"))
              [Rust.addrOf (Rust.arrExpr [Rust.litExpr (Rust.intLit' Rust.U8 (toInteger b)) | b <- integerToBytes x])]
              
        (smallSuff, meth, smallCases, bigCases) =
          case useI of
            [] -> (Rust.U64, "try_to_unsigned", sortOn fst (useUI ++ useU), sortOn fst useBig)
            _  -> (Rust.I64, "try_to_signed", sortOn fst (useI ++ useUI), sortOn fst (useU ++ useBig))



compileJumpChoice :: FnCtx => VM.VMT -> VM.JumpChoice Core.Pattern -> [Rust.Arm ()]
compileJumpChoice ty (VM.JumpCase opts) =
  [ Rust.matchArm (compilePat ty p) (Rust.blockExpr (compileJumpWithFree k []))
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
    Core.PNum n ->
      case ty of
        VM.TSem (Core.TUInt (Core.TSize x)) -> num False x
        VM.TSem (Core.TSInt (Core.TSize x)) -> num True x
        _ -> panic "compilePath" ["Unexpected type in numeric pattern"]
        where
        num sign x = Rust.litPat (Rust.intLit' (suff sign x) n)
        suff sign x
          | x <= 8  = if sign then Rust.I8  else Rust.U8
          | x <= 16 = if sign then Rust.I16 else Rust.U16
          | x <= 32 = if sign then Rust.I32 else Rust.U32
          | x <= 64 = if sign then Rust.I64 else Rust.U64
          | otherwise = panic "compilePat" ["Unexpected size"]
    Core.PBytes {}  -> panic "compilePat" ["Unexpected PBytes"]
    Core.PCon uc    ->
      case ty of
        VM.TSem (Core.TUser ut) -> Rust.conPat nm [Rust.wildPat | hasData ]
          where
          nm    = Rust.simplePath' [qual, compileConLabel uc]
          unm   = Core.utName ut
          qual  = compileTName (isRecTy ty || Core.tnameBD unm) unm
          hasData =
            case Core.tnameFlav unm of
              Core.TFlavEnum {} -> False
              Core.TFlavUnion ls -> lookup uc ls == Just Core.HasData
              Core.TFlavStruct {} -> panic "compilePat" ["struct"]


        _  -> panic "compilePat" ["Con pat for not TUser"]

    Core.PAny       -> Rust.wildPat

