{-# Language ImportQualifiedPost, OverloadedStrings, ImplicitParams, ConstraintKinds #-}
module Daedalus.VM.Backend.Rust (
  compileProgram,
  Config(..)
) where

import Data.Text qualified as Text
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List(foldl')
import Data.ByteString qualified as BS
import Control.Exception

import Daedalus.PP
import Daedalus.GUID(guidString)
import Daedalus.Panic(panic)
import Daedalus.Core qualified as Core
import Daedalus.VM qualified as VM
import Daedalus.VM.Backend.Rust.Lang qualified as Rust


newtype Config = Config {
  cfgUserModule :: String
}

newtype Unsupported = Unsupported Doc deriving Show
instance Exception Unsupported

unsupported :: Doc -> a
unsupported x = throw (Unsupported x)

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
    Rust.use' unusedOk (Rust.useSelect (Rust.simplePath "ddl") [ Rust.useOne x Nothing | x <- map Rust.simplePath [ "Type", "Clonable" ] ])
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
compileModule m = map compileFun (VM.mFuns m) -- XXX: type declarations
  
compileFun :: ProgCtx => VM.VMFun -> Rust.Item ()
compileFun fu =
  case VM.vmfCaptures fu of
    VM.Capture   -> unsupported (fnMsg <+> "captures the stack")
    VM.Unknown   -> panic "compileFun" [ show (pp fnm), "`Unknwon` capture" ]
    VM.NoCapture ->
      Rust.mkFnItem Nothing [] [] nm Rust.noGenerics args resT def
  where
  fnMsg           = backticks (pp fnm)
  fnm             = VM.vmfName fu
  nm              = compileFName fnm
  resT
    | VM.vmfPure fu = compileType VM.Owned (Core.fnameType fnm)
    | otherwise     = Rust.tOption (Rust.tTuple [compileType VM.Owned (Core.fnameType fnm), compileType VM.Owned Core.TStream])

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

  contT     = Rust.itemStmt (Rust.mkEnum contTypeName Rust.noGenerics
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
      [ Rust.localLet [] (compileBVName x) Nothing (Rust.callMethod (compileExpr VM.Borrowed e) "cloned" [])]
    VM.Free _                   -> [] -- Rust should drop things on its own
    VM.NoteFail loc str inp msg -> [] -- XXX: error messages
    VM.PushDebug {}             -> [] -- XXX: stack trace
    VM.PopDebug                 -> [] -- XXX
  where
  bad     = panic "compileBlockInstr" ["Unexpected instruction", show (pp instr)]
  notYet  = unsupported (?fnMsg <+> "instruction:" <+> pp instr)



ddlPath :: Rust.Ident -> Rust.Path ()
ddlPath f = Rust.simplePath' [ddlModName,f]

compilePrim :: FnCtx => VM.BV -> VM.PrimName -> [VM.E] -> [Rust.Stmt ()]
compilePrim x prim es =
  case prim of
    VM.ByteArray bs ->
      def ty (Rust.call (Rust.pathExpr (ddlPath "new_array_slice")) [Rust.litExpr (Rust.bytesLit bs)])
        where ty = if BS.null bs then Just (compileType VM.Owned (Core.TArray Core.TByte)) else Nothing

    VM.NewBuilder ty -> 
      def (Just (compileType VM.Owned ty)) (Rust.call (Rust.pathExpr (ddlPath "new_builder")) [])

    VM.Integer i -> xxx

    VM.StructCon ut -> xxx
    
    VM.Op1 op ->
      case es of
        [e1] -> compileOp1 x op e1
        _    -> bad 1

    VM.Op2 op ->
      case es of
        [e1,e2] -> compileOp2 x op e1 e2
        _       -> bad 2

    VM.Op3 op ->
      case es of
        [e1,e2,e3] -> compileOp3 x op e1 e2 e3
        _          -> bad 3

    VM.OpN op -> compileOpN x op es
  
  where
  bad n = panic "compilePrim" ["Expected " ++ show (n::Int) ++ "argument, but have " ++ show (length es), show (pp prim) ]
  def mb re = [Rust.localLet [] (compileBVName x) mb re]
  tmp = show (pp prim <> parens (commaSep (map pp es)))
  xxx =
    [ Rust.localLet [] (compileBVName x) (Just (compileVMT (VM.getType x) VM.Owned))
      (Rust.call (Rust.identExpr "todo") [Rust.litExpr (Rust.strLit tmp)])
    ]

compileOp1 :: FnCtx => VM.BV -> Core.Op1 -> VM.E -> [Rust.Stmt ()]
compileOp1 x op e =
  case op of
    Core.Head -> def (Rust.callMethod (compileExpr VM.Borrowed e) "head" [])
    Core.IsEmptyStream -> def (Rust.callMethod (compileExpr VM.Borrowed e) "is_empty" [])
    
    Core.CoerceTo ty -> xxx
    Core.StreamOffset -> xxx
    Core.BytesOfStream -> xxx
    Core.OneOf bs -> xxx
    Core.Neg -> xxx
    Core.BitNot -> xxx
    Core.Not -> xxx
    Core.ArrayLen -> xxx
    Core.Concat -> xxx
    Core.FinishBuilder -> xxx
    Core.NewIterator -> xxx
    Core.IteratorDone -> xxx
    Core.IteratorKey -> xxx
    Core.IteratorVal -> xxx
    Core.IteratorNext -> xxx
    Core.EJust -> xxx
    Core.FromJust -> xxx
    Core.SelStruct ty lab -> xxx
    Core.InUnion ut lab -> xxx
    Core.FromUnion ty lab -> xxx
    Core.WordToFloat -> xxx
    Core.WordToDouble -> xxx
    Core.IsNaN -> xxx
    Core.IsInfinite -> xxx
    Core.IsDenormalized -> xxx
    Core.IsNegativeZero -> xxx
    
  where
  def re = [Rust.localLet [] (compileBVName x) Nothing re]
  tmp = show (pp op <> parens (commaSep [pp e]))
  xxx =
    [ Rust.localLet [] (compileBVName x) (Just (compileVMT (VM.getType x) VM.Owned))
      (Rust.call (Rust.identExpr "todo") [Rust.litExpr (Rust.strLit tmp)])
    ]


compileOp2 :: FnCtx => VM.BV -> Core.Op2 -> VM.E -> VM.E -> [Rust.Stmt ()]
compileOp2 x op e1 e2 =
  case op of
    Core.Drop ->
      def (Rust.callMethod (compileExpr VM.Owned e2) "advance" [ compileSize e1 ])

    Core.IsPrefix -> xxx
  
    Core.DropMaybe -> xxx
    Core.Take -> xxx
 
    Core.Eq -> xxx
    Core.NotEq -> xxx
    Core.Leq -> xxx
    Core.Lt -> xxx
 
    Core.Add -> xxx
    Core.Sub -> xxx
    Core.Mul -> xxx
    Core.Div -> xxx
    Core.Mod -> xxx
 
    Core.BitAnd -> xxx
    Core.BitOr -> xxx
    Core.BitXor -> xxx
    Core.Cat -> xxx
    Core.LCat -> xxx
    Core.LShift -> xxx
    Core.RShift -> xxx
 
    Core.ArrayIndex -> xxx
    Core.Emit -> xxx
    Core.EmitArray -> xxx
    Core.EmitBuilder -> xxx
    Core.MapLookup -> xxx
    Core.MapMember -> xxx
 
    Core.ArrayStream -> xxx
    
  where
  def e = [Rust.localLet [] (compileBVName x) Nothing e]
  tmp = show (pp op <> parens (commaSep [pp e1, pp e2]))
  xxx =
    [ Rust.localLet [] (compileBVName x) (Just (compileVMT (VM.getType x) VM.Owned))
      (Rust.call (Rust.identExpr "todo") [Rust.litExpr (Rust.strLit tmp)])
    ]


compileOp3 :: FnCtx => VM.BV -> Core.Op3 -> VM.E -> VM.E -> VM.E -> [Rust.Stmt ()]
compileOp3 x op e1 e2 e3 =
  case op of
    Core.RangeUp -> xxx
    Core.RangeDown -> xxx
    Core.MapInsert -> xxx
  where
  def e = [Rust.localLet [] (compileBVName x) Nothing e]
  tmp = show (pp op <> parens (commaSep [pp e1, pp e2, pp e3]))
  xxx =
    [ Rust.localLet [] (compileBVName x) (Just (compileVMT (VM.getType x) VM.Owned))
      (Rust.call (Rust.identExpr "todo") [Rust.litExpr (Rust.strLit tmp)])
    ]

compileOpN :: FnCtx => VM.BV -> Core.OpN -> [VM.E] -> [Rust.Stmt ()]
compileOpN x op es =
  case op of
    Core.ArrayL t ->
      def (if null es then Just (compileType VM.Owned t) else Nothing)
          (Rust.call (Rust.pathExpr (ddlPath "new_array"))
              [Rust.arrExpr (map (compileExpr VM.Owned) es)]) 
    Core.CallF {} -> panic "compileOpN" ["Unexpected CallF"]
  where
  def mbT e = [Rust.localLet [] (compileBVName x) mbT e]

compileSize :: FnCtx => VM.E -> Rust.Expr ()
compileSize e = Rust.cast (compileExpr VM.Owned e) Rust.tUsize

compileExpr :: FnCtx => VM.Ownership -> VM.E -> Rust.Expr ()
compileExpr how expr =
  case expr of
    VM.EUnit         -> Rust.tupleExpr []
    VM.ENum n ty     -> Rust.litExpr (Rust.intLit n) --- XXX: suffix? non-standard sizes
    VM.EBool b       -> Rust.litExpr (Rust.boolLit b)
    VM.EFloat d ty   -> Rust.litExpr (Rust.floatLit d)
    VM.EMapEmpty k v -> unsupported (?fnMsg <+> "empty map expression") -- XXX
    VM.ENothing {}   -> mbBorrow VM.Owned (Rust.identExpr "None") -- type sig?
    VM.EBlockArg x   -> mbBorrow (VM.getOwnership x) (Rust.identExpr (compileBAName x))
    VM.EVar x        -> mbBorrow (VM.getOwnership x) (Rust.identExpr (compileBVName x))
  where
  mbBorrow own e =
    case (how, own) of
      (VM.Borrowed, VM.Owned) -> Rust.callMethod e "borrowed" []
      _ -> e


compileCInstr :: FnCtx => VM.CInstr -> [Rust.Stmt ()]
compileCInstr cinstr =
  case cinstr of
    VM.Jump jp -> compileJump jp []
    VM.JumpIf e opts ->
      [Rust.expr (Rust.matchExpr (compileExpr VM.Borrowed e) (compileJumpChoice opts))]
    VM.Yield             -> bad
    VM.ReturnNo          -> [Rust.ret (Rust.identExpr "None")]
    VM.ReturnYes res inp -> [Rust.ret (Rust.call (Rust.identExpr "Some") [Rust.tupleExpr (map (compileExpr VM.Owned) [res,inp])])]
    VM.ReturnPure res    -> [Rust.ret (compileExpr VM.Owned res)]
    VM.CallPure f j es   -> compileJumpWithFree j [doCall f es]
    VM.CallNoCapture f (VM.JumpCase opts) es ->
      [ Rust.expr (Rust.matchExpr (doCall f es) 
          [ Rust.matchArm (Rust.somePat (Rust.identPat "x")) (opt True [Rust.identExpr "x"]),
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
    (Rust.call (Rust.pathExpr (Rust.simplePath' [contTypeName, compileBlockLabel l]))
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

compileJumpChoice :: FnCtx => VM.JumpChoice Core.Pattern -> [Rust.Arm ()]
compileJumpChoice (VM.JumpCase opts) =
  [ Rust.matchArm (compilePat p) (Rust.blockExpr (compileJumpWithFree k []))
  | (p,k) <- Map.toList opts
  ]


compilePat :: Core.Pattern -> Rust.Pat ()
compilePat p =
  case p of
    Core.PBool b    -> Rust.litPat (Rust.boolLit b)
    Core.PNothing   -> Rust.nonePat
    Core.PJust      -> Rust.somePat Rust.wildPat
    Core.PNum n     -> Rust.litPat (Rust.intLit n)
    Core.PBytes bs  -> xxx
    Core.PCon uc    -> xxx
    Core.PAny       -> Rust.wildPat
  where
  xxx = unsupported (pp p)



--------------------------------------------------------------------------------             
-- Names
--------------------------------------------------------------------------------             

ddlModName :: Rust.Ident
ddlModName = "ddl"

compileFName :: Core.FName -> Rust.Ident
compileFName f = Rust.mkIdent (txt ++ "_" ++ uid)
  where
  uid = guidString (Core.fnameId f)
  txt = Rust.snakeCase (Text.unpack (Core.fnameText f))

compileBAName :: VM.BA -> Rust.Ident
compileBAName (VM.BA n _ _) = Rust.mkIdent ("_arg_" ++ show n)

compileBVName :: VM.BV -> Rust.Ident
compileBVName (VM.BV n _) = Rust.mkIdent ("_tmp_" ++ show n)

pcName :: Rust.Ident
pcName = Rust.mkIdent "block_id"

compileBlockLabel :: VM.Label -> Rust.Ident
compileBlockLabel (VM.Label txt n) = Rust.mkIdent (Text.unpack txt ++ show n)

contTypeName :: Rust.Ident
contTypeName = "Cont"

--------------------------------------------------------------------------------

compileVMT :: FnCtx => VM.VMT -> VM.Ownership -> Rust.Ty ()
compileVMT ty own =
  case ty of
    VM.TSem t     -> compileType own t
    VM.TThreadId  -> unsupported (?fnMsg <+> "ThreadId type")


-- | Compile a type in its owned form.
compileType :: VM.Ownership -> Core.Type -> Rust.Ty ()
compileType own ty =
  case ty of
    Core.TStream -> maybeRef (Rust.pathType (ddlPath "Input"))

    Core.TUInt sz ->
      case sz of
        Core.TSize n
          | n `elem` [8,16,32,64] -> Rust.tU n
          | otherwise             -> xxx
        Core.TSizeParam tp -> xxx

    Core.TSInt sz ->
      case sz of
        Core.TSize n
          | n `elem` [8,16,32,64] -> Rust.tI n
          | otherwise             -> xxx
        Core.TSizeParam tp -> xxx

    Core.TInteger           -> xxx
    Core.TBool              -> Rust.tBool
    Core.TFloat             -> Rust.tF 32
    Core.TDouble            -> Rust.tF 64
    Core.TUnit              -> Rust.tTuple []
    Core.TArray t           -> maybeB "Array" "ArrayB" t
    Core.TBuilder t         -> maybeB "Builder" "BuilderB" t

    Core.TMaybe t           -> maybeRef (Rust.tOption (compileType VM.Owned t))
    Core.TMap tk kv         -> xxx
    Core.TIterator t        -> xxx
    Core.TUser t            -> xxx
    Core.TParam bp          -> xxx
  where
  xxx = error ("XXX: " ++ show (pp ty))
  maybeRef rt =
    case own of
      VM.Borrowed -> Rust.tRef Nothing rt
      _           -> rt
  maybeB town tbor t = Rust.pathType p
   where
    p = Rust.pathWithTypes [ddlModName,base] [compileType VM.Owned t]
    base =
      case own of
        VM.Borrowed -> tbor
        _           -> town

compileTParam :: Core.TParam -> Rust.Ty ()
compileTParam = undefined