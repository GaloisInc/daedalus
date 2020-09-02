{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Data.Char
import Numeric
import Text.PrettyPrint as P

import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.Rec(Rec(..))
import Daedalus.VM
import qualified Daedalus.Core as Src


cModule :: Module -> Doc
cModule m = vcat' (map cTypeGroup (mTypes m))


--------------------------------------------------------------------------------

cStmt :: Instr -> CStmt
cStmt instr =
  case instr of
    SetInput e      -> call "p.setInput" [ cExpr e]
    Say x           -> call "p.say"      [ cString x ]
    Output e        -> call "p.output"   [ cExpr e ]
    Notify e        -> call "p.notify"   [ cExpr e ]
    NoteFail        -> call "p.noteFail" []
    GetInput x      -> cVarDecl x (call "p.getInput" [])
    Spawn l x       -> cVarDecl x (call "p.spawn" [ cClo1 l ])
    CallPrim p es x -> "/* XXX cStmt: call primt */"


cExpr :: E -> CExpr
cExpr expr =
  case expr of
    EBlockArg x   -> cArgUse x
    EVar x        -> cVarUse x
    EUnit         -> call "Unit" []
    EBool b       -> if b then "true" else "false"
    EByteArray bs -> cBytes bs
    ENum n ty     -> call f [ integer n ]
      where
      f = case ty of
            Src.TUInt (Src.TSize n)
              | n <= 8  -> "UINT8_C"
              | n <= 16 -> "UINT16_C"
              | n <= 32 -> "UINT32_C"
              | n <= 64 -> "UINT64_C"
              | otherwise -> todo

            Src.TSInt (Src.TSize n)
              | n <= 8  -> "INT8_C"
              | n <= 16 -> "INT16_C"
              | n <= 32 -> "INT32_C"
              | n <= 64 -> "INT64_C"
              | otherwise -> todo

            Src.TInteger -> todo

            _ -> panic "cExpr" [ "Unexpected type for numeric constant"
                               , show (pp ty) ]

    EMapEmpty {} -> todo
    ENothing {}  -> todo

  where
  todo = "/* XXX cExpr:" <+> pp expr <+> "*/"

cClo1 :: JumpPoint -> CExpr
cClo1 (JumpPoint l es) = "/* XXX: closure */"

cVarUse :: BV -> CExpr
cVarUse (BV x _) = "x" P.<> int x

cVarDecl :: BV -> CExpr -> CStmt
cVarDecl v@(BV x t) e = cType t <+> cVarUse v <+> "=" <+> e P.<> semi

cArgUse :: BA -> CExpr
cArgUse (BA x _) = "a" P.<> int x

-- XXX: should be representing some of these with pointers, or copying ok?
cType :: VMT -> CType
cType ty =
  case ty of
    TThreadId -> "ThreadId"
    TSem sty  -> cSemType sty

cSemType :: Src.Type -> Doc
cSemType sty =
  case sty of
    Src.TStream                -> "Input"
    Src.TUInt n                -> "UInt<" <.> cSizeType n <.> ">"
    Src.TSInt n                -> "SInt<" <.> cSizeType n <.> ">"
    Src.TInteger               -> "Integer"
    Src.TBool                  -> "bool"
    Src.TUnit                  -> "Unit"
    Src.TArray t               -> "std::vector<" <+> cSemType t <+> ">"
    Src.TMaybe t               -> "std::optional<" <+> cSemType t <+> ">"
    Src.TMap {}                -> todo
    Src.TBuilder t             -> todo
    Src.TIterator t            -> todo
    Src.TUser ut               -> cTUser ut
    Src.TParam a               -> cTParam a -- can happen in types
  where
  todo = "/* XXX:" <+> pp sty <+> "*/"



cSizeType :: Src.SizeType -> CType {- ish -}
cSizeType ty =
  case ty of
    Src.TSize n      -> integer n
    Src.TSizeParam x -> cTParam x -- in types

--------------------------------------------------------------------------------
-- Types

cLabel :: Src.Label -> Doc
cLabel x = text (Text.unpack x)

-- XXX: module names, namespaces?
cTName :: Src.TName -> CType
cTName t = case Src.tnameAnon t of
             Nothing -> root
             Just i  -> root <.> int i
  where
  root = text (Text.unpack (Src.tnameText t))

cTUser :: Src.UserType -> CType
cTUser t = cTName nm <.> args
  where
  nm   = Src.utName t
  args = case map cSizeType (Src.utNumArgs t) ++
              map cSemType (Src.utTyArgs t) of

           [] -> empty
           xs -> "<" <.> hsep (punctuate comma xs) <.> ">"

cTParam :: Src.TParam -> CType
cTParam (Src.TP n) = "t" <.> int n


-- Note: this does not add the semi at the end, so we can reuse it in `Def`.
cTypeDecl' :: Src.TDecl -> CDecl
cTypeDecl' ty = vcat [ template, kw <+> cTName (Src.tName ty) ]
  where
  kw = "struct"
  template =
    case map intP (Src.tTParamKNumber ty) ++ map tyP (Src.tTParamKValue ty) of
      [] -> empty
      ps -> "template <" <.> fsep (punctuate comma ps) <.> ">"

  intP x = "int" <+> cTParam x
  tyP x  = "typename" <+> cTParam x

cTypeDecl :: Src.TDecl -> CDecl
cTypeDecl ty = cTypeDecl' ty <.> semi

cTypeGroup :: Rec Src.TDecl -> CDecl
cTypeGroup rec =
  case rec of
    NonRec d  -> cTypeDef d
    MutRec ds -> vcat' (map cTypeDecl ds ++ map cTypeDef ds)

cTypeDef :: Src.TDecl -> CDecl
cTypeDef ty = vcat
  [ cTypeDecl' ty <+> "{"
  , nest 2 inner
  , "};"
  ]
  where
  nm = Src.tName ty
  inner =
    case Src.tDef ty of
      Src.TStruct fs -> cStructDef fs
      Src.TUnion fs  -> cSumDef ty fs

cStructDef :: [(Src.Label, Src.Type)] -> CDecl
cStructDef fs =
  vcat [ cType (TSem t) <+> cLabel l <.> semi | (l,t) <- fs ]

cSumDef :: Src.TDecl -> [(Src.Label, Src.Type)] -> CDecl
cSumDef decl fs =
  vcat $
    [ hang "using Data = std::variant<" 2
        (fsep (punctuate comma (map (cType . TSem . snd) fs)) <.> ">;")
    , repTy <+> "data;"
    ] ++ zipWith mkCon [ 0.. ] fs
  where
  nm    = Src.tName decl
  isRec = Src.tnameRec nm
  repTy = if isRec then "std::shared_ptr<Data>" else "Data"
  tu    = Src.UserType
            { Src.utName = nm
            , Src.utNumArgs = map Src.TSizeParam (Src.tTParamKNumber decl)
            , Src.utTyArgs = map Src.TParam (Src.tTParamKValue decl)
            }

  mkCon i (l,t) = vcat
    [ cTUser tu <+> cLabel l <.> parens (cType (TSem t) <+> "x") <+> "{"
    , nest 2 $ "return" <+>
                  "{ .data =" <+> mk <.> parens (tag <.> ", x") <+> "};"
    , "}"
    ]
    where
    tag = "std::in_place_index<" <.> int i <.> ">"
    mk  | isRec     = "std::make_shared<Data>"
        | otherwise = "Data"

--------------------------------------------------------------------------------
type CExpr = Doc
type CStmt = Doc
type CType = Doc
type CDecl = Doc

call :: CExpr -> [CExpr] -> CExpr
call f es = f P.<> parens (fsep (punctuate comma es))

cString :: String -> CExpr
cString = text . show

cBytes :: ByteString -> CExpr
cBytes bs = parens ("std::vector<uint8_t>" <+>
                       braces (fsep (punctuate comma cs)))
  where
  cs = map sh (BS8.unpack bs)
  sh c = if isAscii c && isPrint c && (c /= '\'')
            then text (show c)
            else "0x" P.<> text (showHex (fromEnum c) "")

