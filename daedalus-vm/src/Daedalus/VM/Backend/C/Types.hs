{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Types where

{-
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

import Daedalus.VM.Backend.C.Lang


cType :: VMT -> CType
cType ty =
  case ty of
    TThreadId -> "DDL::ThreadId"
    TSem sty  -> cSemType sty

cSemType :: Src.Type -> Doc
cSemType sty =
  case sty of
    Src.TStream                -> "DDL::Input"
    Src.TUInt n                -> inst "DDL::UInt" [ cSizeType n ]
    Src.TSInt n                -> inst "DDL::SInt" [ cSizeType n ]
    Src.TInteger               -> "DDL::Integer"
    Src.TBool                  -> "bool"
    Src.TUnit                  -> "DDL::Unit"
    Src.TArray t               -> inst "DDL::Array" [ cSemType t ]
    Src.TMaybe t               -> inst "DDL::Optional" [ cSemType t ]
    Src.TMap k v               -> inst "DDL::Map" [ cSemType k, cSemType v ]
    Src.TBuilder t ->
      case t of
        Src.TArray a  -> inst "DDL::ArrayBuilder" [ cSemType a ]
        Src.TMap k v  -> inst "DDL::MapBuilder" [ cSemType k, cSemType v]
        _ -> panic "cSemType" [ "Unexpected iterator type", show (pp t) ]

    Src.TIterator t ->
      case t of
        Src.TArray a  -> inst "DDL::ArrayIterator" [ cSemType a ]
        Src.TMap k v  -> inst "DDL::MapIterator" [ cSemType k, cSemType v]
        _ -> panic "cSemType" [ "Unexpected iterator type", show (pp t) ]
    Src.TUser ut               -> cTUser ut
    Src.TParam a               -> cTParam a -- can happen in types
  where
  todo = "/* XXX:" <+> pp sty <+> "*/"

cSizeType :: Src.SizeType -> CType {- ish -}
cSizeType ty =
  case ty of
    Src.TSize n      -> integer n
    Src.TSizeParam x -> cTParam x -- in types

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
cTypeDecl' ty = vcat [ template, "struct" <+> cTName (Src.tName ty) ]
  where
  template =
    case cTypeParams ty of
      [] -> empty
      ps -> inst "template" ps

cTypeParams :: Src.TDecl -> [Doc]
cTypeParams ty =
  map intP (Src.tTParamKNumber ty) ++ map tyP (Src.tTParamKValue ty)
  where
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
cTypeDef ty = vcat'
  [ vcat
      [ cTypeDecl' ty <+> "{"
      , nest 2 inner
      , "};"
      ]
  , generateHash ty
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


declToType :: Src.TDecl -> Src.UserType
declToType decl =
  Src.UserType
    { Src.utName = Src.tName decl
    , Src.utNumArgs = map Src.TSizeParam (Src.tTParamKNumber decl)
    , Src.utTyArgs = map Src.TParam (Src.tTParamKValue decl)
    }



cSumDef :: Src.TDecl -> [(Src.Label, Src.Type)] -> CDecl
cSumDef decl fs =
  vcat $
    [ "using Data = std::variant<"
    , nest 2 (vcat (punctuate comma (map (cType . TSem . snd) fs)))
    , ">;"
    , repTy <+> "data;"
    ] ++ zipWith mkCon [ 0.. ] fs
  where
  nm    = Src.tName decl
  isRec = Src.tnameRec nm
  repTy = if isRec then inst "std::shared_ptr" ["Data"] else "Data"
  tu    = declToType decl

  mkCon i (l,t) = vcat
    [ cTUser tu <+> cLabel l <.> parens (cType (TSem t) <+> "x") <+> "{"
    , nest 2 $ "return" <+>
                  "{ .data =" <+> mk <.> parens (tag <.> ", x") <+> "};"
    , "}"
    ]
    where
    tag = inst "std::in_place_index" [ int i ]
    mk  | isRec     = inst "std::make_shared" [ "Data" ]
        | otherwise = "Data"


generateHash :: Src.TDecl -> CDecl
generateHash ty =
  vcat
    [ "namespace std {"
    , nest 2 $ vcat
        [ inst "template" (cTypeParams ty)
        , "struct" <+> inst "hash" [ thisTy ] <+> "{"
        , nest 2 $ vcat
            [ "std::size_t operator()(" <.> thisTy <+>
                                                "const& x) const noexcept {"
            , nest 2 hashCode
            , "}"
            ]
        , "};"
        ]
    , "}"
    ]
  where
  thisTy = cTUser (declToType ty)
  hashCode = case Src.tDef ty of
               Src.TUnion {}
                 | Src.tnameRec (Src.tName ty) ->
                    "return" <+> hash (thisTy <.> "::Data") "*x.data" <.> ";"
                 | otherwise ->
                    "return" <+> hash (thisTy <.> "::Data") "x.data" <.> ";"
               Src.TStruct fs ->
                 vcat $
                    "std::size_t h = 17;"
                  : map hashField fs ++
                    [ "return h;" ]
  hash t a = call (inst "std::hash" [t] <.> "{}") [a]
  hashField (l,t) = "h = 23 * h +" <+> hash (cSemType t)
                                      (cSelect "x" (cLabel l)) <.> ";"

-}
