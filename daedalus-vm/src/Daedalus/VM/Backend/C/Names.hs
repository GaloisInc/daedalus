{-# Language OverloadedStrings, BlockArguments #-}
{-# Language ImplicitParams, ConstraintKinds #-}
module Daedalus.VM.Backend.C.Names where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Char(isAlphaNum,isAscii)
import Numeric(showHex)

import Daedalus.PP
import Daedalus.GUID(guidString)

import Daedalus.VM
import qualified Daedalus.Core as Src
import Daedalus.VM.Backend.C.Lang

--------------------------------------------------------------------------------
nsDDL :: Doc
nsDDL = "DDL"

type NSUser = ( ?nsUser :: Doc

              , ?nsInputType :: Doc
                -- Use this type to represent input streams


              , ?nsExternal :: Map Src.MName Doc
                -- external modules with corresponding namespace
              )

-- | The namespace where we should put user definde type that are not external
nsUser :: NSUser => Doc
nsUser = ?nsUser

nsInputType :: NSUser => Doc
nsInputType = ?nsInputType

nsPrivate :: Doc
nsPrivate = "Private"

nsTag :: Doc
nsTag = "Tag"

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
cDebug :: String -> CStmt
cDebug x = cStmt (cCall (nsDDL .:: "debug") [ cString x ] )

cDebugLine :: String -> CStmt
cDebugLine x = cStmt (cCall (nsDDL .:: "debugLine") [ cString x ] )

cDebugNL :: CStmt
cDebugNL = cStmt (cCall (nsDDL .:: "debugNL") [])

cDebugVal :: CExpr -> CStmt
cDebugVal x = cStmt (cCall (nsDDL .:: "debugVal") [ x ])

cDebugValNL :: CExpr -> CStmt
cDebugValNL x = cStmt (cCall (nsDDL .:: "debugValNL") [ x ])
--------------------------------------------------------------------------------




-- These are always local to the main function, so no need for namespace
cReturnClassName :: [VMT] -> Doc
cReturnClassName ts = "Return_" <.> escDoc nm
  where nm = hcat $ punctuate "_" $ map pp ts

-- These are always local to the main function, so no need for namespace
cThreadClassName :: [VMT] -> Doc
cThreadClassName ts = "Thread_" <.> escDoc nm
  where nm = hcat $ punctuate "_" $ map pp ts



-- | Root name of a type, without namesapces
cTNameRoot :: Src.TName -> CType
cTNameRoot t = case Src.tnameAnon t of
             Nothing -> root
             Just i  -> root <.> int i
  where
  root = text (pref ++ Text.unpack (Src.tnameText t))
  txt  = Src.tnameText t
  pref = if isReserved txt then "_" else ""



-- | The name of the underlying type used by a boxed type.
cTNameUse :: NSUser => GenVis -> Src.TName -> CType
cTNameUse vis x =
  case vis of
    GenPublic  -> ns .:: cTNameRoot x
    GenPrivate -> ns .:: nsPrivate .:: cTNameRoot x
  where
  ns = Map.findWithDefault nsUser (Src.tnameMod x) ?nsExternal


data GenVis = GenPublic | GenPrivate
data GenOwn = GenBorrow | GenOwn


-- | Name of a type parameter.
cTParam :: Src.TParam -> CIdent
cTParam (Src.TP n) = "T" <.> int n

-- | A C identifier corresponding to a source lable.
cLabel :: Src.Label -> CIdent
cLabel x = pref <.> escText x
  where
  pref = if isReserved x then "_" else ""

-- | The name of a field in struct/union
cField :: Int -> Doc
cField n = "_" <.> int n

cBlockLabel :: Label -> CIdent
cBlockLabel (Label f x) = escDoc (pp f) <.> "_" <.> int x

cVarUse :: BV -> CIdent
cVarUse = pp

cArgUse :: Block -> BA -> CIdent
cArgUse b a = cBlockLabel (blockName b) <.> "_" <.> pp a

cRetVar :: VMT -> CIdent
cRetVar ty = "ret_" <.> escDoc (pp ty)

cRetInput :: CIdent
cRetInput = "input_ret"

cRetVarFun :: CIdent
cRetVarFun = "_result"

cRetInputFun :: CIdent
cRetInputFun = "_result_input"


escDoc :: Doc -> Doc
escDoc = escString . show

escText :: Text -> Doc
escText = escString . Text.unpack

escText' :: Text -> Text
escText' = Text.pack . escString' . Text.unpack

escString :: String -> Doc
escString = text . escString'

escString' :: String -> String
escString' = concatMap esc
  where
  esc c
    | c == 'z'                              = "zz"
    | c == '_' || isAlphaNum c && isAscii c = [c]
    | otherwise                             = "z" ++ showHex (fromEnum c) "z"



unionCon :: Src.Label -> CIdent
unionCon l = "init_" <.> cLabel l

structCon :: CIdent
structCon = "init"

selName :: GenOwn -> Src.Label -> CIdent
selName own l = pref <.> "_" <.> cLabel l
  where pref = case own of
                 GenBorrow -> "borrow"
                 GenOwn    -> "get"



cFNameWithPref :: Text -> FName -> CIdent
cFNameWithPref pref f =
  escText (if Src.fnamePublic f
             then stem
             else stem <> "_" <> Text.pack (guidString (Src.fnameId f)))
  where stem = pref <> Src.fnameText f

cFName :: FName -> CIdent
cFName = cFNameWithPref "parser_"

-- | This is the initializer function for global variables
cFNameInit :: FName -> CIdent
cFNameInit = cFNameWithPref "init_"

cFEntryName :: FName -> CIdent
cFEntryName f = escText ("parse" <> Src.fnameText f)



--------------------------------------------------------------------------------
isReserved :: Text -> Bool
isReserved x = x `Set.member` reservedSet

reservedSet :: Set Text
reservedSet = Set.fromList
  [ "alignas"
  , "alignof"
  , "and"
  , "and_eq"
  , "asm"
  , "atomic_cancel"
  , "atomic_commit"
  , "atomic_noexcept"
  , "auto"
  , "bitand"
  , "bitor"
  , "bool"
  , "break"
  , "case"
  , "catch"
  , "char"
  , "char8_t"
  , "char16_t"
  , "char32_t"
  , "class"
  , "compl"
  , "concept"
  , "const"
  , "consteval"
  , "constexpr"
  , "constinit"
  , "const_cast"
  , "continue"
  , "co_await"
  , "co_return"
  , "co_yield"
  , "decltype"
  , "default"
  , "delete"
  , "do"
  , "double"
  , "dynamic_cast"
  , "else"
  , "enum"
  , "explicit"
  , "export"
  , "extern"
  , "false"
  , "float"
  , "for"
  , "friend"
  , "goto"
  , "if"
  , "inline"
  , "int"
  , "long"
  , "mutable"
  , "namespace"
  , "new"
  , "noexcept"
  , "not"
  , "not_eq"
  , "nullptr"
  , "operator"
  , "or"
  , "or_eq"
  , "private"
  , "protected"
  , "public"
  , "reflexpr"
  , "register"
  , "reinterpret_cast"
  , "requires"
  , "return"
  , "short"
  , "signed"
  , "sizeof"
  , "static"
  , "static_assert"
  , "static_cast"
  , "struct"
  , "switch"
  , "synchronized"
  , "template"
  , "this"
  , "thread_local"
  , "throw"
  , "true"
  , "try"
  , "typedef"
  , "typeid"
  , "typename"
  , "union"
  , "unsigned"
  , "using"
  , "virtual"
  , "void"
  , "volatile"
  , "wchar_t"
  , "while"
  , "xor"
  , "xor_eq"
  ]
