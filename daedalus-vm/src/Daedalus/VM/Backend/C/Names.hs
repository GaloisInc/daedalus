{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Names where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Set(Set)
import qualified Data.Set as Set

import Daedalus.PP

import Daedalus.VM
import qualified Daedalus.Core as Src
import Daedalus.VM.Backend.C.Lang



cReturnClassName :: JumpPoint -> Doc
cReturnClassName j = "Return_" <.> pp (jLabel j)

-- | Name of a type.
-- XXX: module names, namespaces?
cTName :: Src.TName -> CType
cTName t = case Src.tnameAnon t of
             Nothing -> root
             Just i  -> root <.> int i
  where
  root = text (pref ++ Text.unpack (Src.tnameText t))
  txt  = Src.tnameText t
  pref = if isReserved txt then "_" else ""

-- | The name of the underlying type used by a boxed type.
cTName' :: GenVis -> Src.TName -> CType
cTName' vis x =
  case vis of
    GenPublic -> cTName x
    GenPrivate -> "_" <.> cTName x


data GenVis = GenPublic | GenPrivate


-- | Name of a type parameter.
cTParam :: Src.TParam -> CType
cTParam (Src.TP n) = "T" <.> int n

-- | A C identifier corresponding to a source lable.
cLabel :: Src.Label -> Doc
cLabel x = text (pref ++ Text.unpack x)
  where
  pref = if isReserved x then "_" else ""

-- | The name of a field in struct/union
cField :: Int -> Doc
cField n = "_" <.> int n


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
