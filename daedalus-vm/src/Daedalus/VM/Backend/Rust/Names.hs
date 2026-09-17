module Daedalus.VM.Backend.Rust.Names where

import Data.Char(isAlphaNum,isAscii)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Numeric(showHex)

import Daedalus.GUID(guidString)
import Daedalus.Panic(panic)
import Daedalus.Core qualified as Core
import Daedalus.VM qualified as VM
import Daedalus.VM.Backend.Rust.Lang qualified as Rust


ddlModName :: Rust.Ident
ddlModName = "ddl"

ddlPath :: Rust.Ident -> Rust.Path ()
ddlPath f = Rust.simplePath' [ddlModName,f]

type ExternalTypes = Map Core.MName (Rust.Path ())

-- Types from modules named by `--extern` are defined in another Rust module.
-- Build either that qualified path or the usual local type name.
compileTPath :: (?externalTypes :: ExternalTypes) =>
                Bool -> Core.TName -> Rust.Path ()
compileTPath isPriv ty =
  case Map.lookup (Core.tnameMod ty) ?externalTypes of
    Nothing        -> Rust.simplePath (compileTName isPriv ty)
    Just namespace -> appendPath namespace (compileTName isPriv ty)

appendPath :: Rust.Path () -> Rust.Ident -> Rust.Path ()
appendPath (Rust.Path global segments ()) name =
  Rust.Path global (segments ++ [Rust.PathSegment name Nothing ()]) ()

-- Attach generic type arguments to the final segment of a qualified path.
addPathTypes :: Rust.Path () -> [Rust.Ty ()] -> Rust.Path ()
addPathTypes path [] = path
addPathTypes (Rust.Path global segments ()) tys =
  case reverse segments of
    [] -> panic "addPathTypes" ["empty path"]
    Rust.PathSegment name Nothing () : more ->
      Rust.Path global
        (reverse more ++
          [ Rust.PathSegment name
              (Just (Rust.AngleBracketed (map Rust.TypeArg tys) [] ())) ()
          ])
        ()
    _ -> panic "addPathTypes" ["path already has generic arguments"]

-- XXX: collisions
compileFName :: Core.FName -> Rust.Ident
compileFName f = Rust.mkIdent (txt ++ suff)
  where
  suff = if Core.fnamePublic f then "" else "_" ++ uid
  uid = guidString (Core.fnameId f)
  txt = Rust.snakeCase (escapeFName (Text.unpack (Core.fnameText f)))

-- Named byte classes have names such as `$lf`.  Give these a readable Rust
-- name, so `$lf` becomes `byte_class_lf`.  Encode any other characters which
-- are not valid in Rust identifiers using the convention employed by the C
-- backend.
escapeFName :: String -> String
escapeFName name =
  case name of
    '$' : more -> "byte_class_" ++ escapeChars more
    _          -> escapeChars name
  where
  escapeChars = concatMap escape

  escape c
    | c == '_' || isAlphaNum c && isAscii c = [c]
    | otherwise                            = "z" ++ showHex (fromEnum c) "z"

compileRecEntryTypeName :: Core.FName -> Rust.Ident
compileRecEntryTypeName f =
  Rust.mkIdent ("__CallRec" ++ guidString (Core.fnameId f))
  -- `compileTName` cannot produce the leading double underscore.

compileRecEntryConName :: Core.FName -> Rust.Ident
compileRecEntryConName f =
  Rust.mkIdent
    (Rust.upperCamelCase (escapeFName (Text.unpack (Core.fnameText f))) ++
     guidString (Core.fnameId f))

compileRecFunName :: Core.FName -> Rust.Ident
compileRecFunName f =
  Rust.mkIdent ("__call_rec" ++ guidString (Core.fnameId f))

compileRecFrameConName :: Core.FName -> Rust.Ident
compileRecFrameConName f =
  Rust.mkIdent
    ("Done" ++
     Rust.upperCamelCase (escapeFName (Text.unpack (Core.fnameText f))) ++
     guidString (Core.fnameId f))

compileRecCallFrameConName :: VM.Label -> Core.FName -> Rust.Ident
compileRecCallFrameConName (VM.Label caller n) callee =
  Rust.mkIdent
    (Rust.upperCamelCase (escapeFName (Text.unpack caller)) ++
     Rust.upperCamelCase
       (escapeFName (Text.unpack (Core.fnameText callee))) ++
     show n)

recFrameTypeName :: Rust.Ident
recFrameTypeName = "Frame"

recStackName :: Rust.Ident
recStackName = "__stack"

compileBAName :: VM.BA -> Rust.Ident
compileBAName (VM.BA n _ _) = Rust.mkIdent ("_arg_" ++ show n)

compileBVName :: VM.BV -> Rust.Ident
compileBVName (VM.BV n _) = Rust.mkIdent ("_tmp_" ++ show n)

pcName :: Rust.Ident
pcName = "block_id"

parserStateName :: Rust.Ident
parserStateName = "_state"

compileBlockLabel :: VM.Label -> Rust.Ident
compileBlockLabel (VM.Label txt n) =
  Rust.mkIdent
    (Rust.upperCamelCase (escapeFName (Text.unpack txt)) ++ "B" ++ show n)

-- XXX: Name collisions
compileTName :: Bool -> Core.TName -> Rust.Ident
compileTName isPriv x = Rust.mkIdent (pref ++ root ++ suff)
    where pref = if isPriv then "_" else ""
          root = Rust.upperCamelCase (Text.unpack (Core.tnameText x))
          suff = case Core.tnameAnon x of
                   Nothing -> ""
                   Just i  -> show i

compileFieldLabel :: Core.Label -> Rust.Ident
compileFieldLabel l
  | s `elem` rustKeywords = (Rust.mkIdent s) { Rust.raw = True }
  | otherwise             = Rust.mkIdent s
  where s = Rust.snakeCase (Text.unpack l)

rustKeywords :: [String]
rustKeywords = words "as async await box break const continue crate do dyn else enum \
  \extern false fn for gen if impl in let loop macro match mod move mut \
  \pub ref return self static struct super trait true try type \
  \unsafe use where while yield abstract become final override priv \
  \proc typeof unsized virtual"

compileBDFieldLabel :: Core.Label -> Rust.Ident
compileBDFieldLabel l = Rust.mkIdent ("get_" <> Rust.snakeCase (Text.unpack l))
  
compileConLabel :: Core.Label -> Rust.Ident
compileConLabel l = Rust.mkIdent (Rust.upperCamelCase (Text.unpack l))

bdJunkName :: Rust.Ident
bdJunkName = "Junk"

valTPName :: Core.TParam -> Rust.Ident
valTPName (Core.TP n) = Rust.mkIdent ("T" ++ show n)

numTPName :: Core.TParam -> Rust.Ident
numTPName (Core.TP n) = Rust.mkIdent ("N" ++ show n)

contTypeName :: Rust.Ident
contTypeName = "Goto"

intDecisionTreeDfltName :: String
intDecisionTreeDfltName = "_int_case"

funLoopName :: String
funLoopName = "_fun_loop"
