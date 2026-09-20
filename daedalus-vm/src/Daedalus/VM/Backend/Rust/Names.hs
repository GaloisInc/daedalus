module Daedalus.VM.Backend.Rust.Names where

import Data.Char(isAlphaNum,isAscii)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
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
compileTPath isRepr ty =
  case Map.lookup (Core.tnameMod ty) ?externalTypes of
    Nothing        -> Rust.simplePath (compileTName isRepr ty)
    Just namespace -> appendPath namespace (compileTName isRepr ty)

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

compileFName :: Core.FName -> Rust.Ident
compileFName f
  | Core.fnamePublic f = compileSourceFName txt
  | otherwise =
      compileGeneratedIdent
        (Text.pack (escapeFName (Text.unpack txt) ++ "_" ++ uid))
  where
  uid = guidString (Core.fnameId f)
  txt = Core.fnameText f

-- Preserve source-level spelling whenever it is a Rust identifier.  Byte
-- classes need an encoding because their Daedalus names start with `$`.
compileSourceFName :: Text.Text -> Rust.Ident
compileSourceFName name =
  case Text.uncons name of
    Just ('$', more) -> compileGeneratedIdent ("class_" <> more)
    _                -> compileSourceIdent name

-- Daedalus identifiers start with a letter, so leading `_` is reserved for
-- names introduced or encoded by the backend.
compileGeneratedIdent :: Text.Text -> Rust.Ident
compileGeneratedIdent name = Rust.mkIdent (Text.unpack (Text.cons '_' name))

compileSourceIdent :: Text.Text -> Rust.Ident
compileSourceIdent name
  | name `Set.member` rawIdentifierExceptions =
      compileGeneratedIdent name
  | name `Set.member` rustKeywords =
      (Rust.mkIdent (Text.unpack name)) { Rust.raw = True }
  | otherwise = Rust.mkIdent (Text.unpack name)

-- Rust does not permit these names even as raw identifiers.
rawIdentifierExceptions :: Set Text.Text
rawIdentifierExceptions =
  Set.fromList (map Text.pack ["crate", "self", "super", "Self"])

-- Encode characters which are not valid in Rust identifiers using the
-- convention employed by the C backend.  This is used for private and
-- internal names; public byte classes are handled by `compileSourceFName`.
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

compileTName :: Bool -> Core.TName -> Rust.Ident
compileTName isRepr x =
  case (isRepr, Core.tnameAnon x) of
    (False, Nothing) -> compileSourceIdent root
    (True,  Nothing) -> compileGeneratedIdent ("repr_" <> root)
    (False, Just i)  -> compileGeneratedIdent
                          ("anon_" <> root <> "_" <> Text.pack (show i))
    (True,  Just i)  -> compileGeneratedIdent
                          ("repr_anon_" <> root <> "_" <> Text.pack (show i))
  where
  root = Core.tnameText x

compileFieldLabel :: Core.Label -> Rust.Ident
compileFieldLabel = compileSourceIdent

rustKeywords :: Set Text.Text
rustKeywords = Set.fromList (Text.words (Text.pack
  "as async await box break const continue crate do dyn else enum \
  \extern false fn for gen if impl in let loop macro match mod move mut \
  \pub ref return self static struct super trait true try type \
  \unsafe use where while yield abstract become final override priv \
  \proc typeof unsized virtual"))

compileBDFieldLabel :: Core.Label -> Rust.Ident
compileBDFieldLabel l = Rust.mkIdent ("get_" <> Text.unpack l)
  
compileConLabel :: Core.Label -> Rust.Ident
compileConLabel = compileSourceIdent

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
