{-# Language ImportQualifiedPost, OverloadedStrings #-}
module Daedalus.VM.Backend.Rust.Names where

import Data.Text qualified as Text

import Daedalus.GUID(guidString)
import Daedalus.Core qualified as Core
import Daedalus.VM qualified as VM
import Daedalus.VM.Backend.Rust.Lang qualified as Rust


ddlModName :: Rust.Ident
ddlModName = "ddl"

ddlPath :: Rust.Ident -> Rust.Path ()
ddlPath f = Rust.simplePath' [ddlModName,f]

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
compileBlockLabel (VM.Label txt n) = Rust.mkIdent (Rust.upperCamelCase (Text.unpack txt ++ show n))

-- XXX: Name collisions
compileTName :: Bool -> Core.TName -> Rust.Ident
compileTName isPriv x = Rust.mkIdent (pref ++ Rust.upperCamelCase (Text.unpack (Core.tnameText x)))
    where pref = if isPriv then "_" else ""

compileFieldLabel :: Core.Label -> Rust.Ident
compileFieldLabel l = Rust.mkIdent (Rust.snakeCase (Text.unpack l))

compileConLabel :: Core.Label -> Rust.Ident
compileConLabel l = Rust.mkIdent (Rust.upperCamelCase (Text.unpack l))

valTPName :: Core.TParam -> Rust.Ident
valTPName (Core.TP n) = Rust.mkIdent ("T" ++ show n)

contTypeName :: Rust.Ident
contTypeName = "Cont"

