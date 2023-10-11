module AST where

import Data.Text(Text)

data ModName  = ModName [Ident] Ident
data Name     = Name (Maybe ModName) Ident

data IdentT   = Prefix | Infix
  deriving Eq

data Ident    = Ident IdentT Text

data Module   = Module ModName (Maybe [Name]) [Import] [Decl]
data Import   = Import ModName (Maybe ModName)

data TopDecl  = DType  TypeDecl
              | DValue Decl

data Type     = TCon Name [Type]
              | TFun Type Type
              | TQual [Type] Type
              | TTuple [Type]
              | TVar Ident

data Param    = Param Ident (Maybe Type)

data TypeDecl = TypeDecl Ident [Param] [Con]
data Con      = Con Ident [Type]

data Decl     = Decl Ident [Param] Type [Param] Expr

data Expr     = Var Name [Type] [Expr]
              | Op Expr Name Expr
              | Let [Decl] Expr
              | Case Expr [Alt]
              | If Expr Expr Expr
              | Do [Stmt] Expr
              | Parens Expr

data Alt      = Alt Pat Expr
data Pat      = PVar Param
              | PCon Name [Pat]
              | PWild

data Stmt     = Stmt (Maybe Pat) Expr



