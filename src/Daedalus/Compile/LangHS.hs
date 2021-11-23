{-# Language RecordWildCards, OverloadedStrings #-}
{-# Language ExistentialQuantification #-}
module Daedalus.Compile.LangHS where

import Data.String
import Daedalus.PP

data Module = Module
  { hsModName  :: String
  , hsLangExts :: [String]
  , hsGHC      :: [String] -- GHC optiosn
  , hsImports  :: [Import]
  , hsDecls    :: [Decl]
  }

data Decl = DataDecl DataDecl
          | FunDecl FunDecl
          | InstanceDecl InstanceDecl
          | DerivingDecl DerivingDecl
          | SigDecl SigDecl


data Import          = Import String ImportQualifier
data ImportQualifier = Unqualified | Qualified | QualifyAs String


class HsDecl a where
  declare :: a -> Decl

instance HsDecl FunDecl       where declare = FunDecl
instance HsDecl DataDecl      where declare = DataDecl
instance HsDecl InstanceDecl  where declare = InstanceDecl
instance HsDecl DerivingDecl  where declare = DerivingDecl
instance HsDecl SigDecl       where declare = SigDecl


data SigDecl = Sig
  { sigName :: [Term]
  , sigType :: Qual
  }

data FunDecl = Fun
  { funNS  :: FunNS
  , funLHS :: Term
  , funDef :: Term
  }

data FunNS = TypeFun | ValueFun

data DataDecl = Data { dataLHS :: Term
                     , dataCons :: [Term]
                     }

data InstanceDecl = Instance
  { instOverlaps  :: Overlaps
  , instAsmps     :: [Term]
  , instHead      :: Term
  , instMethods   :: [FunDecl]
  }

data Overlaps = Normal | Overlaps

data DerivingDecl = Deriving
  { deriveAsmps :: [Term]
  , deriveHead  :: Term
  }

instance IsString Term where
  fromString = Var

data Term = Ap Term Term
          | ApI String Term Term
          | Var String
          | Tuple [Term]
          | List [Term]
          | Do (Maybe Term) Term Term
          | If Term Term Term
          | Case Term [(Term,Term)]
          | Lam [Term] Term
          | TyParam Term
          | forall a. Show a => Raw a


data Qual = Forall [Term] [Term] Term

strictAp :: Term -> Term -> Term
strictAp f x = ApI "HS.$!"  f x

aps :: Term -> [Term] -> Term
aps = foldl Ap

hasType :: Term -> Term -> Term
hasType t e = ApI "::" e t


--------------------------------------------------------------------------------
instance PP Decl where
  ppPrec n decl =
    case decl of
      DataDecl d     -> ppPrec n d
      FunDecl d      -> ppPrec n d
      InstanceDecl d -> ppPrec n d
      DerivingDecl d -> ppPrec n d
      SigDecl d      -> ppPrec n d

commas :: [Doc] -> Doc
commas = fsep . punctuate comma

instance PP Term where
  ppPrec n t =
    case t of
      Var x     -> text x
      Ap t1 t2  -> wrapIf (n > 1) (hang (ppPrec 1 t1) 2 (ppPrec 2 t2))
      ApI o x y -> wrapIf (n > 0) (hang (ppPrec 1 x) 2 (text o <+> ppPrec 1 y))
      Tuple xs  -> parens (commas (map pp xs))
      List xs   -> brackets (commas (map pp xs))
      Lam [] e  -> ppPrec n e
      Lam ps e  -> wrapIf (n > 0) $
                   hang ("\\" <.> hsep (map (ppPrec 1) ps) <+> "->") 2 (pp e)
      Raw x     -> text (show x)
      TyParam x -> "@" <.> ppPrec 2 x
      If x y z  -> wrapIf (n > 0) $
                   hang ("if" <+> pp x) 2
                        (("then" <+> pp y) $$ ("else" <+> pp z))
      Case x as -> wrapIf (n > 0) $
                   "case" <+> pp x <+> "of"
                      $$ nest 2 (vcat (map alt as))
        where alt (p,e) = pp p <+> "->" <+> pp e

      Do {}     -> wrapIf (n > 0) ("do" <+> ppK t)
        where
        ppStmt x s =
          case x of
            Nothing -> pp s
            Just p  -> hang (ppPrec 1 p <+> "<-") 2 (pp s)

        ppK m =
          case m of
            Do x s k -> ppStmt x s $$ ppK k
            _        -> pp m


instance PP DataDecl where
  pp Data { .. } = kw <+> pp dataLHS $$ nest 2 body
    where
    kw   = case dataCons of
             [ Ap (Var _con) _field ] -> "newtype"
             _ -> "data"
    body = case dataCons of
             [] -> empty
             xs -> block "=" "|" "" (map pp xs)


instance PP FunDecl where
  pp Fun { .. } = hang (pref <+> pp funLHS <+> "=") 2 (pp funDef)
    where pref = case funNS of
                   ValueFun -> empty
                   TypeFun  -> "type" <+> "instance"

instance PP Qual where
  pp (Forall xs cs t) = hang vars 2 (hang ctrs 2 (pp t))
    where
    vars = case xs of
             [] -> empty
             _  -> "forall" <+> hsep (map (ppPrec 2) xs) <.> "."
    ctrs = case cs of
             []  -> empty
             [c] -> pp c <+> "=>"
             _   -> pp (Tuple cs) <+> "=>"

instance PP InstanceDecl where
  pp Instance { .. } = "instance" <+> pragma <+> pp qual <+> "where"
                      $$ nest 2 (vcat' (map pp instMethods))
      where qual = Forall [] instAsmps instHead
            pragma = case instOverlaps of
                       Normal -> empty
                       Overlaps -> "{-# OVERLAPS #-}"

instance PP DerivingDecl where
  pp Deriving { .. } = "deriving instance" <+> pp qual
    where qual = Forall [] deriveAsmps deriveHead

instance PP Import where
  pp (Import x qual) = "import" <+> q <+> text x <+> suff
    where (q,suff) = case qual of
                       Qualified   -> ("qualified", empty)
                       Unqualified -> (empty, empty)
                       QualifyAs a -> ("qualified", "as" <+> text a)

instance PP Module where
  pp Module { .. } = prags $$ ghcPrags $$ header $$ imps $$ decls
    where
    prag a b = braces ("-#" <+> a <+> text b <+> "#-")

    prags = case hsLangExts of
              [] -> empty
              _  -> vcat [ prag "Language" x | x <- hsLangExts ]

    ghcPrags = case hsGHC of
                 [] -> empty
                 _ -> vcat [ prag "OPTIONS_GHC" x | x <- hsGHC ]

    header = "module" <+> text hsModName <+> "where"
    imps   = case hsImports of
               [] -> empty
               _  -> vcat (" " : map pp hsImports)

    decls = case hsDecls of
              [] -> empty
              _  -> vcat' (" " : map pp hsDecls)

instance PP SigDecl where
  pp Sig { .. } = hang (commas (map pp sigName) <+> "::") 2 (pp sigType)


