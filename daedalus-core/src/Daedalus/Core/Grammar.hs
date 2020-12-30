{-# Language OverloadedStrings #-}
module Daedalus.Core.Grammar where

import Daedalus.PP
import Daedalus.Core.Basics
import Daedalus.Core.Expr

data Grammar =
    Pure Expr
  | GetStream
  | SetStream Expr
  | Fail ErrorSource Type (Maybe Expr)
  | Do_ Grammar Grammar
  | Do  Name Grammar Grammar
  | Let Name Expr Grammar
  | OrBiased Grammar Grammar
  | OrUnbiased Grammar Grammar
  | Call FName [Expr]
  | Annot Annot Grammar
  | Case Expr [(Pattern,Grammar)]

data ErrorSource = ErrorFromUser | ErrorFromSystem

gIf :: Expr -> Grammar -> Grammar -> Grammar
gIf e g1 g2 = Case e [ (PBool True, g1), (PBool False, g2) ]

--------------------------------------------------------------------------------

instance PP Grammar where
  pp gram =
    case gram of
      Pure e         -> "pure" <+> ppPrec 1 e
      GetStream      -> "getStream"
      SetStream e    -> "setStream" <+> ppPrec 1 e
      Fail src t e   -> ppTApp 0 ("fail" <.> suff) [t] <+> ppMb e
        where suff = case src of
                       ErrorFromUser    -> "_user"
                       ErrorFromSystem  -> "_sys"
              ppMb = maybe empty pp
      Do_ {}         -> "do" <+> ppStmts gram
      Do  {}         -> "do" <+> ppStmts gram
      Let {}         -> "do" <+> ppStmts gram
      OrBiased g1 g2 -> "try" $$ nest 2 (pp g1) $$ ppOrBiased g2
      OrUnbiased {}  -> nest 2 (ppOrUnbiased gram)
      Call f es      -> pp f <.> parens (commaSep (map pp es))
      Annot l g      -> "--" <+> pp l $$ pp g
      Case e as      -> "case" <+> pp e <+> "of" $$ nest 2 (vcat (map alt as))
        where
        alt (p,g) = pp p <+> "->" $$ nest 2 (pp g)

ppOrUnbiased :: Grammar -> Doc
ppOrUnbiased gram =
  case gram of
    OrUnbiased g1 g2 -> "fork" $$ nest 2 (pp g1) $$ ppOrUnbiased g2
    Annot a g -> "--" <+> pp a $$ ppOrUnbiased g
    _ -> "fork" $$ nest 2 (pp gram)

ppOrBiased :: Grammar -> Doc
ppOrBiased gram =
  case gram of
    OrBiased g1 g2 -> "else try" $$ nest 2 (pp g1) $$ ppOrBiased g2
    Annot a g      -> "--" <+> pp a $$ ppOrBiased g
    _              -> "else" $$ nest 2 (pp gram)

ppStmts :: Grammar -> Doc
ppStmts gram =
  case gram of
    Do_ g1 g2  -> pp g1 $$ ppStmts g2
    Do x g1 g2 -> pp x <+> "<-" <+> pp g1 $$ ppStmts g2
    Let x e g  -> "let" <+> pp x <+> "=" <+> pp e $$ ppStmts g
    Annot a g  -> "--" <+> pp a $$ ppStmts g
    _          -> pp gram


