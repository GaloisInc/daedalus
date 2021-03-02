{-# Language OverloadedStrings #-}
module Daedalus.Core.Grammar where

import Data.Functor.Identity(Identity(..))

import Daedalus.PP
import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Daedalus.Core.ByteSet

data Grammar =
    Pure Expr
  | GetStream
  | SetStream Expr
  | Match Sem Match
  | Fail ErrorSource Type (Maybe Expr)
  | Do_ Grammar Grammar
  | Do  Name Grammar Grammar
  | Let Name Expr Grammar
  | OrBiased Grammar Grammar
  | OrUnbiased Grammar Grammar
  | Call FName [Expr]
  | Annot Annot Grammar
  | GCase (Case Grammar)

-- | Implicit input manipulation
data Match =
    MatchByte ByteSet       -- ^ Match a single byte
  | MatchBytes Expr         -- ^ Match a sequence of bytes
  | MatchEnd                -- ^ Match the end of input

data Sem = SemNo | SemYes


data ErrorSource = ErrorFromUser | ErrorFromSystem


gIf :: Expr -> Grammar -> Grammar -> Grammar
gIf e g1 g2 = gCase e (BoolCase [ (True, g1), (False, g2) ]) Nothing

gCase :: Expr -> CaseBody Grammar -> Maybe Grammar -> Grammar
gCase e as def = GCase (Case e as def)

--------------------------------------------------------------------------------

childrenG ::
  Applicative f => (Grammar -> f Grammar) -> Grammar -> f Grammar
childrenG f gram =
  case gram of
    Pure {}           -> pure gram
    GetStream         -> pure gram
    SetStream {}      -> pure gram
    Match {}          -> pure gram
    Fail {}           -> pure gram
    Do_ g1 g2         -> Do_ <$> f g1 <*> f g2
    Do  x g1 g2       -> Do x <$> f g1 <*> f g2
    Let x e g         -> Let x e <$> f g
    OrBiased g1 g2    -> OrBiased <$> f g1 <*> f g2
    OrUnbiased g1 g2  -> OrUnbiased <$> f g1 <*> f g2
    Call {}           -> pure gram
    Annot a g         -> Annot a <$> f g
    GCase c           -> GCase <$> traverse f c

mapChildrenG :: (Grammar -> Grammar) -> Grammar -> Grammar
mapChildrenG f g = g1
  where Identity g1 = childrenG (Identity . f) g



--------------------------------------------------------------------------------


instance PP Grammar where
  pp gram =
    case gram of
      Pure e         -> "pure" <+> ppPrec 1 e
      Match s m      -> ppMatch s m
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
      GCase c        -> pp c

ppMatch :: Sem -> Match -> Doc
ppMatch s mat =
  case mat of
    MatchBytes e -> "match" <.> ppSemSuff s <+> pp e
    MatchByte bs -> "match1" <.> ppSemSuff s <+> pp bs
    MatchEnd     -> "matchEnd"

ppSemSuff :: Sem -> Doc
ppSemSuff sem =
  case sem of
    SemYes -> ""
    SemNo  -> "_"

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


