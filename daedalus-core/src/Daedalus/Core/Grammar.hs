{-# Language OverloadedStrings #-}
{-# Language ViewPatterns, PatternSynonyms #-}

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
gIf e g1 g2 = GCase (Case e [ (PBool True, g1), (PBool False, g2) ])

gCase :: Expr -> [(Pattern,Grammar)] -> Grammar
gCase e as = GCase (Case e as)

--------------------------------------------------------------------------------

pattern Choice :: Bool -> [Grammar] -> Grammar
pattern Choice biased cs <- (collectChoices -> Just (biased, cs))

collectChoices :: Grammar -> Maybe (Bool, [Grammar])
collectChoices g@(OrUnbiased {}) = Just (False, go g)
  where
    go (OrUnbiased l r) = go l ++ go r
    go g'               = [g']

collectChoices g@(OrBiased {}) = Just (True, go g)
  where
    go (OrBiased l r) = go l ++ go r
    go g'             = [g']

collectChoices _ = Nothing

--------------------------------------------------------------------------------

-- geb is 'grammar expr byteset'
gebChildrenG ::
  Applicative f => (Grammar -> f Grammar) -> (Expr -> f Expr) -> (ByteSet -> f ByteSet) ->
                   Grammar -> f Grammar
gebChildrenG gf ef bf gram =
  case gram of
    Pure e            -> Pure <$> ef e
    GetStream         -> pure gram
    SetStream e       -> SetStream <$> ef e
    Match s m         -> Match s <$> case m of
      MatchByte bs -> MatchByte  <$> bf bs
      MatchBytes e -> MatchBytes <$> ef e
      MatchEnd     -> pure MatchEnd
    Fail es ty m_e    -> Fail es ty <$> traverse ef m_e
    Do_ g1 g2         -> Do_ <$> gf g1 <*> gf g2
    Do  x g1 g2       -> Do x <$> gf g1 <*> gf g2
    Let x e g         -> Let x <$> ef e <*> gf g
    OrBiased g1 g2    -> OrBiased <$> gf g1 <*> gf g2
    OrUnbiased g1 g2  -> OrUnbiased <$> gf g1 <*> gf g2
    Call fn args      -> Call fn <$> traverse ef args
    Annot a g         -> Annot a <$> gf g
    GCase (Case e ps) -> GCase <$> (Case <$> ef e <*> traverse (\(a, b) -> (,) a <$> gf b) ps)

gebMapChildrenG :: (Grammar -> Grammar) -> (Expr -> Expr) -> (ByteSet -> ByteSet) ->
                   Grammar -> Grammar
gebMapChildrenG gf ef bf g = g1
  where Identity g1 = gebChildrenG (Identity . gf) (Identity . ef) (Identity . bf) g

childrenG ::
  Applicative f => (Grammar -> f Grammar) -> Grammar -> f Grammar
childrenG f = gebChildrenG f (pure . id) (pure . id)

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


