{-# Language OverloadedStrings #-}
module Daedalus.Core.ByteSet where

import Daedalus.PP

import Daedalus.Core.Basics
import Daedalus.Core.Expr

data ByteSet =
    SetAny                              -- ^ Any bytes
  | SetSingle Expr                      -- ^ Exactly this
  | SetRange Expr Expr                  -- ^ Inclusive range

  | SetComplement ByteSet
  | SetUnion ByteSet ByteSet
  | SetIntersection ByteSet ByteSet

  | SetLet Name Expr ByteSet
  | SetCall FName [Expr]
  | SetCase (Case ByteSet)

bCase :: Expr -> [(Pattern,ByteSet)] -> ByteSet
bCase e bs = SetCase (Case e bs)

instance PP ByteSet where
  ppPrec n bs =
    case bs of
      SetAny -> "{ ... }"
      SetSingle e -> "{" <+> pp e <+> "}"
      SetRange e1 e2 -> "{" <+> pp e1 <+> ".." <+> pp e2 <+> "}"
      SetComplement x -> "!" <.> ppPrec 3 x
      SetUnion x y -> wrap 1 (ppPrec 1 x <+> "|" <+> ppPrec 1 y)
      SetIntersection x y -> wrap 2 (ppPrec 2 x <+> "&" <+> ppPrec 2 y)

      SetCall f es -> pp f <.> parens (commaSep (map pp es))
      SetCase e -> pp e
      SetLet x e k -> "let" <+> pp x <+> "=" <+> pp e $$ "in" <+> pp k

    where
    wrap x = if n > x then parens else id
