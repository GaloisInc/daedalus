{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric, DeriveAnyClass #-}

module Daedalus.Core.ByteSet where

import GHC.Generics          (Generic)
import Control.DeepSeq       (NFData)

import Data.Functor.Identity(Identity(..))

import Daedalus.PP

import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Data.Functor.Const

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
  deriving (Generic,NFData)

instance CoreSyn ByteSet where
  coreLet       = SetLet
  coreCase x ps = SetCase (Case x ps)
  coreCall      = SetCall

ebChildrenB ::
  Applicative f => (Expr -> f Expr) -> (ByteSet -> f ByteSet) ->
                   ByteSet -> f ByteSet
ebChildrenB ef bf bs =
  case bs of
    SetAny -> pure bs
    SetSingle e -> SetSingle <$> ef e
    SetRange e1 e2 -> SetRange <$> ef e1 <*> ef e2

    SetComplement b -> SetComplement <$> bf b
    SetUnion b b'   -> SetUnion <$> bf b <*> bf b'
    SetIntersection b b' -> SetIntersection <$> bf b <*> bf b'

    SetLet n e b -> SetLet n <$> ef e <*> bf b
    SetCall fn es -> SetCall fn <$> traverse ef es
    SetCase cs -> SetCase <$> traverse bf cs

ebMapChildrenB :: (Expr -> Expr) -> (ByteSet -> ByteSet) -> ByteSet -> ByteSet
ebMapChildrenB ef bf bs = g1
  where Identity g1 = ebChildrenB (Identity . ef) (Identity . bf) bs

ebFoldMapChildrenB :: Monoid m => (Expr -> m) -> (ByteSet -> m) -> ByteSet -> m
ebFoldMapChildrenB ef bf bs = m
  where Const m = ebChildrenB (Const . ef) (Const . bf) bs

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
