{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Talos.Strategy.Boltzmann.Types (
    SelectedPathF(..), SelectedPath,
    Polynomial(..),
    module Talos.Strategy.Boltzmann.Types,
    ) where

import Data.List
import Data.Foldable
import Data.Void
import Daedalus.Core.Basics
import Talos.Strategy.Boltzmann.Polynomial
import Talos.Strategy.Boltzmann.Util
import Talos.SymExec.Path

data Weighted a = Weighted
    { weight :: Double
    , choice :: a
    } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance PP a => PP (Weighted a) where
    pp w = pp (choice w) <+> parens (text "weight" <+> pp (weight w))

newtype WeightedChoice a = WeightedChoice { wc :: [Weighted a] }
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance PP a => PP (WeightedChoice a) where
    pp (WeightedChoice ch) = fold (intersperse (text "/") (pp <$> ch))

newtype WeightedCase a = WeightedCase (Case (Weighted a))
    deriving (Functor, Foldable, Traversable, PP)

data FName1 a = FName1 FName deriving (Functor, Foldable, Traversable)

instance PP (FName1 a) where ppPrec _ (FName1 fn) = space <> pp fn

type WeightedPath = SelectedPathF WeightedChoice WeightedCase FName1 Void
