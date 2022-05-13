{-# LANGUAGE DefaultSignatures #-}

module Talos.Analysis.Eqv where

import           Data.Function (on)
import           Data.Map      (Map)
import qualified Data.Map      as Map

import           Daedalus.Core (Case (..))

class Eqv a where
  eqv :: a -> a -> Bool
  default eqv :: Eq a => a -> a -> Bool
  eqv = (==)

instance (Ord k, Eqv v) => Eqv (Map k v) where
  eqv m1 m2 = 
    Map.keysSet m1 == Map.keysSet m2
    && Map.isSubmapOfBy eqv m1 m2


instance Eqv Int
instance Eqv Integer

-- instance Eq p => Eqv (SummaryClass p) 
  -- eqv Assertions Assertions = True
  -- eqv (Result p) (Result q) = eqv p q
  -- eqv _          _          = False

-- instance Eqv SLExpr -- juse (==)

instance Eqv ()

instance (Eqv a, Eqv b) => Eqv (a, b) where
  eqv (a, b) (a', b') = a `eqv` a' && b `eqv` b'

instance (Eqv a, Eqv b, Eqv c) => Eqv (a, b, c) where
  eqv (a, b, c) (a', b', c') = a `eqv` a' && b `eqv` b' && c `eqv` c'

instance Eqv a => Eqv [a] where
  eqv xs ys = length xs == length ys && and (zipWith eqv xs ys)

-- FIXME: check length here?
instance Eqv a => Eqv (Case a) where
  eqv (Case _e alts1) (Case _e' alts2) =
    and (zipWith (eqv `on` snd) alts1 alts2)
