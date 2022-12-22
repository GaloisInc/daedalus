{-# Language TypeFamilies, RankNTypes, TypeSynonymInstances #-}
module Daedalus.Interp.Loop where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Control.Monad(foldM)

import Daedalus.Value
import qualified Daedalus.AST as K

import Daedalus.Interp.Env

--------------------------------------------------------------------------------
-- Generic utilities for evaluating loops

type family ResultFor a where
  ResultFor K.Value   = Value
  ResultFor K.Grammar = Parser Value

data LoopEval col k = LoopEval
  { unboxCol  :: Value -> col
  , loopNoKey :: (Value -> Value          -> ResultFor k) ->
                  Value -> col -> ResultFor k
  , loopKey   :: (Value -> Value -> Value -> ResultFor k) ->
                  Value -> col -> ResultFor k

  , mapNoKey  :: (Value          -> ResultFor k) -> col -> ResultFor k
  , mapKey    :: (Value -> Value -> ResultFor k) -> col -> ResultFor k
  }

loopOverArray :: LoopEval (Vector.Vector Value) K.Value
loopOverArray =
  LoopEval
    { unboxCol  = valueToVector
    , mapNoKey  = \f -> VArray . Vector.map f
    , mapKey    = \f -> VArray . Vector.imap (stepKeyMap f)
    , loopNoKey = \f s -> Vector.foldl' f s
    , loopKey   = \f s -> Vector.ifoldl' (stepKey f) s
    }
  where
  stepKey f    = \sV kV elV -> f sV (vSize (toInteger kV)) elV
  stepKeyMap f = \   kV elV -> f    (vSize (toInteger kV)) elV

loopOverArrayM :: LoopEval (Vector.Vector Value) K.Grammar
loopOverArrayM =
  LoopEval
    { unboxCol  = valueToVector
    , loopNoKey = \f s -> Vector.foldM'           f  s
    , loopKey   = \f s -> Vector.ifoldM' (stepKey f) s
    , mapNoKey  = \f   -> fmap VArray . Vector.mapM f
    , mapKey    = \f   -> fmap VArray . Vector.imapM (stepKeyMap f)
    }
  where
  stepKey f    = \sV kV elV -> f sV (vSize (toInteger kV)) elV
  stepKeyMap f = \   kV elV -> f    (vSize (toInteger kV)) elV

loopOverMap :: LoopEval (Map Value Value) K.Value
loopOverMap =
  LoopEval
    { unboxCol  = valueToMap
    , loopNoKey = \f s -> Map.foldl' f s
    , loopKey   = \f s -> Map.foldlWithKey' f s
    , mapNoKey  = \f -> VMap . Map.map f
    , mapKey    = \f -> VMap . Map.mapWithKey f
    }

loopOverMapM :: LoopEval (Map Value Value) K.Grammar
loopOverMapM =
  LoopEval
    { unboxCol  = valueToMap
    , loopNoKey = \f s -> foldM (stepNoKey f) s . Map.toList
    , loopKey   = \f s -> foldM (stepKey   f) s . Map.toList
    , mapNoKey  = \f -> fmap VMap . traverse f
    , mapKey    = \f -> fmap VMap . Map.traverseWithKey f
    }
  where
  stepNoKey f = \sV (_  ,elV)-> f sV    elV
  stepKey f   = \sV (kV,elV) -> f sV kV elV





