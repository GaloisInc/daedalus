{-# LANGUAGE InstanceSigs #-}
module Talos.StatefulProcessor where

import Control.Monad

-- | A monad describing a stateful computation that can fail, combining the
-- State and Either monads.
--
-- Doing this by hand as a learning experience, but
-- in practice this should be replaced with monad transformers.
newtype StatefulProcessor s l r = StatefulProcessor 
    { runState :: s -> (Either l r, s)
    }

state :: (s -> (Either l r, s)) -> StatefulProcessor s l r
state = StatefulProcessor

get :: StatefulProcessor s l s
get = state $ \s -> (Right s, s)

put :: s -> StatefulProcessor s l ()
put newState = state $ \_ -> (Right (), newState)

throwError :: l -> StatefulProcessor s l r
throwError err = state $ \s -> (Left err, s)

instance Functor (StatefulProcessor s l) where
    fmap = liftM

instance Applicative (StatefulProcessor s l) where
    pure :: a -> StatefulProcessor s l a
    pure x = state $ \s -> (Right x, s)

    (<*>) :: StatefulProcessor s l (a -> b) -> StatefulProcessor s l a -> StatefulProcessor s l b
    (<*>) = ap

instance Monad (StatefulProcessor s l) where
    (>>=) :: StatefulProcessor s l a -> (a -> StatefulProcessor s l b) -> StatefulProcessor s l b
    (>>=) sp f = state $ \s ->
        case runState sp s of
            (Right x, s') -> runState (f x) s'
            (Left err, s') -> (Left err, s')