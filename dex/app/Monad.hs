module Monad where

import Control.Monad

newtype M ro rw err a =
  M { runMonad :: ro -> rw -> Either err (a,rw) }

instance Functor (M ro rw err) where
  fmap = liftM

instance Applicative (M ro rw err) where
  pure a = M \_ rw -> Right (a, rw)
  (<*>)  = ap

instance Monad (M ro rw err) where
  M m >>= k = M \ro rw ->
    do
      (a,rw') <- m ro rw
      let M m1 = k a
      m1 ro rw'

getEnv :: M ro rw err ro
getEnv = M \ro rw -> pure (ro,rw)

updEnv :: (ro -> ro) -> M ro rw err a -> M ro rw err a
updEnv f (M m) = M \ro rw -> m (f ro) rw

getState :: M ro rw err rw
getState = M \_ rw -> pure (rw,rw)

setState :: rw -> M ro rw err ()
setState s = M \_ _ -> pure ((), s)

updState :: (rw -> rw) -> M ro rw err ()
updState f = M \_ rw -> pure ((), f rw)

reportError :: err -> M ro rw err a
reportError err = M \_ _ -> Left err