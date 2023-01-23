-- | This is convenient for writing parsers with a state that is
-- backtracked when the parser backtracks.
-- It is just a standard state transformer: we implement it here to
-- avoid dependency on an external library and orphan instances.
{-# Language BlockArguments, TypeFamilies #-}
module RTS.StateT
  ( StateT
  , runStateT
  , liftS
  , get
  , set, sets, sets_
  ) where

import Control.Monad(liftM,ap)

import RTS.ParserAPI

newtype StateT s m a = S (s -> m (a,s))

runStateT :: s -> StateT s m a -> m (a,s)
runStateT s (S m) = m s

instance Monad m => Functor (StateT s m) where
  fmap = liftM

instance Monad m => Applicative (StateT s m) where
  pure a = S \s -> pure (a,s)
  (<*>)  = ap

instance Monad m => Monad (StateT s m) where
  S m1 >>= f = S \s1 -> m1 s1 >>= \(a,s2) -> let S m2 = f a in m2 s2

liftS :: Monad m => m a -> StateT s m a
liftS m = S \s -> m >>= \a -> pure (a,s)

get :: Monad m => StateT s m s
get = S \s -> pure (s,s)

set :: Monad m => s -> StateT s m ()
set s = S \_ -> pure ((),s)

-- Strict
sets :: Monad m => (s -> (a,s)) -> StateT s m a
sets f = S \s -> pure $! f s

-- Strict in the state
sets_ :: Monad m => (s -> s) -> StateT s m ()
sets_ f = S \s -> let s1 = f s in s1 `seq` pure ((), s1)


instance BasicParser m => BasicParser (StateT s m) where
  type Annot (StateT s m)   = Annot m
  type ITrace (StateT s m)  = ITrace m
  S m1 ||| S m2       = S \s -> m1 s ||| m2 s
  S m1 <|| S m2       = S \s -> m1 s <|| m2 s
  pFail e             = liftS (pFail e)
  pByte r             = liftS (pByte r)
  pEnter l (S m)      = S \s -> pEnter l (m s)
  pStack              = liftS pStack
  pITrace             = liftS pITrace
  pSetITrace i        = liftS (pSetITrace i)
  pPeek               = liftS pPeek
  pSetInput i         = liftS (pSetInput i)
  pErrorMode e (S m)  = S \s -> pErrorMode e (m s)

