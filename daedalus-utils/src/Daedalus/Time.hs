
-- This module exports utilities to benchmark IO operations

module Daedalus.Time (timeIt) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Clock           (Clock (MonotonicRaw), diffTimeSpec,
                                         getTime, toNanoSecs)

-- Returns the wallclock time for executing m
timeIt :: MonadIO m => m a -> m (a, Integer)
timeIt m = do
  start         <- liftIO $ getTime MonotonicRaw
  r             <- m
  end           <- liftIO $ getTime MonotonicRaw
  pure (r, toNanoSecs (diffTimeSpec end start))


