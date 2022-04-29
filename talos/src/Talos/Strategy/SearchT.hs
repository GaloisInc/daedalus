{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

module Talos.Strategy.SearchT where -- (SearchT, runSearchT) where

import           Control.Monad.Trans.Free
import           Talos.Strategy.SearchTree (Location)
import qualified Talos.Strategy.SearchTree as ST

-- c.f. https://www.haskellforall.com/2013/06/from-zero-to-cooperative-threads-in-33.html

--------------------------------------------------------------------------------
-- One way to think about searching is to consider each element of the
-- frontier to be a thread which cooperates with the scheduler by
-- forking (choosing) from a set of next steps, and by
-- failing/backtracking when the search fails.  The scheduler is then
-- the search policy.

data ThreadF next = Choose [next] | Backtrack
  deriving (Functor)

type SearchT = FreeT ThreadF 

choose :: Monad m => [a] -> SearchT m a
choose xs = liftF (Choose xs)

backtrack :: Monad m => SearchT m a
backtrack = liftF Backtrack

--------------------------------------------------------------------------------
-- Strategies

type SearchStrat m = forall r. SearchT m r -> m (Maybe r)

bfs :: Monad m => SearchStrat m
bfs m = go [m]
  where
    go [] = pure Nothing
    go (x : xs) = do
      r <- runFreeT x
      case r of
        Free (Choose xs') -> go (xs ++ xs')
        Free Backtrack    -> go xs
        Pure res          -> pure (Just res)

dfs :: Monad m => SearchStrat m
dfs m = go [m]
  where
    go [] = pure Nothing
    go (x : xs) = do
      r <- runFreeT x
      case r of
        Free (Choose xs') -> go (xs' ++ xs)
        Free Backtrack    -> go xs
        Pure res          -> pure (Just res)

tree :: Monad m =>
        (forall n. Location n () -> m (Location n ())) ->
        (forall n. Location n () -> m (Maybe (Location n ()))) ->
        SearchStrat m
tree chSel btSel m = go (ST.empty m)
  where
    go loc =
      case ST.locTree loc of
        ST.Unexplored m' -> do
          r <- runFreeT m'
          case r of
            Free (Choose xs') ->
              go =<< chSel (ST.replaceUnexplored () xs' loc)
            Free Backtrack -> do
              m_loc' <- btSel loc
              case m_loc' of
                Nothing   -> pure Nothing
                Just loc' -> go loc'
            Pure res       -> pure (Just res)
        ST.Node {} -> error "Unexpected node"
        
--------------------------------------------------------------------------------
-- Examples

searchSort :: Ord a => [a] -> IO (Maybe [a])
searchSort = bfs . go []
  where
    go acc [] = pure acc
    go acc xs = do
      (i, x) <- choose (zip [0..] xs)
      let (l, _ : u) = splitAt i xs
      case acc of
        y : _ | y > x -> backtrack
        _ -> go (x : acc) (l ++ u)

-- newtype SearchTState r m = SearchTState
--   { stTree     :: SearchTree (m r) ()
--   }
    
-- newtype SearchT r m a = SearchT { getSearchT :: StateT (SearchTState r m) m a }
--   deriving (Functor, Applicative, Monad)

-- runSearchT :: SearchT ann r m a -> m r
-- runSearchT = undefined

-- choose :: [SearchT ann r m a] -> SearchT ann r m a
-- choose opts = 
