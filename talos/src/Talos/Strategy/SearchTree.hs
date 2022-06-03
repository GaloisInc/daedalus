{-# LANGUAGE DeriveFunctor #-}

-- We could also use the zipper package, but it is simpler to roll our
-- own (and requires less understanding of lens etc.)

module Talos.Strategy.SearchTree
  ( -- Types, constructors exported mainly to ease debugging
    SearchTree(..)
  , Ctx(..)
  , Location(..)
  -- Views
  , locTag
  , locBranches
  -- Movement
  , upward
  , downward
  , leftward
  , rightward
  , rightUnexplored
  , leftmostUnexplored
  -- Construct/destruct/general manipulation
  , empty
  , fromTree
  , rezip
  , replaceUnexplored
  , forgetGoUp
  -- Helpers
  , maximally
  , tryMove
  ) where
import Control.Applicative ((<|>))

data SearchTree m a =
  Unexplored m
  | Node a [SearchTree m a] -- ^ Invariant: the list is non-empty.
  deriving (Show, Eq, Ord, Functor)
    
data Ctx m a = Top | CNode a [SearchTree m a] (Ctx m a) [SearchTree m a] 
  deriving (Show, Eq, Ord, Functor)

data Location m a = Location
  { locTree :: SearchTree m a
  , locCtx  :: Ctx m a
  }
  deriving (Show, Eq, Ord, Functor)

--------------------------------------------------------------------------------
-- Examination

locTag :: Location m a -> Maybe a
locTag Location { locCtx = CNode a _ _ _ } = Just a
locTag _ = Nothing

locBranches :: Location m a -> Int
locBranches Location { locTree = Node _ ns } = length ns
locBranches _                                = 0

--------------------------------------------------------------------------------
-- Movement

upward :: Location a m -> Maybe (Location a m)
upward Location { locCtx = Top } = Nothing
upward Location { locCtx = CNode a l c r, locTree = t } =
  Just (Location { locCtx = c, locTree = Node a (l ++ [t] ++ r) })

downward :: Int -> Location a m -> Maybe (Location a m)
downward _ Location { locTree = Unexplored _ } = Nothing
downward i Location { locCtx = c, locTree = Node a ns } 
  | (l, t : r) <- splitAt i ns = Just (Location { locCtx = CNode a l c r, locTree = t })
  | otherwise                  = Nothing

-- | Move leftward, locally
leftward :: Location a m -> Maybe (Location a m)
leftward Location { locCtx = Top } = Nothing
leftward Location { locCtx = CNode a l c r, locTree = t }
  | t' : l' <- reverse l = Just (Location { locCtx = CNode a (reverse l') c (t : r), locTree = t' })
  | otherwise            = Nothing

-- | Move rightward, locally
rightward :: Location a m -> Maybe (Location a m)
rightward Location { locCtx = CNode a l c (t' : r'), locTree = t } =
  Just (Location { locCtx = CNode a (l ++ [t]) c r', locTree = t' })
rightward _ = Nothing

-- | Finds the next unexplored node to the right of the current
-- _location_.
rightUnexplored :: Location a m -> Maybe (Location a m)
rightUnexplored l =
  (leftmostUnexplored <$> rightward l) <|> (rightUnexplored =<< upward l)

-- | Moves the location to the leftmost unexplored _in the current tree_.
leftmostUnexplored :: Location a m -> Location a m
leftmostUnexplored = maximally (downward 0)

--------------------------------------------------------------------------------
-- Conversion to/from trees

empty :: m -> Location m a
empty = fromTree . Unexplored

fromTree :: SearchTree a m -> Location a m
fromTree t = Location { locCtx = Top, locTree = t }

rezip :: Location a m -> SearchTree a m
rezip = locTree . maximally upward

--------------------------------------------------------------------------------
-- Extending search space

-- | Replace the current tree with a new choice node.  Probably only
-- makes sense if the current node is Unexplored
replaceUnexplored :: a -> [m] -> Location m a -> Location m a
replaceUnexplored a new l = l { locTree = Node a (map Unexplored new) }

-- | Forget the current tree and move one node upwards, if possible.
-- If we can't go up, it means we have no more search tree nodes.
forgetGoUp :: Location m a -> Maybe (Location m a)
forgetGoUp = go . locCtx
  where
    -- We allow empty choices.
    -- go (CNode _a [] c []) = go c
    go (CNode  a l  c r) =
      Just (Location { locCtx = c, locTree = Node a (l ++ r) })
    go _ = Nothing

--------------------------------------------------------------------------------
-- Helpers

maximally :: (a -> Maybe a) -> a -> a
maximally f = go
  where
    go x
      | Just x' <-  f x = go x'
      | otherwise = x


tryMove :: (a -> Maybe a) -> a -> a
tryMove f x
  | Just x' <- f x = x'
  | otherwise      = x
