{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- A branching thing

module Talos.Strategy.PathSymbolic.Branching
  ( Branching -- (..)
  , IndexedBranching (..)
  -- * Constructors
  , singleton  
  , branching
  
  -- * Operations
  , disjointUnion
  , fold
  , fold1  
  , foldM
  , fold1M
  , unzip
  , unzip3
  , catMaybes
  , mapVariants
  , partitionEithers

  , empty
  , null
  , all
  , select
  , resolve
  , muxMaps
  , toSExpr
  , reducePS
  , nub
  , explodeList
  
  -- * Optimising
  , namePathSets
  -- * Debugging
  , invariant
  ) where

import           Control.Monad                       (ap)
import qualified Data.Either                         as Either
import           Data.Foldable                       (foldl', foldlM)
import qualified Data.List                           as List
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (isJust, mapMaybe)
import qualified Data.Maybe                          as Maybe
import           GHC.Generics                        (Generic)
import           Prelude                             hiding (null, unzip,
                                                      unzip3, all)
import qualified Prelude
import qualified SimpleSMT                           as S

import           Daedalus.Panic                      (panic)
import           Daedalus.PP                         (PP, pp, vcat)

import           Talos.Lib                           (andMany, findM)
import qualified Talos.Strategy.PathSymbolic.PathSet as PS
import           Talos.Strategy.PathSymbolic.PathSet (PathSet, PathVar)
import Data.Tuple (swap)

-- FIXME: maybe rep. as a tree
-- data Branching a = Leaf (PathSet, a) | Conj PathSet [Branching a]

-- | A 'Branching' represents a branching value, where exactly one
-- path is viable (perhaps assuming some restricting context).
-- Because the branches are disjoint and total, it is possible to
-- remove one when converting to an if-then-else SMT expression, as
-- the negation of the remaining branches should entail the elided
-- branch.
data Branching a = Branching
  { total :: Bool -- ^ If at least one path is satisfiable (at most one is).
  , variants :: [ (PathSet, a) ]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- FIXME: We could be more efficient here when either side of the
-- applicative is [(true, ...)] to just traverse to avoid creating
-- garbage.
instance Applicative Branching where
  pure v = Branching { total = True, variants = [(PS.true, v)] }
  (<*>) = ap

instance MonadFail Branching where
  fail _ = Branching { total = False, variants = [] }

-- Essentially 'join', we could use this structure to optimise
-- e.g. query size.
instance Monad Branching where
  b >>= f =
    let (tots, vs) = Prelude.unzip 
          [ (total b', (PS.conj ps1 ps2, v))
          | (ps1, w) <- variants b
          , let b' = f w
          , (ps2, v) <- variants b'
          ]
    in branching (or (total b : tots)) vs

newtype IndexedBranching i a = IndexedBranching
  { getIndexedBranching :: Branching (i, a)
  } deriving Generic
    
singleton :: a -> Branching a
singleton = pure

-- | Smart constructor, prunes unsatisfiable pathsets
branching :: Bool -> [(PathSet, a)] -> Branching a
branching isTotal = Branching isTotal  . filter (not . PS.null . fst) 

empty :: Branching a
empty = Branching True []
  
-- | An empty branching is denoted as False
null :: Branching a -> Bool
null = Prelude.null . variants

all :: (a -> Bool) -> Branching a -> Bool
all p = Prelude.all (p . snd) . variants

-- For debugging/checking.  Note the empty branching does not satisfy
-- this.
invariant :: Branching a -> S.SExpr
invariant b = andMany (atLeastOneTrue : atMostOneTrue)
  where
    -- FIXME: name these.
    ps = map (PS.toSExpr . fst) (variants b)
    atLeastOneTrue | total b   = S.orMany ps
                   | otherwise = S.bool True
    atMostOneTrue =
      [ ps' `S.implies` S.not (andMany pss)
      | (ps', pss) <- holes [] [] ps ] 

    -- Ignores order
    holes _lhs acc [] = acc
    holes lhs acc (x : xs) = holes (x : lhs) ((x, lhs ++ xs) : acc) xs

-- -- FIXME: maybe discard the largest path
-- split :: Branching a -> Maybe (a, [(ps, a)])
-- split = undefined

-- Standard operations

-- | Combines two branching structures where the pathsets are
-- disjoint.  This discards totality, so prefer other combinators
disjointUnion :: Branching a -> Branching a -> Branching a
disjointUnion b1 b2 = branching False (variants b1 <> variants b2)

fold :: (PathSet -> a -> b -> b) -> b -> Branching a -> b
fold f i b = foldl' (\a' (ps, a) -> f ps a a') i (variants b)

fold1 :: (PathSet -> a -> a -> a) -> Branching a -> a
fold1 _f Branching { variants = [] } = panic "Empty branching in fold1" []
fold1 f Branching { variants = (_, v) : vs } =
  foldl' (\a' (ps, a) -> f ps a a') v vs

foldM :: Monad m => (PathSet -> a -> b -> m b) -> b -> Branching a -> m b
foldM f i b = foldlM (\a' (ps, a) -> f ps a a') i (variants b)

fold1M :: Monad m => (PathSet -> a -> a -> m a) -> Branching a -> m a
fold1M _f Branching { variants = [] } = panic "Empty branching in fold1M" []
fold1M f Branching { variants = (_, v) : vs } =
  foldlM (\a' (ps, a) -> f ps a a') v vs

catMaybes :: Branching (Maybe a) -> Branching a
catMaybes b = branching (total b && allJust) $ mapMaybe sequence (variants b) -- sequence :: (a, Maybe b) -> Maybe (a, b)
  where
    allJust = all isJust b

-- FIXME: duplicates the pathsets
unzip :: Branching (a, b) -> (Branching a, Branching b)
unzip b = ( branching (total b) (zip pss vs1)
          , branching (total b) (zip pss vs2)
          )
  where
    (pss, vs)  = Prelude.unzip (variants b)
    (vs1, vs2) = Prelude.unzip vs

-- FIXME: duplicates pathsets
unzip3 :: Branching (a, b, c) -> (Branching a, Branching b, Branching c)
unzip3 b = ( branching (total b) (zip pss vs1)
           , branching (total b) (zip pss vs2)
           , branching (total b) (zip pss vs3)
           )
  where
    (pss, vs)  = Prelude.unzip (variants b)
    (vs1, vs2, vs3) = Prelude.unzip3 vs

-- Does not duplicate
partitionEithers :: Branching (Either a b) -> (Branching a, Branching b)
partitionEithers b = ( branching (total b && Prelude.null rs) ls
                     , branching (total b && Prelude.null ls) rs)
  where
    (ls, rs) = Either.partitionEithers [ either (Left . (,) p) (Right . (,) p) v
                                       | (p, v) <- variants b ]

mapVariants :: (PathSet -> a -> Maybe (PathSet, a)) -> Branching a -> Branching a
mapVariants f bvs = branching (total bvs && allJust) (Maybe.catMaybes news)
  where
    news = map (uncurry f) (variants bvs)
    allJust = Prelude.all isJust news

muxMaps :: Ord k => Branching (Map k v) -> Map k (Branching v)
muxMaps bmv = branching (total bmv && Map.size ms' == 1) <$> ms'
  where
  ms' = Map.unionsWith (<>) [ List.singleton . (,) ps <$> m' | (ps, m') <- variants bmv ]

-- | Find the reachable branching value in a model
resolve :: PS.PathSetModelMonad m => Branching a -> m (Maybe a)
resolve bvs =
  fmap snd <$> findM (PS.fromModel . fst) (variants bvs)

reducePS :: Branching PathSet -> PathSet
reducePS b = PS.disjMany (map (uncurry PS.conj) (variants b))

-- | Return some element, if it exists.
select :: Branching a -> Maybe a
select b | (_, x) : _ <- variants b = Just x
         | otherwise = Nothing

-- | Combines duplicate elements onto a single path.
nub :: Ord a => Branching a -> Branching a
nub b
  | [(v, _ps)] <- res, total b = pure v
  | otherwise = b { variants = [ (ps, v) | (v, ps) <- res ] }
  where
    res = Map.toList (Map.fromListWith PS.disj (map swap (variants b)))

explodeList :: Branching [a] -> [ (a, Branching (Maybe a)) ]
explodeList b = go PS.true (variants b)
  where
    go _   [] = []
    -- special case
    go pfx ( (ps, []) : rest ) = go (PS.disj pfx ps) rest
    go pfx ( (ps, xs) : rest)  = goOne pfx ps xs rest

    goOne pfx ps xs rest =
      let pfx' = PS.disjMany (pfx : map fst rest)
          bvs  = [ (x, branching (total b) [ (ps, Just x), (pfx', Nothing) ] )
                 | x <- xs ]
      in bvs ++ go (ps `PS.disj` pfx) rest

-- We can do some optimisations here to make the result easier to
-- read.  If we know the result is total we can produce a bunch of
-- implications, and we can special case e.g. on singletons and where
-- the elements are all true/false.
toSExpr :: Branching S.SExpr -> S.SExpr
toSExpr b
  | [(ps, v)] <- variants b = handleSingleton ps v
  -- prefer this to disjMany (msp PS.toSExpr poss) as it allows to
  -- combine the paths more intelligently (if we ever choose to do
  -- so).
  | Just poss <- collectPos = PS.toSExpr (PS.disjMany poss)
  | total b = andMany [ PS.toSExpr ps `S.implies` v
                      | (ps, v) <- variants b, v /= S.bool True
                      ]
  | otherwise = fold (S.ite . PS.toSExpr) (S.bool False) b
  where
    handleSingleton ps v
      | total b           = v    
      | PS.trivial ps     = v -- maybe entailed by (total b)?
      | v == S.bool False = v -- RHS is 'false'
      | v == S.bool True  = PS.toSExpr ps
      | otherwise         = S.and (PS.toSExpr ps) v

    -- Collect all the positive (i.e., true on the RHS) paths, failing
    -- if we see a non-constant value.
    collectPos = mconcat <$> traverse collectOnePos (variants b) 
    collectOnePos (p, bse)
      | bse == S.bool True  = Just [p]
      | bse == S.bool False = Just []
      | otherwise = Nothing

-- -----------------------------------------------------------------------------
-- Optimising

-- | Names all the pathsets in a branching so that duplication
-- (e.g. via unzip) is cheaper.
namePathSets :: PathVar -> Branching a -> (Int, Branching PathSet, Branching a)
namePathSets pv b = (length (variants b), named, newb)
  -- FIXME: there are 2 equivalent ways of representing the nameing branching:
  --   p1 --> pv = 0; p2 --> pv = 1; ...
  -- and
  --  pv = 0 --> p1; pv = 1 --> p2; ...
  --
  -- we should expeirment to see which is faster in the solver.  The
  -- first option is total, so we will default to that.
  where
    -- turns [ (ps1, v1), (ps2, v2), ... ] into
    -- [ (pv = 0, (ps1, v1)), (pv = 1, (ps2, v2)), ... ] and then unzips the resulting branching.
    (named, newb) = unzip (branching True (zip (map (PS.choiceConstraint pv) [9..]) (variants b)))
    
-- -----------------------------------------------------------------------------
-- Instances

instance PP a => PP (Branching a) where
  pp b = vcat [ pp ps <> " ==> " <> pp v  | (ps, v) <- variants b ]
