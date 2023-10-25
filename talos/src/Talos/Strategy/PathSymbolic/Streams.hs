{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Talos.Strategy.PathSymbolic.Streams
  ( StreamTreeNode(..)
  , StreamTreeInfo(..)
  , null
  , branching
  , singleton
  ) where

import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Sequence                         (Seq ((:<|), (:|>)))
import qualified Data.Sequence                         as Seq
import           GHC.Generics                          (Generic)
import           Prelude                               hiding (null)
import           Data.Foldable                         (find, toList)
import qualified SimpleSMT                             as S
import           SimpleSMT                             (SExpr)
import           Control.Monad                         (replicateM)
import           Control.Monad.IO.Class                (MonadIO)
import           Data.Set                              (Set)
import qualified Data.Set as Set

import           Data.Traversable                      (forM)

import           Daedalus.GUID                         (GUID, HasGUID,
                                                        invalidGUID)
import           Daedalus.Core.Type                    (sizeType)
import           Daedalus.Core                    (Op0(IntL))
import           Daedalus.Panic                        (panic)

import qualified Talos.Solver.SolverT                  as Solv
import           Talos.Solver.SolverT                  (SMTVar, SolverT)
import qualified Talos.Strategy.PathSymbolic.Branching as B
import           Talos.Strategy.PathSymbolic.Branching (Branching)
import qualified Talos.Strategy.PathSymbolic.Assertion as A
import           Talos.Strategy.PathSymbolic.Assertion (Assertion)
import qualified Talos.Strategy.PathSymbolic.PathSet   as PS
import           Talos.Strategy.PathSymbolic.PathSet   (PathSet)
import           Talos.Strategy.PathSymbolic.SymExec   (symExecOp0, symExecTy)
import Control.Arrow ((&&&))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (maybeToList)
import Daedalus.PP (PP, pp, text)

-- There are essentially three classes of stream:
--
--   1. the current stream, which accumulates usages (i.e., the length
--      used increments for each match operation), termed "open";
--
--   2. formerly current streams which have been replaced by SetStream,
--      and are termed "closed";
--
--   3. streams which have been derived via Take and DropMaybe. This
--      category also includes streams obtained via GetStream, which
--      (FIXME?)  means that GetStream should also close the current
--      stream.
--
-- Generally streams will be branching, as e.g. the current stream can
-- be path dependent, as will the number of bytes consumed.  For example:
--
--   1 def Main = block
--   2   Choose
--   3     block
--   4       Match "bounded"
--   5       SetStream (Take 10 GetStream)
--   6     block
--   7       Match "unbounded"
--   8   Match "hello" | Match "goodbye"
--
-- where at line 8 the current stream will be branching based upon
-- line 2, and after line 8 the amount consumed will be either 5 or 7.
-- In most usages the SetStream is expected to dominate the bytes in
-- the stream (i.e., the Bounded will be a singleton) while the amount
-- consumed will vary on the paths.
--
-- This branching gets removed (for both offset and length) by
-- inventing a new solver variable and assigning it to 0 everywhere
-- the stream is not defined.  This means we get a top-level term we
-- can use for non-overlap checks without requiring path conditions.
--
-- Q: is it possible to arrange things such that in the case of just
--    using Chunk we get the obvious, simple, result?
-- 

data StreamTreeNode = StreamTreeNode
  { stnBase   :: SMTVar
  , stnLength :: SMTVar
  } deriving (Show, Eq, Ord, Generic)

-- This is not a branching as the values aren't alternatives.
data StreamTreeInfo = StreamTreeInfo
  { streamTreeVars :: Set SMTVar
  -- ^ A cache of the variables appearing in the below.
  , streamTreeInfo :: Seq StreamTreeNode
  } deriving (Generic)

instance Semigroup StreamTreeInfo where
  sti1 <> sti2 = StreamTreeInfo 
    (streamTreeVars sti1 <> streamTreeVars sti2)
    (streamTreeInfo sti1 <> streamTreeInfo sti2)

instance Monoid StreamTreeInfo where
  mempty = StreamTreeInfo mempty mempty
    
null :: StreamTreeInfo -> Bool
null = Seq.null . streamTreeInfo

branching :: Branching StreamTreeInfo -> (Assertion, StreamTreeInfo)
branching b = (assn, sti)
  where
    assn = A.BAssert (mkAssn <$> b)
    mkAssn sti'
      | Just ne <- nonEmpty (toList leftovers) =
          A.entailMany PS.true (assert0 <$> ne)
      | otherwise = A.true
      where
        leftovers = allvs `Set.difference` streamTreeVars sti'
        assert0   = A.SExprAssert . (`S.eq` sizeToSExpr 0) . S.const
      
    sti = StreamTreeInfo { streamTreeVars = allvs
                         , streamTreeInfo = allinfo
                         }
    (allvs, allinfo) = foldMap (streamTreeVars &&& streamTreeInfo) b

singleton :: StreamTreeNode -> StreamTreeInfo
singleton n = StreamTreeInfo
  { streamTreeVars = Set.fromList [ stnBase n, stnLength n ]
  , streamTreeInfo = Seq.singleton n
  }



  
-- -----------------------------------------------------------------------------
-- Resolving into an SMT term

-- The basic idea here is to remove the branching by setting the
-- length to 0 on the paths where the stream isn't created.

-- FIXME: how do we ensure legitimate empty segments are inside the
-- file?

type STCtxt m = (Monad m, HasGUID m, MonadIO m)

-- data StreamTree = StreamTree
--   { stNode :: StreamTreeNode
--   , stGuard :: SExpr
--   , stChildren :: Seq StreamTree
--   } deriving (Generic)

-- makeStreamTree :: StreamTreeInfo -> StreamTree StreamTreeNode
-- makeStreamTree sti
--   | Just root <- find ((==) invalidGUID . stnParent) stns = go root
--   | otherwise = panic "Couldn't find root node" []
--   where
--     go stn =
--       let stns' = Map.findWithDefault mempty (stnID stn) depMap
--       in StreamTree { stNode = stn
--                     , stChildren = fmap go stns'
--                     }
    
--     stns = flattenSTI sti
--     depMap = Map.fromListWith (<>) [ (stnParent stn, Seq.singleton stn) | stn <- stns ]

-- FIXME: a bit gross.
sizeToSExpr :: Integer -> SExpr
sizeToSExpr i = symExecOp0 (IntL i sizeType)

makeAssertion :: STCtxt m => StreamTreeInfo -> SolverT m SExpr
makeAssertion st = do
  let sizeS = symExecTy sizeType
  -- Just to make things easier to read

  -- assert nodes are bounded and non-wrapped
  --   - relOffset <= relOffset + len && relOffset + len <= parent-len
  boundedSym <- Solv.defineFunSymbol "bounded"
                  [ ("parent-len", sizeS)
                  , ("off", sizeS), ("len", sizeS) ]
                  S.tBool
                  (S.andMany [ S.bvULeq (S.const "off") (S.bvAdd (S.const "off") (S.const "len"))
                             , S.bvULeq (S.bvAdd (S.const "off") (S.const "len")) (S.const "parent-len")
                             ])

  noOverlapSym <-  Solv.defineFunSymbol "no-overlap"
                   [ ("off1", sizeS), ("len1", sizeS) 
                   , ("off2", sizeS), ("len2", sizeS) ]
                   S.tBool
                   (S.ite (S.or (S.eq (S.const "len1") (sizeToSExpr 0)) (S.eq (S.const "len2") (sizeToSExpr 0)))
                          (S.bool True)
                          (S.ite (S.bvULt (S.const "off1") (S.const "off2"))
                                 (S.bvULeq (S.bvAdd (S.const "off1") (S.const "len1")) (S.const "off2"))
                                 (S.and (S.bvULt (S.const "off2") (S.const "off1"))
                                        (S.bvULeq (S.bvAdd (S.const "off2") (S.const "len2")) (S.const "off1")))))
  
  -- assert nodes are non-overlapping
  --   - go [] = true
  --   - go (x : xs) = all (disjoint' x) xs && go xs
  --   - disjoint' x y = (len x == 0) \/ (len y == 0) \/ (off x <= y /\ off x + len x <= y) \/ (off y <= off x /\ off y + len y <= x)
  
  undefined

-- toSExpr :: (Monad m, HasGUID m) =>
--            StreamTreeInfo -> SolverT m SExpr
-- toSExpr sti = do
--   let s
    
-- -----------------------------------------------------------------------------
-- Instances 

instance PP StreamTreeNode where
  pp stn = "[" <> text (stnBase stn) <> "..+" <> text (stnLength stn) <> ")"
  
