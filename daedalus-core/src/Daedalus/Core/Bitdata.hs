{-# Language ImplicitParams, ConstraintKinds, BlockArguments #-}
-- | Common functionality for compiling bitdata
module Daedalus.Core.Bitdata where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits(shiftL, (.|.))

import Daedalus.Panic(panic)
import Daedalus.PP(pp)
import qualified Daedalus.BDD as BDD

import Daedalus.Core.Basics
import Daedalus.Core.Type(bdUniverse)
import Daedalus.Core.Decl

-- operations:
-- constructors
-- case/patterns
-- selectors
-- comparisons

-- | Maps type name to (universe, definition)
type HasTDecls = (?tdecls :: Map TName TDecl)

getBD :: HasTDecls => TName -> TDecl
getBD t = case Map.lookup t ?tdecls of
            Just r -> r
            Nothing -> panic "getBD" ["Missing bitdata declaration", show (pp t)]


-- | Expression for an n-bit number representing a bitdata struct.
bdStructCon :: [BDField] -> Map Label a -> Expr a
bdStructCon fs args = foldl setField (Const (foldl setBits 0 fs)) fs
  where
  getData l = case Map.lookup l args of
                Just e -> e
                Nothing -> panic "bdStructCon" ["Missing field: " ++ show (pp l) ]

  setBits base f =
    case bdFieldType f of
      BDTag x -> base .|. (x `shiftL` bdOffset f)
      _       -> base

  setField e f =
    case bdFieldType f of
      BDData l t ->
        case e of
          Const 0 -> v
          _       -> Or e v
        where v = ShiftL (ToBits (bdWidth f) (Extern (getData l))) (bdOffset f)
      _ -> e


bdCase :: HasTDecls => TName -> [(Label,a)] -> Map Integer (Map Integer a)
bdCase t = BDD.groupTestsByMask' . BDD.patTestsAssumingInOrder univ . map doAlt
  where
  (univ,cons) = case tDef (getBD t) of
                  TBitdata u (BDUnion fs) -> (u,fs)
                  _ -> panic "bdCase" ["Not a bitdata union"]
  conMap = Map.fromList [ (l, bdUniverse ?tdecls ft) | (l,ft) <- cons ]
  doAlt (l,a) =
    case Map.lookup l conMap of
      Just p  -> (p,a)
      Nothing -> panic "bdCase" ["Missing bitdata constructor: " ++ show (pp l)]




data Expr a = Extern a
              -- ^ An external expression

            | Const Integer
              -- ^ A constant for an n-bit number
              -- Should always fit in the expected type.

            | ToBits BDD.Width (Expr a)
              -- ^ Convert the expression to an n-bit number

            | Or (Expr a) (Expr a)
              -- ^ Bitwise-or of 2 n-bit numbers

            | ShiftL (Expr a) BDD.Width

