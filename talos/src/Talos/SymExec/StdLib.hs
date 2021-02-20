
module Talos.SymExec.StdLib (
  makeStdLib,
  -- * Model types
  tModel,  
  -- * Convenience wrappers for SMT
  -- ** Syntactic helpers
  mklet,
  mklets,  
  -- ** Bytes
  tByte,
  sByte,
  tBytes,
  -- ** Unit
  tUnit,
  sUnit,
  -- ** Maybe
  tMaybe,
  sJust,
  sNothing,
  -- ** Pairs
  tTuple,
  sTuple,
  sFst,
  sSnd,
  -- ** Sums
  tSum,
  sInl,
  sInr,
  sIsLeft,
  sGetLeft,
  sIsRight,
  sGetRight,
  -- ** List
  tList,
  sCons,
  sNil,
  sFromList,
  -- ** List with their length (not checked)
  tArrayWithLength,
  sArrayLen,
  sSelectL,
  -- ** Map
  tMap
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Word

import SimpleSMT (SExpr)
import qualified SimpleSMT as S

import Talos.SymExec.Monad
-- import Talos.SymExec.Convenience

-- Should be run once
makeStdLib :: SymExecM ()
makeStdLib = withSolver $ \s -> liftIO $ do
  S.declareDatatype s "Maybe" ["t"] [ ("Nothing", [])
                                    , ("Just", [("fromJust", S.const "t")]) ]

  S.declareDatatype s "Tuple" ["t1", "t2"]
    [ ("mk-tuple", [("fst", S.const "t1"), ("snd", S.const "t2")]) ]

  S.declareDatatype s "Sum" ["t1", "t2"]
    [ ("inl", [("get-Left", S.const "t1")])
    , ("inr", [("get-Right", S.const "t2")])
    ]

  S.declareDatatype s "Unit"  []  [ ("unit", []) ]
  
  -- This has to be correct by construction, as the reason it is
  -- needed is that we can't write a polymorphic length function in
  -- smtlib
  S.declareDatatype s "ArrayWithLength" ["t"] [ ("mk-ArrayWithLength",
                                                  [ ("get-array", S.tArray S.tInt (S.const "t"))
                                                  , ("get-length", S.tInt)
                                                  ])
                                              ]

  -- The generic type of parse trees/paths
  S.declareDatatype s "Model" []
    [ ("bytes",   [("get-bytes", tList tByte)])
    , ("munit",    [])
    , ("indexed",  [("index", S.tInt), ("get-body", S.const "Model")])
    -- , ("seq",     [("count", S.tInt), ("values", S.tArray S.tInt (S.const "Model"))])
    , ("seq",     [("mfst", S.const "Model"), ("msnd", S.const "Model")])
    ]

  -- A stub for now
  S.declareDatatype s "Map" ["k", "v"]
    [ ("mk-Map", [ ("vals", S.tArray (S.const "k") (S.const "v"))
                 , ("keys", S.tArray (S.const "k") S.tBool)
                 ])
    ]

  -- Grammar operations
  void $ S.defineFun s "getByteP" [("$rel", tBytes), ("$res", tByte)] S.tBool
    $ let rel = S.const "$rel"
          res = S.const "$res"
      in S.andMany [ S.not (S.fun "is-nil" [rel])
                   , S.eq (S.fun "head" [rel]) res
                   , S.fun "is-nil" [S.fun "tail" [rel]]
                   ]

-- Get around SMT monomorphic function restriction
-- lookupMap :: SExpr -> SExpr -> SExpr -> SExpr
-- lookupMap vT m k = S.ite (S.select (S.fun "keys" [m]) k)
--                       (sJust (S.select (S.fun "vals" [m]) k))
--                       (sNothing vT)





tUnit :: SExpr
tUnit = S.const "Unit"

sUnit :: SExpr
sUnit = S.const "unit"

-- -----------------------------------------------------------------------------
-- Helpers

-- | Emit an SMT let expression
mklet :: String -> SExpr -> SExpr -> SExpr
mklet v e b = S.fun "let" [S.List [S.List [S.const v, e]], b]

mklets :: [(String, SExpr)] -> SExpr -> SExpr
mklets [] b = b
mklets xs b =
  S.fun "let" [S.List (map (\(v, e) -> S.List [S.const v, e]) xs), b]

-- -----------------------------------------------------------------------------
-- Types, both builtin and ours

sByte :: Word8 -> SExpr
sByte = S.bvHex 8 . fromIntegral

tList :: SExpr -> SExpr
tList t = S.fun "List" [t]

sCons :: SExpr -> SExpr -> SExpr
sCons x xs = S.fun "insert" [x, xs]

sNil :: SExpr -> SExpr
sNil elT = S.as (S.const "nil") (tList elT)

tArrayWithLength :: SExpr -> SExpr
tArrayWithLength t = S.fun "ArrayWithLength" [t]

sArrayLen :: SExpr -> SExpr
sArrayLen arr = S.fun "get-length" [arr]

sSelectL :: SExpr -> SExpr -> SExpr
sSelectL arr n = S.select (S.fun "get-array" [arr]) n

-- | Quote the structure of a list, given a type of its elements.  In
-- other words, a Haskell list is converted to an SMT list by
-- replacing Haskell list constructors with code to construct the
-- corresponing SMT list.
sFromList :: SExpr -> [SExpr] -> SExpr
sFromList elT = foldr sCons (sNil elT)

tByte :: SExpr
tByte = S.tBits 8

tBytes :: SExpr
tBytes = tList tByte

tMaybe :: SExpr -> SExpr
tMaybe t = S.fun "Maybe" [t]

sNothing :: SExpr -> SExpr
sNothing t = S.as (S.const "Nothing") (tMaybe t)

sJust :: SExpr -> SExpr
sJust v = S.fun "Just" [v]

tTuple :: SExpr -> SExpr -> SExpr
tTuple t1 t2 = S.fun "Tuple" [t1, t2]

sTuple :: SExpr -> SExpr -> SExpr
sTuple v1 v2 = S.fun "mk-tuple" [v1, v2]

sFst :: SExpr -> SExpr
sFst t = S.fun "fst" [t]

sSnd :: SExpr -> SExpr
sSnd t = S.fun "snd" [t]

tMap :: SExpr -> SExpr -> SExpr
tMap kt vt = S.fun "Map" [kt, vt]

tSum :: SExpr -> SExpr -> SExpr
tSum lt rt = S.fun "Sum" [lt, rt]

sInl :: SExpr -> SExpr -> SExpr -> SExpr
sInl lt rt v = S.as (S.fun "Left" [v]) (tSum lt rt)

sInr :: SExpr -> SExpr -> SExpr -> SExpr
sInr lt rt v = S.as (S.fun "Right" [v]) (tSum lt rt)

sIsLeft :: SExpr -> SExpr
sIsLeft v = S.fun "is-Left" [v]

sIsRight :: SExpr -> SExpr
sIsRight v = S.fun "is-Right" [v]

sGetLeft :: SExpr -> SExpr
sGetLeft v = S.fun "get-Left" [v]

sGetRight :: SExpr -> SExpr
sGetRight v = S.fun "get-Right" [v]

tModel :: SExpr
tModel = S.const "Model"
