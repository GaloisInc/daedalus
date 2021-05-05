
module Talos.SymExec.StdLib (
  makeStdLib,
  -- * Model types
  tModel,
  -- * Monadic results
  tResult,
  -- * Convenience wrappers for SMT
  -- ** Syntactic helpers
  mklet,
  mklets,
  mkMatch,
  -- ** Type defs
  tSize,
  sSize,
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
  sArrayWithLength,
  sEmptyL,
  sArrayLen,
  sSelectL,
  sPushBack,
  -- ** Iterators
  tArrayIter,
  sArrayIterNew,
  sArrayIterDone,
  sArrayIterKey,
  sArrayIterVal,
  sArrayIterNext,
  -- ** Map
  tMap
  ) where

import Control.Monad (void)
import Data.Word

import SimpleSMT (SExpr, Solver)
import qualified SimpleSMT as S

-- Should be run once
makeStdLib :: Solver -> IO ()
makeStdLib s = do
  S.declareDatatype s "Maybe" ["t"] [ ("Nothing", [])
                                    , ("Just", [("fromJust", S.const "t")]) ]

  S.declareDatatype s "Tuple" ["t1", "t2"]
    [ ("mk-tuple", [("fst", S.const "t1"), ("snd", S.const "t2")]) ]

  S.declareDatatype s "Sum" ["t1", "t2"]
    [ ("inl", [("get-Left", S.const "t1")])
    , ("inr", [("get-Right", S.const "t2")])
    ]

  S.declareDatatype s "Unit"  []  [ ("unit", []) ]

  -- Define 'Byte' as a bv8 and Size as bv64
  S.ackCommand s (S.fun "define-sort" [S.const "Byte", S.List [], S.tBits 8])
  S.ackCommand s (S.fun "define-sort" [S.const "Size", S.List [], S.tBits 64])
  
  -- This has to be correct by construction, as the reason it is
  -- needed is that we can't write a polymorphic length function in
  -- smtlib
  S.declareDatatype s "ArrayWithLength" ["t"] [ ("mk-ArrayWithLength",
                                                  [ ("get-array", S.tArray tSize (S.const "t"))
                                                  , ("get-length", tSize)
                                                  ])
                                              ]

  S.declareDatatype s "ArrayIter" ["t"] [ ("mk-ArrayIter",
                                           [ ("get-arrayL", tArrayWithLength (S.const "t"))
                                           , ("get-index", tSize)
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

  S.declareDatatype s "Result" ["t"]
    [ ("failure", [])
    , ("success", [("get-result", S.const "t")])
    ]

  -- A stub for now
  S.declareDatatype s "Map" ["k", "v"]
    [ ("mk-Map", [ ("vals", S.tArray (S.const "k") (S.const "v"))
                 , ("keys", S.tArray (S.const "k") S.tBool)
                 ])
    ]

  -- Grammar operations
  void $ S.defineFun s "$get-byte" [("$bytes", tBytes)] (tResult tByte)
    $ let bytes = S.const "$bytes"
      in S.ite (S.andMany [ S.not (S.fun "is-nil" [bytes])
                          , S.fun "is-nil" [S.fun "tail" [bytes]]
                          ]) (S.fun "success" [S.fun "head" [bytes]]) (S.as (S.const "failure") (tResult tByte))

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

mkMatch :: SExpr -> [(SExpr, SExpr)] -> SExpr
mkMatch e cs =
  S.fun "match" [ e
                , S.List [ S.List [ pat, rhs ] | (pat, rhs) <- cs ]
                ]

-- -----------------------------------------------------------------------------
-- Types, both builtin and ours

tByte :: SExpr
tByte = S.const "Byte"

tBytes :: SExpr
tBytes = tList tByte

sByte :: Word8 -> SExpr
sByte = S.bvHex 8 . fromIntegral

tSize :: SExpr
tSize = S.const "Size"

sSize :: Integer -> SExpr
sSize = S.bvHex 64

tList :: SExpr -> SExpr
tList t = S.fun "List" [t]

sCons :: SExpr -> SExpr -> SExpr
sCons x xs = S.fun "insert" [x, xs]

sNil :: SExpr -> SExpr
sNil elT = S.as (S.const "nil") (tList elT)

tArrayWithLength :: SExpr -> SExpr
tArrayWithLength t = S.fun "ArrayWithLength" [t]

-- FIXME: check length somehow?
sArrayWithLength :: SExpr -> SExpr -> SExpr
sArrayWithLength arr l = S.fun "mk-ArrayWithLength" [arr, l]

sArrayLen :: SExpr -> SExpr -> SExpr
sArrayLen elTy arr = S.app (S.as (S.const "get-length") (tArrayWithLength elTy)) [arr]

sArrayL :: SExpr -> SExpr
sArrayL arr = S.fun "get-array" [arr]

sEmptyL :: SExpr -> SExpr -> SExpr
sEmptyL ty def = sArrayWithLength (S.app (S.as (S.const "const") (S.tArray tSize ty)) [def]) (sSize 0) 

sSelectL :: SExpr -> SExpr -> SExpr
sSelectL arr n = S.select (sArrayL arr) n

letUnlessAtom :: String -> SExpr -> (SExpr -> SExpr) -> SExpr
letUnlessAtom _v x@(S.Atom _) f = f x
letUnlessAtom v  x f = mklet v x (f (S.const v))

sPushBack :: SExpr -> SExpr -> SExpr -> SExpr
sPushBack elTy el arrL =
  -- FIXME: is this ok wrt clashing with other names?
  letUnlessAtom "$arrL" arrL
  $ \arrL' -> sArrayWithLength (S.store (sArrayL arrL') (sArrayLen elTy arrL') el)
                               (S.bvAdd (sArrayLen elTy arrL') (sSize 1))

-- Iterators
tArrayIter :: SExpr -> SExpr
tArrayIter t = S.fun "ArrayIter" [t]

sArrayIterNew :: SExpr -> SExpr
sArrayIterNew arr = S.fun "mk-ArrayIter" [arr, sSize 0]

sArrayIterDone :: SExpr -> SExpr -> SExpr
sArrayIterDone elTy arrI = S.bvULeq (sArrayLen elTy (S.fun "get-arrayL" [arrI])) (S.fun "get-index" [arrI])

sArrayIterKey :: SExpr -> SExpr
sArrayIterKey arrI = S.fun "get-index" [arrI]

sArrayIterVal :: SExpr -> SExpr
sArrayIterVal arrI = sSelectL (S.fun "get-arrayL" [arrI]) (sArrayIterKey arrI)

sArrayIterNext :: SExpr -> SExpr
sArrayIterNext arrI = S.fun "mk-ArrayIter" [ S.fun "get-arrayL" [arrI]
                                           , S.bvAdd (sArrayIterKey arrI) (sSize 1)
                                           ]

-- | Quote the structure of a list, given a type of its elements.  In
-- other words, a Haskell list is converted to an SMT list by
-- replacing Haskell list constructors with code to construct the
-- corresponing SMT list.
sFromList :: SExpr -> [SExpr] -> SExpr
sFromList elT = foldr sCons (sNil elT)

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

tResult :: SExpr -> SExpr
tResult t = S.fun "Result" [t]
