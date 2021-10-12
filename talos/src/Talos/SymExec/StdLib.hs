
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
  -- ** Numbers
  sBitVec,
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
  sArrayL,  
  sArrayLen,
  sArrayLit,  
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
  tMap,
  sMapEmpty,
  mkMapLookup,
  mkMapMember,
  mkMapInsert
  ) where

import Control.Monad (void)
import Data.Word

import SimpleSMT (SExpr, Solver, bvHex, bvBin)
import qualified SimpleSMT as S
import Control.Monad.IO.Class (MonadIO, liftIO)

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


  S.ackCommand s (S.fun "define-sort" [S.const "Map", S.List [S.const "k", S.const "v"],
                                       tList (tTuple (S.const "k") (S.const "v"))])


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

sBitVec :: Int -> Integer -> SExpr
sBitVec n i = 
  if n `mod` 4 == 0
  then bvHex (fromIntegral n) i
  else bvBin (fromIntegral n) i

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

sHead :: SExpr -> SExpr
sHead l = S.fun "head" [l]

sTail :: SExpr -> SExpr
sTail l = S.fun "tail" [l]


tArrayWithLength :: SExpr -> SExpr
tArrayWithLength t = S.fun "ArrayWithLength" [t]

-- FIXME: check length somehow?
sArrayWithLength :: SExpr -> SExpr -> SExpr -> SExpr
sArrayWithLength elTy arr l =
  S.app (S.as (S.const "mk-ArrayWithLength") (tArrayWithLength elTy)) [arr, l]

sArrayLen :: SExpr -> SExpr -> SExpr
sArrayLen elTy arr = S.fun "get-length" [arr]
-- sArrayLen :: SExpr -> SExpr -> SExpr
-- sArrayLen elTy arr = S.app (S.as (S.const "get-length") (tArrayWithLength elTy)) [arr]

sArrayL :: SExpr -> SExpr
sArrayL arr = S.fun "get-array" [arr]

sEmptyL :: SExpr -> SExpr -> SExpr
sEmptyL ty def = sArrayWithLength ty (S.app (S.as (S.const "const") (S.tArray tSize ty)) [def]) (sSize 0) 

sSelectL :: SExpr -> SExpr -> SExpr
sSelectL arr n = S.select (sArrayL arr) n

letUnlessAtom :: String -> SExpr -> (SExpr -> SExpr) -> SExpr
letUnlessAtom _v x@(S.Atom _) f = f x
letUnlessAtom v  x f = mklet v x (f (S.const v))

sPushBack :: SExpr -> SExpr -> SExpr -> SExpr
sPushBack elTy el arrL =
  -- FIXME: is this ok wrt clashing with other names?
  letUnlessAtom "$arrL" arrL
  $ \arrL' -> sArrayWithLength elTy (S.store (sArrayL arrL') (sArrayLen elTy arrL') el)
                                    (S.bvAdd (sArrayLen elTy arrL') (sSize 1))

sArrayLit :: SExpr -> SExpr -> [SExpr] -> SExpr
sArrayLit elTy def els =
  let empty = S.app (S.as (S.const "const") (S.tArray tSize elTy)) [def]
  in sArrayWithLength elTy (foldr (\(i, el) arr -> S.store arr (sSize i) el)
                                  empty (zip [0..] els))
                       (sSize (fromIntegral $ length els))

-- Iterators
tArrayIter :: SExpr -> SExpr
tArrayIter t = S.fun "ArrayIter" [t]

sArrayIterNew :: SExpr -> SExpr
sArrayIterNew arr = S.fun "mk-ArrayIter" [arr, sSize 0]

sArrayIterDone :: SExpr -> SExpr -> SExpr
sArrayIterDone elTy arrI = S.bvULeq (sArrayLen elTy
                                      (S.app (S.as (S.const "get-arrayL") (tArrayWithLength elTy)) [arrI])) (S.fun "get-index" [arrI])

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

sJust :: SExpr -> SExpr -> SExpr
sJust ty v = S.app (S.as (S.const "Just") (tMaybe ty)) [v]

tTuple :: SExpr -> SExpr -> SExpr
tTuple t1 t2 = S.fun "Tuple" [t1, t2]

sTuple :: SExpr -> SExpr -> SExpr
sTuple v1 v2 = S.fun "mk-tuple" [v1, v2]

sFst :: SExpr -> SExpr
sFst t = S.fun "fst" [t]

sSnd :: SExpr -> SExpr
sSnd t = S.fun "snd" [t]


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

-- ----------------------------------------------------------------------------------------
-- Map operations
--
-- List implementation

tMap :: SExpr -> SExpr -> SExpr
tMap kt vt = S.fun "Map" [kt, vt]

sMapEmpty :: SExpr -> SExpr -> SExpr
sMapEmpty kt vt = sNil (tTuple kt vt)

-- (define-fun-rec BLookup ((m (Map BString BString)) (k BString)) (Maybe BString)
--   (ite (= m (as nil (Map BString BString))) (as Nothing (Maybe BString)) (ite (= (fst (head m)) k) (Just (snd (head m))) (BLookup (tail m) k))))

mkMapLookup :: MonadIO m => Solver -> String -> SExpr -> SExpr -> m ()
mkMapLookup s fnm kt vt = do
  liftIO $ S.defineFunsRec s [(fnm, [("m", tMap kt vt), ("k", kt)], tMaybe vt, body)]
  where
    m = S.const "m"
    k = S.const "k"
    body = S.ite (S.eq m (sNil (tTuple kt vt)))
                 (sNothing vt)
                 (S.ite (S.eq k (sFst (sHead m))) (sJust vt (sSnd (sHead m))) (S.fun fnm [sTail m, k]))

mkMapMember :: MonadIO m => Solver -> String -> SExpr -> SExpr -> m ()
mkMapMember s fnm kt vt = do
  liftIO $ S.defineFunsRec s [(fnm, [("m", tMap kt vt), ("k", kt)], S.tBool , body)]
  where
    m = S.const "m"
    k = S.const "k"
    body = S.ite (S.eq m (sNil (tTuple kt vt)))
                 (S.bool False)
                 (S.ite (S.eq k (sFst (sHead m))) (S.bool True) (S.fun fnm [sTail m, k]))

mkMapInsert :: MonadIO m => Solver -> String -> SExpr -> SExpr -> m ()
mkMapInsert s fnm kt vt = do
  liftIO $ S.defineFunsRec s [(fnm, [("m", tMap kt vt), ("k", kt), ("v", vt)], tMap kt vt, body)]
  where
    m = S.const "m"
    k = S.const "k"
    v = S.const "v"
    
    body = sCons (sTuple k v) m
