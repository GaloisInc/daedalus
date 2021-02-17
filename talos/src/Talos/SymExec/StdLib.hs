
module Talos.SymExec.StdLib (makeStdLib) where

import Control.Monad.IO.Class (liftIO)

import SimpleSMT (Solver)
import qualified SimpleSMT as S
import Control.Monad (void)

import Talos.SymExec.Monad
-- import Talos.SymExec.Convenience

-- Should be run once
makeStdLib :: Solver -> SolverM ()
makeStdLib s = liftIO $ do
  S.declareDatatype s "Maybe" ["t"] [ ("Nothing", [])
                                    , ("Just", [("fromJust", S.const "t")]) ]

  S.declareDatatype s "Tuple" ["t1", "t2"]
    [ ("mk-tuple", [("fst", S.const "t1"), ("snd", S.const "t2")]) ]

  S.declareDatatype s "Sum" ["t1", "t2"]
    [ ("inl", [("get-Left", S.const "t1")])
    , ("inr", [("get-Right", S.const "t2")])
    ]

  S.declareDatatype s "Unit"  []  [ ("unit", []) ]
  
  -- The generic type of parse trees/paths
  S.declareDatatype s "Model" []
    [ ("bytes",   [("get-bytes", tList tByte)])
    , ("munit",    [])
    , ("branch",  [("index", S.tInt), ("get-branch", S.const "Model")])
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

