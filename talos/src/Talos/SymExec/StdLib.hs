
module Talos.SymExec.StdLib (makeStdLib) where

import Control.Monad.IO.Class (liftIO)

import SimpleSMT (SExpr, Solver)
import qualified SimpleSMT as S
import Control.Monad (void)

import Talos.SymExec.Monad
import Talos.SymExec.Convenience

-- Should be run once
makeStdLib :: Solver -> SolverM ()
makeStdLib s = do
  liftIO $ S.declareDatatype s "Maybe" ["t"] [ ("Nothing", [])
                                             , ("Just", [("fromJust", S.const "t")]) ]

  liftIO $ S.declareDatatype s "Tuple" ["t1", "t2"]
                                [ ("mk-tuple", [("fst", S.const "t1"), ("snd", S.const "t2")]) ]

  liftIO $ S.declareDatatype s "Sum" ["t1", "t2"]
                                [ ("inl", [("get-Left", S.const "t1")])
                                , ("inr", [("get-Right", S.const "t2")])
                                ]

  liftIO $ S.declareDatatype s "Unit"  []  [ ("unit", []) ]

  liftIO $ S.declareDatatype s "ListWithLength" ["t"]
    [ ("mk-ListWithLength", [ ("get-list", S.fun "List" [S.const "t"])
                            , ("get-length", S.tInt)
                            ])
    ]


  liftIO $ S.declareDatatype s "Config" ["t1", "t2"]
                                [ ("mk-Config", [("config-rel", S.const "t1"), ("config-res", S.const "t2")]) ]

  -- FIXME: prophecies never changes, so could be a global?
  liftIO $ S.declareDatatype s "Oracle" [] [ ("mk-oracle", [("prophecies", S.tArray S.tInt (S.tBits 32)), ("index", S.tInt)]) ]

  liftIO $ S.declareDatatype s "Input" [] [ ("mk-input", [("bytes", tList tByte), ("offset", S.tInt)]) ]
    
  liftIO $ S.declareDatatype s "State" [] [ ("mk-state", [("input", tInput), ("oracle", tOracle)]) ]
  liftIO $ S.declareDatatype s "ParserM" ["a"] [ ("fail", [])
                                         , ("success", [("state", tState), ("result", S.const "a")]) ]

  -- A stub for now
  liftIO $ S.declareDatatype s "Map" ["k", "v"]
              [ ("mk-Map", [ ("vals", S.tArray (S.const "k") (S.const "v"))
                           , ("keys", S.tArray (S.const "k") S.tBool)
                           ])
              ]

  -- Grammar operations
  void $ liftIO $ S.defineFun s "getByteP" [("$rel", tBytes), ("$res", tByte)] S.tBool
    $ let rel = S.const "$rel"
          res = S.const "$res"
      in S.andMany [ S.not (S.fun "is-nil" [rel])
                   , S.eq (S.fun "head" [rel]) res
                   , S.fun "is-nil" [S.fun "tail" [rel]]
                   ]

  -- -- Monadic operations
  -- void $ defineParser s "getByte" () tByte
  --        (do st <- getState
  --            let stream = S.fun "input" [st]
  --                bytes  = S.fun "bytes" [stream]
  --                offset = S.fun "offset" [stream]
  --            S.ite (S.fun "is-nil" [bytes])
  --              <$> sfail
  --              <*> ssuccess (S.fun "mk-state" [
  --                               S.fun "mk-input" [S.fun "tail" [bytes], S.add offset (S.int 1)]
  --                               , S.fun "oracle" [st]])
  --                           (S.fun "head" [bytes]))

  -- void $ defineParser s "getOffset" () S.tInt
  --        (do st <- getState
  --            let stream = S.fun "input" [st]
  --                offset = S.fun "offset" [stream]
  --            ssuccess st offset)

  -- -- Consults the oracle
  -- void $ defineParser s "getProphecy" () (S.tBits 32)
  --        (do st <- getState
  --            let oracle       = S.fun "oracle" [st]
  --            let prophecies   = S.fun "prophecies" [oracle]
  --            let index        = S.fun "index" [oracle]
  --            let nextProphecy = S.select prophecies index
  --            ssuccess (S.fun "mk-state" [S.fun "input" [st], S.fun "mk-oracle" [prophecies, S.add index (S.int 1)]])
  --                     nextProphecy)

  -- void $ defineParserRec s "matchBytes_" (N "bs" (tList tByte)) tUnit
  --        (\bs -> S.ite (S.fun "is-nil" [bs])
  --                     <$> spure sUnit
  --                     <*> sbind (sbind (scallP "getByte" [])
  --                                    (sguard . (S.eq (S.fun "head" [bs]))))
  --                             (\_ -> scallP "matchBytes_" [S.fun "tail" [bs]]))

  -- void $ defineParser s "end" () tUnit
  --        (do st <- getState
  --            let stream = S.fun "input" [st]         
  --            S.ite (S.fun "is-nil" [S.fun "bytes" [stream]])
  --              <$> spure sUnit
  --              <*> sfail)

  -- void $ defineFun s "getInt" [("i", tInput)] (tParserM tInt)
  --         (sBind tInt (Just "i1") (Just "x")
  --           (fun "getByte" [const "i"])
  --           (sPure (fun "bv2int"  [const "x"]) (const "i1")))


-- Get around SMT monomorphic function restriction
lookupMap :: SExpr -> SExpr -> SExpr -> SExpr
lookupMap vT m k = S.ite (S.select (S.fun "keys" [m]) k)
                      (sJust (S.select (S.fun "vals" [m]) k))
                      (sNothing vT)

