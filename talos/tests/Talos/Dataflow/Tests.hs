module Talos.Dataflow.Tests where

import Talos.Dataflow
import Test.HUnit
import Daedalus.GUID (firstValidGUID)
import Daedalus.Core.Basics
import Daedalus.Core.Expr

import Data.Text
import Data.Map (Map)
import qualified Data.Map as Map

tests :: Test
tests = TestList testData

data TD a = TD
  { name     :: String
  , input    :: DataflowState
  , expected :: (Either ErrorMsg a, DataflowState)
  , dsp       :: DSP a
  }

mkTest :: (Eq a, Show a) => TD a -> Test
mkTest td =
  name td ~: exp ~=? act
  where
    exp = expected td
    act = runDSP (dsp td) (input td)

fn = FName
  { fnameId = firstValidGUID
  , fnameText = pack "fn"
  , fnamePublic = True
  , fnameType = TBool
  , fnameMod = MName { mNameText = pack "mod" }
  }

simpleVState = VariableAbstractState 
 { constrainedness = Unconstrained
 , fromStream = False
 }

simpleState = DataflowState
  { fname = Just fn
  , vmap = Map.fromList [(firstValidGUID, Map.fromList [(firstValidGUID, simpleVState)])]
  }

testData = 
  [ mkTest TD 
    { name = "var success"
    , input = simpleState
    , expected = (Right simpleVState, simpleState)
    , dsp = dataflowExpr $ Var Name { nameId = firstValidGUID, nameText = Just $ pack "v", nameType = TBool }
    }
  , mkTest TD 
    { name = "var failure"
    , input = emptyDataflowState { fname = Just fn }
    , expected = (Left "Error: undeclared variable: GUID {getGUID = 0}", emptyDataflowState { fname = Just fn })
    , dsp = dataflowExpr $ Var Name { nameId = firstValidGUID, nameText = Just $ pack "v", nameType = TBool }
    }
  ]
