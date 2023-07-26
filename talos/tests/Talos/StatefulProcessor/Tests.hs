module Talos.StatefulProcessor.Tests where

import Talos.StatefulProcessor
import Test.HUnit

tests :: Test
tests = TestList testData

data TD s l r = TD
  { name     :: String
  , input    :: s
  , expected :: (Either l r, s)
  , sp       :: StatefulProcessor s l r
  }

mkTest :: (Eq s, Eq l, Eq r, Show s, Show l, Show r) => TD s l r -> Test
mkTest td =
  name td ~: exp ~=? act
  where
    exp = expected td
    act = runState (sp td) (input td)

testData :: [Test]
testData =
  [ mkTest (TD
    { name     = "'get' succeeds"
    , input    = 1
    , expected = (Right 1, 1)
    , sp       = get
    } :: TD Int Int Int)
  , mkTest (TD
    { name     = "'put' succeeds"
    , input    = 1
    , expected = (Right (), 2)
    , sp       = put 2
    } :: TD Int () ())
  , mkTest (TD
    { name     = "'put' overwrites"
    , input    = 1
    , expected = (Right 3, 3)
    , sp       = do
        put 2
        put 3
        get
    } :: TD Int Int Int)
  , mkTest (TD
    { name     = "'throwError' succeeds"
    , input    = 1
    , expected = (Left "error", 1)
    , sp       = throwError "error"
    } :: TD Int String ())
  , mkTest (TD
    { name     = "'throwError' propagates"
    , input    = 1
    , expected = (Left "error", 1)
    , sp       = do
        throwError "error"
        put 2
        get
    } :: TD Int String Int)
  , mkTest (TD
    { name     = "'pure' succeeds"
    , input    = 1
    , expected = (Right 2, 1)
    , sp       = pure 2
    } :: TD Int String Int)
  ]