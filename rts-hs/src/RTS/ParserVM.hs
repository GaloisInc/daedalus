{-# Language KindSignatures, DataKinds #-}
module RTS.ParserVM where

import Data.Text(Text)
import Data.IntSet(IntSet)
import qualified Data.IntSet as Set

import qualified RTS.Input as RTS
import qualified RTS.Vector as RTS
import qualified RTS.Numeric as RTS
import qualified RTS.ParserAPI as PAPI

-- | A direct parser.  Used for parsers that do not use unbiased chocie.
type DParser m a = ParserErrorState -> m (Maybe (a,RTS.Input), ParserErrorState)

-- | A continuation parser.  Used for parsers that use unbiased choice.
type CParser r m a  = YesCont r m a -> NoCont r m -> Code r m

data Thread r m = Thread
  { tId   :: Int
  , tCode :: Bool -> Code r m
  }

type Code r m       = ThreadState r m -> m (ThreadState r m)

type YesCont r m a  = a -> RTS.Input -> Code r m
type NoCont r m     = Code r m

data ThreadState r m = ThreadState
  { thrNextThreadId  :: !Int
  , thrNotified      :: !IntSet
  , thrStack         :: [Thread r m]
  , thrResults       :: [r]
  , thrErrors        :: ParserErrorState
  }

data ParserErrorState = ParserErrorState -- XXX

vmNoteFail ::
  PAPI.ParseErrorSource ->
  Text ->
  RTS.Input ->
  RTS.Vector (RTS.UInt 8) ->
  ParserErrorState ->
  ParserErrorState
vmNoteFail = undefined

vmPushDebugTail :: Text -> ParserErrorState -> ParserErrorState
vmPushDebugTail = undefined

vmPushDebugCall :: Text -> ParserErrorState -> ParserErrorState
vmPushDebugCall = undefined

vmPopDebug :: ParserErrorState -> ParserErrorState
vmPopDebug = undefined

vmOutput :: Applicative m => r -> Code r m
vmOutput r = \s -> pure s { thrResults = r : thrResults s }

vmNotify :: Applicative m => Int -> Code r m
vmNotify tid = \s -> pure s { thrNotified = Set.insert tid (thrNotified s) }

topContinue :: Applicative m => Code r m
topContinue s =
  case thrStack s of
    [] -> pure s
    Thread tid code : more ->
      let n  = tid `Set.member` thrNotified s
          s1 = s { thrResults  = thrResults s
                 , thrNotified = Set.delete tid (thrNotified s)
                 , thrStack    = more
                 }
      in n `seq` code n s1

topYesCont :: Applicative m => YesCont r m r
topYesCont r _ = \s -> topContinue s { thrResults = r : thrResults s }

topNoCont :: Applicative m => NoCont r m
topNoCont = topContinue




