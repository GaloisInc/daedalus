{-# Language KindSignatures #-}
module RTS.ParserVM where

import Data.IntSet(IntSet)
import qualified Data.IntSet as Set

import RTS.Input

-- | A direct parser.  Used for parsers that do not use unbiased chocie.
type DParser m a    = ParserErrorState -> m (Maybe (a,Input), ParserErrorState)

-- | A continuation parser.  Used for parsers that use unbiased choice.
type CParser r m a  = YesCont r m a -> NoCont r m -> Code r m

data Thread r m = Thread
  { tId   :: Int
  , tCode :: Bool -> Code r m
  }

type Code r m       = ThreadState r m -> m (ThreadState r m)

type YesCont r m a  = a -> Input -> Code r m
type NoCont r m     = Code r m

data ThreadState r m = ThreadState
  { thrNextThreadId  :: !Int
  , thrNotified      :: !IntSet
  , thrStack         :: [Thread r m]
  , thrResults       :: [r]
  , thrErrors        :: ParserErrorState
  }

data ParserErrorState = ParserErrors

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




