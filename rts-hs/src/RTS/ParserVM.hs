{-# Language KindSignatures, DataKinds, RankNTypes #-}
module RTS.ParserVM where

import Data.Text(Text)
import Data.IntSet(IntSet)
import qualified Data.IntSet as Set
import Data.Functor.Identity
import Data.Coerce(coerce)

import qualified RTS.Input as RTS
import qualified RTS.Vector as RTS
import qualified RTS.Numeric as RTS
import qualified RTS.ParserAPI as PAPI

-- | A direct parser.  Used for parsers that do not use unbiased chocie.
-- No custom user monad
type DParser a = ParserErrorState -> (Maybe (a,RTS.Input), ParserErrorState)

-- | A direct parser.  Used for parsers that do not use unbiased chocie.
type DParserM m a = ParserErrorState -> m (Maybe (a,RTS.Input), ParserErrorState)


-- | A continuation parser.  Used for parsers that use unbiased choice.
-- No custom user monad
type CParser a = CParserM Identity a

-- | A continuation parser.  Used for parsers that use unbiased choice.
type CParserM m a   = forall r. NoCont r m -> YesCont r m a -> Code r m

type Thread r m     = Bool -> Code r m

type Code r m       = ThreadState r m -> m (ThreadState r m)
type YesCont r m a  = a -> RTS.Input -> Code r m
type NoCont r m     = Code r m

data ThreadState r m = ThreadState
  { thrNotified      :: !IntSet
  , thrStack         :: [Thread r m]
  , thrThreadNum     :: !Int    -- ^ Number of live threads, used to assign ids.
  , thrResults       :: [r]
  , thrErrors        :: ParserErrorState
  }

data ParserErrorState = ParserErrorState
  { pesCallStack :: [[Text]]
  , pesError     :: Maybe ParseError
  }

data ParseError = ParseError
  { peSource :: PAPI.ParseErrorSource
  , peLoc    :: Text
  , peInput  :: RTS.Input
  , peMsg    :: RTS.Vector (RTS.UInt 8)
  , peStack  :: [[Text]]
  } deriving Show

thrUpdateErrors ::
  (ParserErrorState -> ParserErrorState) -> ThreadState r m -> ThreadState r m
thrUpdateErrors f s = s { thrErrors = f (thrErrors s) }


vmNoteFail ::
  PAPI.ParseErrorSource ->
  Text ->
  RTS.Input ->
  RTS.Vector (RTS.UInt 8) ->
  ParserErrorState ->
  ParserErrorState
vmNoteFail ty loc inp msg s =
  case pesError s of
    Nothing -> s { pesError = Just newErr }
    Just e  -> s { pesError = Just (improve e) }
  where
  newErr = ParseError
             { peSource = ty
             , peLoc    = loc
             , peInput  = inp
             , peMsg    = msg
             , peStack  = pesCallStack s
             }

  improve old =
    case (peSource old, ty) of
      (PAPI.FromUser, PAPI.FromSystem) -> old
      (PAPI.FromSystem, PAPI.FromUser) -> newErr
      _ | RTS.inputOffset inp < RTS.inputOffset (peInput old) -> old
        | otherwise -> newErr


vmPushDebugTail :: Text -> ParserErrorState -> ParserErrorState
vmPushDebugTail t s =
  case pesCallStack s of
    xs : more -> s { pesCallStack = (t : xs) : more }
    []        -> s { pesCallStack = [[t]] }

vmPushDebugCall :: Text -> ParserErrorState -> ParserErrorState
vmPushDebugCall t s = s { pesCallStack = [t] : pesCallStack s }

vmPopDebug :: ParserErrorState -> ParserErrorState
vmPopDebug s = case pesCallStack s of
                _ : xs -> s { pesCallStack = xs }
                _      -> s

vmOutput :: r -> ThreadState r m -> ThreadState r m
vmOutput r s = s { thrResults = r : thrResults s }

vmNotify :: Int -> ThreadState r m -> ThreadState r m
vmNotify tid s = s { thrNotified = Set.insert tid (thrNotified s) }

vmSpawn :: (Bool -> Code r m) -> ThreadState r m -> (Int,ThreadState r m)
vmSpawn code s = (tid, newS)
  where
  tid  = thrThreadNum s
  newS = s { thrThreadNum = tid + 1
           , thrStack = code : thrStack s
           }

vmIsNotified :: Int -> ThreadState r m -> Bool
vmIsNotified tid s = tid `Set.member` thrNotified s

vmYield :: Applicative m => Code r m
vmYield s =
  case thrStack s of
    [] -> pure s
    code : more ->
      let tid  = thrThreadNum s - 1
          note = thrNotified s
          n    = tid `Set.member` thrNotified s
          s1   = s { thrThreadNum = tid
                   , thrStack     = more
                   , thrNotified  = if n then Set.delete tid note else note
                   }
      in code n s1

--------------------------------------------------------------------------------
-- Running parsers

topNoK :: Applicative m => NoCont r m
topNoK = vmYield

topYesOneK :: Applicative m => YesCont r m r
topYesOneK a _ s = pure (vmOutput a s)

topYesAllK :: Applicative m => YesCont r m r
topYesAllK a _ s = vmYield (vmOutput a s)


-- | Initial state for direct parsers.
initParseErrorState :: ParserErrorState
initParseErrorState = ParserErrorState
  { pesCallStack = []
  , pesError     = Nothing
  }

-- | Initial state for continuation parsers.
initThreadState :: ThreadState r m
initThreadState = ThreadState
  { thrNotified   = Set.empty
  , thrStack      = []
  , thrThreadNum  = 0
  , thrResults    = []
  , thrErrors     = initParseErrorState
  }


runDParserM :: Functor m => DParserM m a -> m (Either ParseError a)
runDParserM p = dparserToEither <$> p initParseErrorState

runDParser :: DParser a -> Either ParseError a
runDParser p = dparserToEither (p initParseErrorState)

runCParserOne :: CParser a -> Either ParseError a
runCParserOne p = coerce (runCParserOneM p)

runCParserAll :: CParser a -> Either ParseError [a]
runCParserAll p = coerce (runCParserAllM p)

runCParserOneM :: Applicative m => CParserM m a -> m (Either ParseError a)
runCParserOneM p = done <$> p topNoK topYesOneK initThreadState
  where
  done s = case thrResults s of
            []    -> Left (doGetErr (thrErrors s))
            r : _ -> Right r

runCParserAllM :: Applicative m => CParserM m a -> m (Either ParseError [a])
runCParserAllM p = done <$> p topNoK topYesAllK initThreadState
  where
  done s = case thrResults s of
             [] -> Left (doGetErr (thrErrors s))
             rs -> Right rs

--------------------------------------------------------------------------------
dparserToEither :: (Maybe (a,RTS.Input), ParserErrorState) -> Either ParseError a
dparserToEither (res,s) =
  case res of
    Nothing -> Left (doGetErr s)
    Just (a,_) -> Right a


doGetErr :: ParserErrorState -> ParseError
doGetErr s = case pesError s of
               Just e  -> e
               Nothing -> error "doGetErr: Nothing"



