module Daedalus.RTS.Parser where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS8
import Data.IntSet(IntSet)
import qualified Data.IntSet as Set
import Data.Functor.Identity
import Data.Coerce(coerce)
import qualified Text.PrettyPrint as PP

import qualified Daedalus.RTS.Input as RTS
import qualified Daedalus.RTS.Vector as RTS
import qualified Daedalus.RTS.Numeric as RTS

-- | A direct parser.  Used for parsers that do not use unbiased chocie.
-- No custom user monad.  No error reporting.
type DParser'    a = Maybe (a,RTS.Input)

-- | A direct parser.  Used for parsers that do not use unbiased chocie.
-- No error reporting.
type DParserM' m a = m (Maybe (a,RTS.Input))

-- | A direct parser.  Used for parsers that do not use unbiased chocie.
-- No custom user monad
type DParser a = ParserErrorState -> (Maybe (a,RTS.Input), ParserErrorState)

-- | A direct parser.  Used for parsers that do not use unbiased chocie.
type DParserM m a = ParserErrorState -> m (Maybe (a,RTS.Input), ParserErrorState)

liftD :: Functor m => m a -> RTS.Input -> DParserM m a
liftD m i s = mk <$> m
  where mk a = (Just (a,i), s)

liftD' :: Functor m => m a -> RTS.Input -> DParserM' m a
liftD' m i = mk <$> m
  where mk a = Just (a,i)

-- | A continuation parser.  Used for parsers that use unbiased choice.
-- No custom user monad
type CParser r a    = CParserM r Identity a

-- | A continuation parser.  Used for parsers that use unbiased choice.
type CParserM r m a = NoCont r m -> YesCont r m a -> Code r m

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

data ParseErrorSource = FromUser | FromSystem
  deriving Show

data ParseError = ParseError
  { peSource :: ParseErrorSource
  , peLoc    :: Text
  , peInput  :: RTS.Input
  , peMsg    :: RTS.Vector (RTS.UInt 8)
  , peStack  :: [[Text]]
  } deriving Show

thrUpdateErrors ::
  (ParserErrorState -> ParserErrorState) -> ThreadState r m -> ThreadState r m
thrUpdateErrors f s = s { thrErrors = f (thrErrors s) }
{-# INLINE thrUpdateErrors #-}

vmNoteFail ::
  ParseErrorSource ->
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
    case compare (RTS.inputOffset inp) (RTS.inputOffset (peInput old)) of
      LT -> old
      GT -> newErr
      EQ -> case (peSource old, ty) of
              (FromUser, FromSystem) -> old
              _ -> newErr


vmPushDebugTail :: Text -> ParserErrorState -> ParserErrorState
vmPushDebugTail t s =
  case pesCallStack s of
    xs : more -> s { pesCallStack = (t : xs) : more }
    []        -> s { pesCallStack = [[t]] }
{-# INLINE vmPushDebugTail #-}

vmPushDebugCall :: Text -> ParserErrorState -> ParserErrorState
vmPushDebugCall t s = s { pesCallStack = [t] : pesCallStack s }
{-# INLINE vmPushDebugCall #-}

vmPopDebug :: ParserErrorState -> ParserErrorState
vmPopDebug s = case pesCallStack s of
                _ : xs -> s { pesCallStack = xs }
                _      -> s
{-# INLINE vmPopDebug #-}

vmOutput :: r -> ThreadState r m -> ThreadState r m
vmOutput r s = s { thrResults = r : thrResults s }
{-# INLINE vmOutput #-}

vmNotify :: Int -> ThreadState r m -> ThreadState r m
vmNotify tid s = s { thrNotified = Set.insert tid (thrNotified s) }
{-# INLINE vmNotify #-}

vmSpawn :: (Bool -> Code r m) -> ThreadState r m -> (Int,ThreadState r m)
vmSpawn code s = newS `seq` (tid, newS)
  where
  tid  = thrThreadNum s
  newS = s { thrThreadNum = tid + 1
           , thrStack = code : thrStack s
           }
{-# INLINE vmSpawn #-}

vmIsNotified :: Int -> ThreadState r m -> Bool
vmIsNotified tid s = tid `Set.member` thrNotified s
{-# INLINE vmIsNotified #-}

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
      in code n $! s1

--------------------------------------------------------------------------------
-- Running parsers

topNoK :: Applicative m => NoCont r m
topNoK = vmYield
{-# INLINE topNoK #-}

topYesOneK :: Applicative m => YesCont r m r
topYesOneK a _ s = pure $! vmOutput a s
{-# INLINE topYesOneK #-}

topYesAllK :: Applicative m => YesCont r m r
topYesAllK a _ s = vmYield $! vmOutput a s
{-# INLINE topYesAllK #-}


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

runDParserM' :: Functor m => DParserM' m a -> m (Maybe a)
runDParserM' p = dparserToMaybe <$> p

runDParser' :: DParser' a -> Maybe a
runDParser' = dparserToMaybe

runCParserOne :: CParser a a -> Either ParseError a
runCParserOne p = coerce (runCParserOneM p)

runCParserAll :: CParser a a -> Either ParseError [a]
runCParserAll p = coerce (runCParserAllM p)

runCParserOneM :: Applicative m => CParserM a m a -> m (Either ParseError a)
runCParserOneM p = done <$> p topNoK topYesOneK initThreadState
  where
  done s = case thrResults s of
             []    -> Left (doGetErr (thrErrors s))
             r : _ -> Right r

runCParserAllM :: Applicative m => CParserM a m a -> m (Either ParseError [a])
runCParserAllM p = done <$> p topNoK topYesAllK initThreadState
  where
  done s = case thrResults s of
             [] -> Left (doGetErr (thrErrors s))
             rs -> Right rs

--------------------------------------------------------------------------------
dparserToEither ::
  (Maybe (a,RTS.Input), ParserErrorState) -> Either ParseError a
dparserToEither (res,s) =
  case res of
    Nothing -> Left (doGetErr s)
    Just (a,_) -> Right a


doGetErr :: ParserErrorState -> ParseError
doGetErr s = case pesError s of
               Just e  -> e
               Nothing -> error "doGetErr: Nothing"

dparserToMaybe :: DParser' a -> Maybe a
dparserToMaybe = fmap fst

--------------------------------------------------------------------------------

ppParseError :: ParseError -> PP.Doc
ppParseError pe =
  PP.vcat
    [ "Parse error at" PP.<+> PP.text (show (RTS.inputName inp))
                       PP.<> ":" PP.<> PP.int (RTS.inputOffset inp) PP.<> ":"
    , PP.nest 2 $ PP.vcat
        [ PP.text (BS8.unpack (RTS.vecToRep (peMsg pe)))
        , "Location:" PP.<+> ppText (peLoc pe)
        , "Call stack:"
        , PP.nest 2 $ PP.vcat $ map ppFun (peStack pe)
        ]
    ]
  where
  inp      = peInput pe
  ppText t = PP.text (Text.unpack t)
  ppFun xs = PP.hsep (map ppText xs)


