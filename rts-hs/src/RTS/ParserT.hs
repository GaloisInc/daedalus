{-# LANGUAGE BlockArguments, RecordWildCards, RankNTypes #-}
module RTS.ParserT (ParserT, runParserT, inBase) where

import Control.Monad
import Data.List.NonEmpty(NonEmpty(..))

import RTS.Input
import RTS.ParserAPI


data S = S [String] {-# UNPACK #-} !Input

data IList a = ICons a {-# UNPACK #-} !S (IList a)
             | INil

iappend :: IList a -> IList a -> IList a
iappend xs ys =
  case xs of
    INil -> ys
    ICons a i zs -> ICons a i (iappend zs ys)

itoList :: IList a -> [a]
itoList xs =
  case xs of
    INil -> []
    ICons a _ bs -> a : itoList bs


-- A simple list monad, where we don't e.g. interleave on alternatives
newtype ParserT m a = Parser { runP :: forall r. Res (S -> m r) a }

type Res r a =
  {-NoResAbort-} (ParseError -> r) ->
  {-NoResFail-}  (ParseError -> r) ->
  {-Res-}        (a -> r) ->
  {-MultiRes-}   (a -> IList a -> r) ->
  r

joinIList :: (a -> ParserT m b) -> ParserT m b -> IList a -> ParserT m b
joinIList k done xs =
  case xs of
    INil         -> done
    ICons a i ys -> joinRes2 done i (joinIList k (k a) ys)


joinRes2 :: ParserT m a -> S -> ParserT m a -> ParserT m a
joinRes2 m1 i2 m2 = Parser
  \rAbort rFail rOk rMany ->
  let leftFailed e1 _ =
        runP m2 rAbort
           do \e2 -> rFail $! mergeErrors e1 e2
           rOk
           rMany
           i2

      leftOk a i =
        runP m2 rAbort
           do \_ _    -> rOk a i
           do \b j    -> rMany a (ICons b j INil) i
           do \b xs j -> rMany a (ICons b j xs) i
           i2

      leftMany a xs i =
        runP m2 rAbort
           do \_ _    -> rMany a xs i
           do \b j    -> rMany a (ICons b j xs) i
           do \b ys j -> rMany a (ICons b j (iappend xs ys)) i
           i2

  in runP m1 rAbort leftFailed leftOk leftMany
{-# INLINE joinRes2 #-}



mergeErrors :: ParseError -> ParseError -> ParseError
mergeErrors e1 e2 = if peOffset e1 <= peOffset e2 then e2 else e1
{-# INLINE mergeErrors #-}


instance Functor (ParserT m) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (ParserT m) where
  pure v = Parser \_rAbort _rFail rOk _rMany -> rOk v
  (<*>)  = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad (ParserT m) where
  Parser p >>= k = Parser
    \rAbort rFail rOk rMore ->
    p rAbort
      rFail
      do \a    -> runP (k a)
                  rAbort rFail rOk rMore
      do \a xs -> runP (joinIList k (k a) xs)
                  rAbort rFail rOk rMore
  {-# INLINE (>>=)  #-}


runParserT :: Applicative m => ParserT m a -> Input -> m (Result a)
runParserT p = f . S []
  where
  f = runP p do \e _    -> pure (NoResults e)
             do \e _    -> pure (NoResults e)
             do \a _    -> pure (Results (a :| []))
             do \a xs _ -> pure (Results (a :| itoList xs))
{-# INLINE runParserT #-}

getS :: ParserT m S
getS = Parser \_ _ rOk _ -> \s -> rOk s s
{-# INLINE getS #-}

setS :: S -> ParserT m ()
setS s = Parser \_ _ rOk _ -> \_ -> rOk () s
{-# INLINE setS #-}

inBase :: Monad m => m a -> ParserT m a
inBase m = Parser \_ _ rOk _ -> \s -> m >>= \a -> rOk a s
{-# INLINE inBase #-}

instance BasicParser (ParserT m) where
  pByte rng =
    do Parser \_rAbort rFail rOk _rMany ->
         \s@(S cs inp) ->
         case inputByte inp of
           Just (x,xs) -> rOk x (S cs xs)
           Nothing     -> rFail PE { peInput = inp
                                   , peGrammar =  [rng]
                                   , peMsg =  msg
                                   , peSource = FromSystem
                                   , peStack = cs
                                   , peMore = Nothing
                                   }
                                s
             where msg = "unexpected end of input"

  pPeek = do S _ i <- getS
             pure i
  {-# INLINE pPeek #-}

  pSetInput i = do S cs _ <- getS
                   setS (S cs i)
  {-# INLINE pSetInput #-}


  pEnter x p =
    do S cs i <- getS
       setS (S (x:cs) i)
       a <- p
       S _ j <- getS
       setS (S cs j)
       pure a
  {-# INLINE pEnter #-}

  pStack     = do S cs _ <- getS
                  pure cs
  {-# INLINE pStack #-}

  p <|| q = Parser \iA iF iO iM ->
            \i ->
            runP p iA
                   do \_ _ -> runP q iA iF iO iM i
                   iO
                   iM
                   i
  {-# INLINE (<||) #-}

  p ||| q = do s <- getS
               joinRes2 p s q
  {-# INLINE (|||) #-}

  pFail e = Parser \_ rFail _ _ -> rFail e
  {-# INLINE pFail #-}

  pEnd r =
    do inp <- pPeek
       if inputEmpty inp
         then pure ()
         else pErrorAt FromSystem [r] inp "unexpected left over input"
  {-# INLINE pEnd #-}

  pOffset = inputOffset <$> pPeek
  {-# INLINE pOffset #-}

  pMatch1 erng (ClassVal p str) =
    do inp <- pPeek
       b   <- pByte erng
       let byChar = toEnum (fromEnum b) :: Char
       unless (p b)
         $ pErrorAt FromSystem [erng] inp
         $ unwords ["byte", show byChar, "does not match", str]
       pure b
  {-# INLINE pMatch1 #-}


  pErrorMode em (Parser p) =
    case em of
      Abort -> Parser \iA _ iO iM -> p iA iA iO iM
      Fail  -> Parser \_ iF iO iM -> p iF iF iO iM
  {-# INLINE pErrorMode #-}


