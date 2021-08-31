{-# LANGUAGE BlockArguments, RecordWildCards #-}
module RTS.Parser (Parser, runParser) where

import Control.Monad
import Data.List.NonEmpty(NonEmpty(..))

import RTS.Input
import RTS.Numeric(intToSize)
import RTS.ParserAPI


data IList a = ICons a {-# UNPACK #-} !Input (IList a)
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
newtype Parser a =
  Parser { runP :: [String] -> Input -> Maybe ParseError -> Res a }

data Res a = NoResAbort !ParseError
           | NoResFail  !ParseError
           | Res a {-# UNPACK #-} !Input !(Maybe ParseError)
           | MultiRes a {-# UNPACK #-} !Input (IList a) !(Maybe ParseError)


joinRes2 :: Res a -> Res a -> Res a
joinRes2 xs ys =
  case xs of
    NoResAbort _ -> xs

    NoResFail es1 ->
      case ys of
        NoResAbort {}      -> ys
        NoResFail es2      -> NoResFail (merge es1 es2)
        Res a i mb         -> Res a i (Just $! mergeMb mb es1)
        MultiRes a i is mb -> MultiRes a i is (Just $! mergeMb mb es1)

    Res a i mb ->
      case ys of
        NoResAbort {}       -> ys
        NoResFail es        -> Res a i (Just $! mergeMb mb es)
        Res b j mb1         -> MultiRes a i (ICons b j INil) (mergeMbMb mb mb1)
        MultiRes b j ps mb1 -> MultiRes a i (ICons b j ps) (mergeMbMb mb mb1)

    MultiRes a i ps mb ->
      case ys of
        NoResAbort _    -> ys
        NoResFail es    -> MultiRes a i ps (Just $! mergeMb mb es)
        Res b j mb1     -> MultiRes a i (ICons b j ps) (mergeMbMb mb mb1)
                                                                -- reorders
        MultiRes b j qs mb1 -> MultiRes a i
                                  (ICons b j (iappend ps qs))
                                  (mergeMbMb mb mb1)          -- reorders

merge :: ParseError -> ParseError -> ParseError
merge e1 e2 = e1 <> e2 -- if peOffset e1 <= peOffset e2 then e2 else e1

mergeMb :: Maybe ParseError -> ParseError -> ParseError
mergeMb mb e = case mb of
                 Nothing -> e
                 Just e1 -> merge e1 e

mergeMbMb :: Maybe ParseError -> Maybe ParseError -> Maybe ParseError
mergeMbMb mb1 mb2 =
  case mb1 of
    Nothing -> mb2
    Just e  -> Just $! mergeMb mb2 e


joinRes :: Res a -> (a -> Input -> Maybe ParseError -> Res b) -> Res b
joinRes xs0 k =
  case xs0 of
    NoResAbort e      -> NoResAbort e
    NoResFail  e      -> NoResFail e
    Res a i mb        -> k a i mb
    MultiRes a i more mb -> joinIList (\v j -> k v j mb) (k a i mb) more
{-# INLINE joinRes #-}

joinIList :: (a -> Input -> Res b) -> Res b -> IList a -> Res b
joinIList k done xs =
  case xs of
    INil         -> done
    ICons a i ys -> joinRes2 done (joinIList k (k a i) ys)

instance Functor Parser where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure v = Parser \_ bs err -> Res v bs err
  (<*>)  = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Parser where
  Parser p >>= f =
    Parser \cs bs err -> joinRes (p cs bs err) \a newI err1 ->
                         runP (f a) cs newI err1
  {-# INLINE (>>=) #-}

runParser :: Parser a -> Input -> Result a
runParser p i = case runP p [] i Nothing of
                  NoResAbort e        -> NoResults e
                  NoResFail  e        -> NoResults e
                  Res a _ _           -> Results (a :| [])
                  MultiRes a _ more _ -> Results (a :| itoList more)

instance BasicParser Parser where
  pByte rng =
    Parser \cs inp err ->
      case inputByte inp of
        Just (x,xs) -> Res x xs err
        Nothing     -> NoResFail
                           $ mergeMb err
                             PE { peInput = inp
                                , peGrammar =  [rng]
                                , peMsg =  msg
                                , peSource = FromSystem
                                , peStack = cs
                                , peMore = Nothing
                                }
          where msg = "unexpected end of input"

  pPeek = Parser \_ i err -> Res i i err
  {-# INLINE pPeek #-}

  pSetInput i = Parser \_ _ err -> Res () i err
  {-# INLINE pSetInput #-}

  pEnter x p = Parser \cs inp err -> runP p (x:cs) inp err
  {-# INLINE pEnter #-}

  pStack     = Parser \cs inp err -> Res cs inp err
  {-# INLINE pStack #-}

  p <|| q = Parser \cs inp err ->
             case runP p cs inp err of
               NoResAbort e       -> NoResAbort e
               NoResFail err1     -> runP q cs inp (Just err1)
               Res a i mb         -> Res a i (mergeMbMb mb err)
               MultiRes a i is mb -> MultiRes a i is (mergeMbMb mb err)
  {-# INLINE (<||) #-}

  p ||| q = Parser \cs inp err ->
              runP p cs inp err `joinRes2` runP q cs inp err
  {-# INLINE (|||) #-}

  pFail e = Parser \_ _ err -> NoResFail (mergeMb err e)
  {-# INLINE pFail #-}

  pEnd r =
    do inp <- pPeek
       case inputByte inp of
         Nothing -> pure ()
         Just (_,_) -> pErrorAt FromSystem [r] inp "unexpected left over input"
  {-# INLINE pEnd #-}

  pOffset = intToSize . inputOffset <$> pPeek
  {-# INLINE pOffset #-}

  pMatch1 erng (ClassVal p str) =
    do inp <- pPeek
       b   <- pByte erng
       unless (p b)
         $ pErrorAt FromSystem [erng] inp
         $ unwords ["byte", showByte b, "does not match", str]
       pure b
  {-# INLINE pMatch1 #-}


  pErrorMode em (Parser p) = Parser \cs inp err ->
                             case p cs inp err of
                               NoResAbort msg -> newErr msg
                               NoResFail  msg -> newErr msg
                               x              -> x
    where newErr = case em of
                     Abort -> NoResAbort
                     Fail  -> NoResFail
  {-# INLINE pErrorMode #-}
