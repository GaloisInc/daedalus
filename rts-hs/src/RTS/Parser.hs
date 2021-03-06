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
newtype Parser a = Parser { runP :: [String] -> Input -> Res a }

data Res a = NoResAbort ParseError
           | NoResFail  ParseError
           | Res a {-# UNPACK #-} !Input
           | MultiRes a {-# UNPACK #-} !Input (IList a)


joinRes2 :: Res a -> Res a -> Res a
joinRes2 xs ys =
  case xs of
    NoResAbort _ -> xs

    NoResFail es1 ->
      case ys of
        NoResFail es2  -> NoResFail (merge es1 es2)
        _              -> ys

    Res a i ->
      case ys of
        NoResAbort _    -> ys
        NoResFail {}    -> xs
        Res b j         -> MultiRes a i $ ICons b j INil
        MultiRes b j ps -> MultiRes a i $ ICons b j ps

    MultiRes a i ps ->
      case ys of
        NoResAbort _    -> ys
        NoResFail {}    -> xs
        Res b j         -> MultiRes a i $ ICons b j ps -- reorders
        MultiRes b j qs -> MultiRes a i $ ICons b j $ iappend ps qs
                                                              -- reorders
  where
  merge e1 e2 = if peOffset e1 <= peOffset e2 then e2 else e1


joinRes :: Res a -> (a -> Input -> Res b) -> Res b
joinRes xs0 k =
  case xs0 of
    NoResAbort e      -> NoResAbort e
    NoResFail  e      -> NoResFail e
    Res a i           -> k a i
    MultiRes a i more -> joinIList k (k a i) more
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
  pure v = Parser \_ bs -> Res v bs
  (<*>)  = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Parser where
  Parser p >>= f =
    Parser \cs bs -> joinRes (p cs bs) \a newI -> runP (f a) cs newI
  {-# INLINE (>>=) #-}

runParser :: Parser a -> Input -> Result a
runParser p i = case runP p [] i of
                  NoResAbort e      -> NoResults e
                  NoResFail  e      -> NoResults e
                  Res a _           -> Results (a :| [])
                  MultiRes a _ more -> Results (a :| itoList more)

instance BasicParser Parser where
  pByte rng =
    Parser \cs inp ->
      case inputByte inp of
        Just (x,xs) -> Res x xs
        Nothing     -> NoResFail
                             PE { peInput = inp
                                , peGrammar =  [rng]
                                , peMsg =  msg
                                , peSource = FromSystem
                                , peStack = cs
                                , peMore = Nothing
                                }
          where msg = "unexpected end of input"

  pPeek = Parser \_ i -> Res i i
  {-# INLINE pPeek #-}

  pSetInput i = Parser \_ _ -> Res () i
  {-# INLINE pSetInput #-}

  pEnter x p = Parser \cs inp -> runP p (x:cs) inp
  {-# INLINE pEnter #-}

  pStack     = Parser \cs inp -> Res cs inp
  {-# INLINE pStack #-}

  p <|| q = Parser \cs inp ->
             case runP p cs inp of
               NoResAbort e -> NoResAbort e
               NoResFail _  -> runP q cs inp
                -- XXX: this prefers the right error...
               res          -> res
  {-# INLINE (<||) #-}

  p ||| q = Parser \cs inp ->
              runP p cs inp `joinRes2` runP q cs inp
  {-# INLINE (|||) #-}

  pFail e = Parser \_ _ -> NoResFail e
  {-# INLINE pFail #-}

  pEnd r =
    do inp <- pPeek
       case inputByte inp of
         Nothing -> pure ()
         Just (_,i1) -> pErrorAt FromSystem [r] i1 "unexpected left over input"
  {-# INLINE pEnd #-}

  pOffset = intToSize . inputOffset <$> pPeek
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


  pErrorMode em (Parser p) = Parser \cs inp ->
                             case p cs inp of
                               NoResAbort msg -> newErr msg
                               NoResFail  msg -> newErr msg
                               x              -> x
    where newErr = case em of
                     Abort -> NoResAbort
                     Fail  -> NoResFail
  {-# INLINE pErrorMode #-}
