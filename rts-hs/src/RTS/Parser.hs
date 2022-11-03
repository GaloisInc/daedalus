{-# LANGUAGE BlockArguments, RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module RTS.Parser (ParserG, runParser) where

import Control.Monad
import Data.List.NonEmpty(NonEmpty(..))

import RTS.Input
import RTS.Numeric(intToSize)
import RTS.ParseError
import RTS.ParserAPI

import Debug.Trace


data IList s a = ICons a !s {-# UNPACK #-} !Input (IList s a)
               | INil

iappend :: IList s a -> IList s a -> IList s a
iappend xs ys =
  case xs of
    INil           -> ys
    ICons a t i zs -> ICons a t i (iappend zs ys)

itoList :: IList s a -> [(a,s)]
itoList xs =
  case xs of
    INil           -> []
    ICons a t _ bs -> (a,t) : itoList bs


newtype ParserG s e a =
  Parser { runP ::
           [e]              {- context stack -} ->
           s                {- (debug) bytes of interest to get to here -} ->
           Input            {- the input we are parsing -} ->
           Maybe (ParseErrorG s e)
                  {- last error if we got here by backtracking -} ->
           Res s e a
         }

data Res s e a =
    NoResAbort !(ParseErrorG s e)
  | NoResFail  !(ParseErrorG s e)
  | Res a !s{-# UNPACK #-} !Input !(Maybe (ParseErrorG s e))
  | MultiRes a !s {-# UNPACK #-} !Input (IList s a) !(Maybe (ParseErrorG s e))


joinRes2 :: Res s e a -> Res s e a -> Res s e a
joinRes2 xs ys =
  case xs of
    NoResAbort _ -> xs

    NoResFail es1 ->
      case ys of
        NoResAbort {}      -> ys
        NoResFail es2      -> NoResFail (merge EQ es1 es2)
        Res a t i mb       -> Res a t i (Just $! mergeMb LT mb es1)
        MultiRes a t i is mb -> MultiRes a t i is (Just $! mergeMb LT mb es1)

    Res a t1 i mb ->
      case ys of
        NoResAbort {}       -> ys
        NoResFail es        -> Res a t1 i (Just $! mergeMb LT mb es)
        Res b t2 j mb1      -> MultiRes a t1 i (ICons b t2 j INil)
                                               (mergeMbMb EQ mb mb1)
        MultiRes b t2 j ps mb1 -> MultiRes a t1 i (ICons b t2 j ps)
                                                  (mergeMbMb EQ mb mb1)

    MultiRes a t1 i ps mb ->
      case ys of
        NoResAbort _    -> ys
        NoResFail es    -> MultiRes a t1 i ps (Just $! mergeMb LT mb es)
        Res b t2 j mb1  -> MultiRes a t1 i (ICons b t2 j ps)
                                           (mergeMbMb EQ mb mb1)
                                                                -- reorders
        MultiRes b t2 j qs mb1 -> MultiRes a t1 i
                                  (ICons b t2 j (iappend ps qs))
                                  (mergeMbMb EQ mb mb1)          -- reorders

merge ::
  Ordering -> ParseErrorG s e -> ParseErrorG s e -> ParseErrorG s e
merge dep e1 e2 =
  case dep of
    EQ -> trace ("EQ " ++ show (peMsg e1, peMsg e2)) (e1 <> e2)
    LT -> app e1 e2
    GT -> app e2 e1
  where
  app xs ys = case peMore xs of
                Nothing -> xs { peMore = Just ys }
                Just zs -> xs { peMore = Just (app zs ys) }

mergeMb ::
  Ordering -> Maybe (ParseErrorG s e) -> ParseErrorG s e -> ParseErrorG s e
mergeMb dep mb e =
  case mb of
    Nothing -> e
    Just e1 -> merge dep e1 e

mergeMbMb ::
  Ordering ->
  Maybe (ParseErrorG s e) -> Maybe (ParseErrorG s e) -> Maybe (ParseErrorG s e)
mergeMbMb dep mb1 mb2 =
  case mb1 of
    Nothing -> mb2
    Just e  -> Just $! mergeMb dep mb2 e

joinRes ::
  Res s e a ->
  (a -> s -> Input -> Maybe (ParseErrorG s e) -> Res s e b) ->
  Res s e b
joinRes xs0 k =
  case xs0 of
    NoResAbort e      -> NoResAbort e
    NoResFail  e      -> NoResFail e
    Res a t i mb      -> k a t i mb
    MultiRes a t i more mb -> joinIList (\v t2 j -> k v t2 j mb)
                                                           (k a t i mb) more
{-# INLINE joinRes #-}

joinIList ::
  (a -> s -> Input -> Res s e b) -> Res s e b -> IList s a -> Res s e b
joinIList k done xs =
  case xs of
    INil           -> done
    ICons a t i ys -> joinRes2 done (joinIList k (k a t i) ys)

instance Functor (ParserG s e) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (ParserG s e) where
  pure v = Parser \_ t bs err -> Res v t bs err
  (<*>)  = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad (ParserG s e) where
  Parser p >>= f =
    Parser \cs t bs err -> joinRes (p cs t bs err) \a t' newI err1 ->
                           runP (f a) cs t' newI err1
  {-# INLINE (>>=) #-}

runParser ::
  (HasSourcePaths e, IsITrace s) => ParserG s e a -> Input -> ResultG s e a
runParser p i = case runP p [] (emptyITrace i) i Nothing of
                  NoResAbort e          -> NoResults (normalizePaths e)
                  NoResFail  e          -> NoResults (normalizePaths e)
                  Res a t _ _           -> Results ((a,t) :| [])
                  MultiRes a t _ more _ -> Results ((a,t) :| itoList more)

instance IsITrace s => BasicParser (ParserG s e) where
  type Annot  (ParserG s e) = e
  type ITrace (ParserG s e) = s
  pByte rng =
    Parser \cs t inp err ->
      case inputByte inp of
        Just (x,xs) -> Res x (addITrace inp t) xs err
        Nothing     -> NoResFail
                           $ mergeMb LT err
                             PE { peInput = inp
                                , peGrammar =  [rng]
                                , peMsg =  msg
                                , peSource = FromSystem
                                , peStack = cs
                                , peMore = Nothing
                                , peNumber = -1
                                , peITrace = t
                                }
          where msg = "unexpected end of input"

  pPeek = Parser \_ t i err -> Res i t i err
  {-# INLINE pPeek #-}

  pSetInput i = Parser \_ t _ err -> Res () t i err
  {-# INLINE pSetInput #-}

  pEnter x p = Parser \cs t inp err -> runP p (x:cs) t inp err
  {-# INLINE pEnter #-}

  pStack     = Parser \cs t inp err -> Res cs t inp err
  {-# INLINE pStack #-}

  pITrace = Parser \_ t inp err -> Res t t inp err
  {-# INLINE pITrace #-}

  pSetITrace i = Parser \_ _ inp err -> Res () i inp err
  {-# INLINE pSetITrace #-}

  p <|| q = Parser \cs t inp err ->
             case runP p cs t inp err of
               NoResAbort e       -> NoResAbort e
               NoResFail err1     -> runP q cs t inp (Just err1)
               Res a t1 i mb      -> Res a t1 i mb
               MultiRes a i t1 is mb -> MultiRes a i t1 is mb
  {-# INLINE (<||) #-}

  p ||| q = Parser \cs t1 inp err ->
              runP p cs t1 inp err `joinRes2` runP q cs t1 inp err
  {-# INLINE (|||) #-}

  pFail e = Parser \_ _ _ err -> NoResFail (mergeMb LT err e)
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


  pErrorMode em (Parser p) = Parser \cs t inp err ->
                             case p cs t inp err of
                               NoResAbort msg -> newErr msg
                               NoResFail  msg -> newErr msg
                               x              -> x
    where newErr = case em of
                     Abort -> NoResAbort
                     Fail  -> NoResFail
  {-# INLINE pErrorMode #-}
