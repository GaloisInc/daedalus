{-# LANGUAGE BlockArguments, RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module RTS.Parser (ParserG, runParser) where

import Control.Monad
import Data.List.NonEmpty(NonEmpty(..))

import RTS.Input
import RTS.ParseError
import RTS.ParserAPI


{- | The type of a parsers.

  * `s` is custom parser state used to track input used parser.
  * `e` is a context frame used to keep track of the call stack during parsing.
  * `a` is the type of semantic value produced by the parser.
-}

newtype ParserG s e a = Parser
  { runP ::
     [e]                     {- context stack -} ->
     s                       {- state -} ->
     Input                   {- the input we are parsing -} ->
     Maybe (ParseErrorG s e) {- last error if we got here by backtracking -} ->
     Res s e a
  }



--------------------------------------------------------------------------------
-- Parser Results
--------------------------------------------------------------------------------

-- | The result of parsing something.
data Res s e a =

    NoResAbort !(ParseErrorG s e)
    -- ^ Abort parsing, with this error

  | NoResFail  !(ParseErrorG s e)
    -- ^ Parse error, with the given error

  | Res a !s{-# UNPACK #-} !Input !(Maybe (ParseErrorG s e))
    -- ^ A successful result.
    -- If this carries a parse error,
    -- then we succeeded after another branch failed (biased choice).

  | MultiRes a !s {-# UNPACK #-} !Input (IList s a) !(Maybe (ParseErrorG s e))
    -- ^ Multiple successful results.
    -- If this carries a parse error,
    -- then we succeeded after another branch failed (biased choice).


{- | Additional results, in the case of multiple parses.
     Isomorphic to `[(a,s,Input)]`
-}
data IList s a = ICons a !s {-# UNPACK #-} !Input (IList s a)
               | INil


--------------------------------------------------------------------------------
-- Merging the results of multiple parsers.

-- | Join the results of two parsers (unbiased choice)
joinRes2 :: Res s e a -> Res s e a -> Res s e a
joinRes2 xs ys =
  case xs of
    NoResAbort _ -> xs

    NoResFail es1 ->
      case ys of
        NoResAbort {}         -> ys
        NoResFail es2         -> NoResFail (merge EQ es1 es2)
        Res a t i mb          -> Res a t i (Just $! mergeMb LT mb es1)
        MultiRes a t i is mb  -> MultiRes a t i is (Just $! mergeMb LT mb es1)

    Res a t1 i mb ->
      case ys of
        NoResAbort {}           -> ys
        NoResFail es            -> Res a t1 i (Just $! mergeMb LT mb es)
        Res b t2 j mb1          -> MultiRes a t1 i (ICons b t2 j INil)
                                                   (mergeMbMb EQ mb mb1)
        MultiRes b t2 j ps mb1  -> MultiRes a t1 i (ICons b t2 j ps)
                                                   (mergeMbMb EQ mb mb1)

    MultiRes a t1 i ps mb ->
      case ys of
        NoResAbort _    -> ys
        NoResFail es    -> MultiRes a t1 i ps (Just $! mergeMb LT mb es)
        Res b t2 j mb1  -> MultiRes a t1 i (ICons b t2 j ps)
                                           (mergeMbMb EQ mb mb1) -- reorders

        MultiRes b t2 j qs mb1 -> MultiRes a t1 i
                                  (ICons b t2 j (iappend ps qs))
                                  (mergeMbMb EQ mb mb1)          -- reorders


-- | Append extra results.
iappend :: IList s a -> IList s a -> IList s a
iappend xs ys =
  case xs of
    INil           -> ys
    ICons a t i zs -> ICons a t i (iappend zs ys)

-- | Convert extra result to a list, ignoring the inputs.
itoList :: IList s a -> [(a,s)]
itoList xs =
  case xs of
    INil           -> []
    ICons a t _ bs -> (a,t) : itoList bs

-- | Merge two parse errors.  The ordering specifies which of the errors
-- we should prefer (the "smaller" one)
merge ::
  Ordering -> ParseErrorG s e -> ParseErrorG s e -> ParseErrorG s e
merge dep e1 e2 =
  case dep of
    EQ -> e1 <> e2
    LT -> app e1 e2
    GT -> app e2 e1
  where
  app xs ys = case peMore xs of
                Nothing -> xs { peMore = Just ys }
                Just zs -> xs { peMore = Just (app zs ys) }

-- | Merge a potential error with an error
mergeMb ::
  Ordering -> Maybe (ParseErrorG s e) -> ParseErrorG s e -> ParseErrorG s e
mergeMb dep mb e =
  case mb of
    Nothing -> e
    Just e1 -> merge dep e1 e

-- | Merge two potential errors
mergeMbMb ::
  Ordering ->
  Maybe (ParseErrorG s e) -> Maybe (ParseErrorG s e) -> Maybe (ParseErrorG s e)
mergeMbMb dep mb1 mb2 =
  case mb1 of
    Nothing -> mb2
    Just e  -> Just $! mergeMb dep mb2 e

--------------------------------------------------------------------------------
-- Monad / Sequencing

-- | Given a parser result and a continuation, compute a new parser results.
joinRes ::
  Res s e a ->
  (a -> s -> Input -> Maybe (ParseErrorG s e) -> Res s e b) ->
  Res s e b
joinRes xs0 k =
  case xs0 of
    NoResAbort e      -> NoResAbort e
    NoResFail  e      -> NoResFail e
    Res a t i mb      -> k a t i mb
    MultiRes a t i more mb ->
      joinIList (\v t2 j -> k v t2 j mb) (k a t i mb) more

{-# INLINE joinRes #-}

-- | Add a list of alternative successes to a parser result.
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
  pure v = Parser \_env s i err -> Res v s i err
  (<*>)  = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad (ParserG s e) where
  Parser p >>= f =
    Parser \env s i err -> joinRes (p env s i err) \a newS newI newErr ->
                           runP (f a) env newS newI newErr
  {-# INLINE (>>=) #-}

runParser ::
  (HasSourcePaths e, IsITrace s) => ParserG s e a -> Input -> ResultG s e a
runParser p i = case runP p [] (emptyITrace i) i Nothing of
                  NoResAbort e          -> NoResults (normalizePaths e)
                  NoResFail  e          -> NoResults (normalizePaths e)
                  Res a t _ _           -> Results ((a,t) :| [])
                  MultiRes a t _ more _ -> Results ((a,t) :| itoList more)


--------------------------------------------------------------------------------
-- Parser operations
--------------------------------------------------------------------------------

instance IsITrace s => BasicParser (ParserG s e) where
  type Annot  (ParserG s e) = e
  type ITrace (ParserG s e) = s

  -- Get a byte from the input, if any.  Records byte access in the state.
  pByte rng =
    Parser \env s inp err ->
      case inputByte inp of
        Just (x,newInp) -> Res x (addITrace inp s) newInp err
        Nothing         -> NoResFail
                           $ mergeMb LT err
                             PE { peInput   = inp
                                , peGrammar = [rng]
                                , peMsg     = msg
                                , peSource  = FromSystem
                                , peStack   = env
                                , peMore    = Nothing
                                , peNumber  = -1
                                , peITrace  = s
                                }
          where msg = "unexpected end of input"

  -- Get the parser input.  Note that this allows decisions on the input
  -- that would not be automatically recorded in the trace.
  pPeek = Parser \_ s i err -> Res i s i err
  {-# INLINE pPeek #-}

  -- Set the parser input
  pSetInput i = Parser \_ s _ err -> Res () s i err
  {-# INLINE pSetInput #-}

  -- Enter a new context frame
  pEnter x p = Parser \env s inp err -> runP p (x:env) s inp err
  {-# INLINE pEnter #-}

  -- Get the current context
  pStack     = Parser \env s inp err -> Res env s inp err
  {-# INLINE pStack #-}

  -- Get the current trace state
  pITrace = Parser \_ s inp err -> Res s s inp err
  {-# INLINE pITrace #-}

  -- Set the current trace state
  pSetITrace s = Parser \_ _ inp err -> Res () s inp err
  {-# INLINE pSetITrace #-}

  -- Biased choice
  p <|| q = Parser \env s inp err ->
             case runP p env s inp err of
               NoResAbort newErr             -> NoResAbort newErr
               NoResFail newErr              -> runP q env s inp (Just newErr)
               Res a newS newInp mb          -> Res a newS newInp mb
               MultiRes a newS newI newIs mb -> MultiRes a newS newI newIs mb
  {-# INLINE (<||) #-}

  -- Unbiased choice
  p ||| q = Parser \env s inp err ->
              runP p env s inp err `joinRes2` runP q env s inp err
  {-# INLINE (|||) #-}

  -- Failure
  pFail e = Parser \_ _ _ err -> NoResFail (mergeMb LT err e)
  {-# INLINE pFail #-}

  -- Switch the error mode
  pErrorMode em (Parser p) = Parser \env s inp err ->
                             case p env s inp err of
                               NoResAbort msg -> newErr msg
                               NoResFail  msg -> newErr msg
                               x              -> x
    where newErr = case em of
                     Abort -> NoResAbort
                     Fail  -> NoResFail
  {-# INLINE pErrorMode #-}
