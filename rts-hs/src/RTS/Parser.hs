{-# LANGUAGE BlockArguments, RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module RTS.Parser (ParserG, runParser) where

import Control.Monad
import Data.List.NonEmpty(NonEmpty(..))

import RTS.Input
import RTS.ParseError
import RTS.ParserAPI
import RTS.InputTrace


{- | The type of a parsers.

  * `s` is custom parser state used to track input used by the parser.
  * `e` is a context frame used to keep track of the call stack during parsing.
  * `a` is the type of semantic value produced by the parser.
-}

newtype ParserG e a = Parser
  { runP ::
     ErrorStyle              {- single or multiple errors -} ->
     [e]                     {- context stack -} ->
     InputTrace              {- state -} ->
     Input                   {- the input we are parsing -} ->
     Maybe (ParseErrorG e)   {- last error if we got here by backtracking -} ->
     Res e a
  }




--------------------------------------------------------------------------------
-- Parser Results
--------------------------------------------------------------------------------

-- | The result of parsing something.
data Res e a =

    NoResAbort !(ParseErrorG e)
    -- ^ Abort parsing, with this error

  | NoResFail  !(ParseErrorG e)
    -- ^ Parse error, with the given error

  | Res a !InputTrace{-# UNPACK #-} !Input !(Maybe (ParseErrorG e))
    -- ^ A successful result.
    -- If this carries a parse error,
    -- then we succeeded after another branch failed (biased choice).

  | MultiRes a !InputTrace {-# UNPACK #-} !Input (IList a)
                                                      !(Maybe (ParseErrorG e))
    -- ^ Multiple successful results.
    -- If this carries a parse error,
    -- then we succeeded after another branch failed (biased choice).


{- | Additional results, in the case of multiple parses.
     Isomorphic to `[(a,s,Input)]`
-}
data IList a = ICons a !InputTrace {-# UNPACK #-} !Input (IList a)
             | INil


--------------------------------------------------------------------------------
-- Merging the results of multiple parsers.

-- | Join the results of two parsers (unbiased choice)
joinRes2 :: ErrorStyle -> Res e a -> Res e a -> Res e a
joinRes2 cfg xs ys =
  case xs of
    NoResAbort _ -> xs

    NoResFail es1 ->
      case ys of
        NoResAbort {}         -> ys
        NoResFail es2         -> NoResFail (merge cfg EQ es1 es2)
        Res a t i mb          -> Res a t i (Just $! mergeMb cfg LT mb es1)
        MultiRes a t i is mb  ->
          MultiRes a t i is (Just $! mergeMb cfg LT mb es1)

    Res a t1 i mb ->
      case ys of
        NoResAbort {}           -> ys
        NoResFail es            -> Res a t1 i (Just $! mergeMb cfg LT mb es)
        Res b t2 j mb1          -> MultiRes a t1 i (ICons b t2 j INil)
                                                   (mergeMbMb cfg EQ mb mb1)
        MultiRes b t2 j ps mb1 ->
          MultiRes a t1 i (ICons b t2 j ps) (mergeMbMb cfg EQ mb mb1)

    MultiRes a t1 i ps mb ->
      case ys of
        NoResAbort _    -> ys
        NoResFail es    -> MultiRes a t1 i ps (Just $! mergeMb cfg LT mb es)
        Res b t2 j mb1  -> MultiRes a t1 i (ICons b t2 j ps)
                                           (mergeMbMb cfg EQ mb mb1) -- reorders

        MultiRes b t2 j qs mb1 -> MultiRes a t1 i
                                  (ICons b t2 j (iappend ps qs))
                                  (mergeMbMb cfg EQ mb mb1)          -- reorders


-- | Append extra results.
iappend :: IList a -> IList a -> IList a
iappend xs ys =
  case xs of
    INil           -> ys
    ICons a t i zs -> ICons a t i (iappend zs ys)

-- | Convert extra result to a list, ignoring the inputs.
itoList :: IList a -> [(a,InputTrace)]
itoList xs =
  case xs of
    INil           -> []
    ICons a t _ bs -> (a,t) : itoList bs

-- | Merge two parse errors.  The ordering specifies which of the errors
-- we should prefer (the "smaller" one)
merge ::
  ErrorStyle -> Ordering ->
  ParseErrorG e -> ParseErrorG e -> ParseErrorG e
merge cfg dep e1 e2 =
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
  ErrorStyle ->
  Ordering -> Maybe (ParseErrorG e) -> ParseErrorG e -> ParseErrorG e
mergeMb cfg dep mb e =
  case mb of
    Nothing -> e
    Just e1 -> merge cfg dep e1 e

-- | Merge two potential errors
mergeMbMb ::
  ErrorStyle ->
  Ordering ->
  Maybe (ParseErrorG e) -> Maybe (ParseErrorG e) -> Maybe (ParseErrorG e)
mergeMbMb cfg dep mb1 mb2 =
  case mb1 of
    Nothing -> mb2
    Just e  -> Just $! mergeMb cfg dep mb2 e

--------------------------------------------------------------------------------
-- Monad / Sequencing

-- | Given a parser result and a continuation, compute a new parser results.
joinRes ::
  ErrorStyle ->
  Res e a ->
  (a -> InputTrace -> Input -> Maybe (ParseErrorG e) -> Res e b) ->
  Res e b
joinRes cfg xs0 k =
  case xs0 of
    NoResAbort e      -> NoResAbort e
    NoResFail  e      -> NoResFail e
    Res a t i mb      -> k a t i mb
    MultiRes a t i more mb ->
      joinIList cfg (\v t2 j -> k v t2 j mb) (k a t i mb) more

{-# INLINE joinRes #-}

-- | Add a list of alternative successes to a parser result.
joinIList ::
  ErrorStyle ->
  (a -> InputTrace -> Input -> Res e b) -> Res e b -> IList a -> Res e b
joinIList cfg k done xs =
  case xs of
    INil           -> done
    ICons a t i ys -> joinRes2 cfg done (joinIList cfg k (k a t i) ys)

instance Functor (ParserG e) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (ParserG e) where
  pure v = Parser \_ _env s i err -> Res v s i err
  (<*>)  = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad (ParserG e) where
  Parser p >>= f =
    Parser \cfg env s i err ->
      joinRes cfg (p cfg env s i err) \a newS newI newErr ->
      runP (f a) cfg env newS newI newErr
  {-# INLINE (>>=) #-}

runParser ::
  (HasSourcePaths e) =>
  ParserG e a -> ErrorStyle -> Input -> ResultG e a
runParser p cfg i =
  case runP p cfg [] emptyInputTrace i Nothing of
    NoResAbort e          -> NoResults (normalizePaths e)
    NoResFail  e          -> NoResults (normalizePaths e)
    Res a t _ _           -> Results ((a,t) :| [])
    MultiRes a t _ more _ -> Results ((a,t) :| itoList more)


--------------------------------------------------------------------------------
-- Parser operations
--------------------------------------------------------------------------------

instance BasicParser (ParserG e) where
  type Annot  (ParserG e) = e

  -- Get a byte from the input, if any.  Records byte access in the state.
  pByte rng =
    Parser \cfg env s inp err ->
      case inputByte inp of
        Just (x,newInp) -> Res x (addInputTrace inp s) newInp err
        Nothing         -> NoResFail
                           $ mergeMb cfg LT err
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
  pPeek = Parser \_ _ s i err -> Res i s i err
  {-# INLINE pPeek #-}

  -- Set the parser input
  pSetInput i = Parser \_ _ s _ err -> Res () s i err
  {-# INLINE pSetInput #-}

  -- Enter a new context frame
  pEnter x p = Parser \cfg env s inp err -> runP p cfg (x:env) s inp err
  {-# INLINE pEnter #-}

  -- Get the current context
  pStack     = Parser \_ env s inp err -> Res env s inp err
  {-# INLINE pStack #-}

  -- Get the current trace state
  pITrace = Parser \_ _ s inp err -> Res s s inp err
  {-# INLINE pITrace #-}

  -- Set the current trace state
  pSetITrace s = Parser \_ _ _ inp err -> Res () s inp err
  {-# INLINE pSetITrace #-}

  -- Biased choice
  p <|| q = Parser \cfg env s inp err ->
             case runP p cfg env s inp err of
               NoResAbort newErr  -> NoResAbort newErr
               NoResFail newErr   -> runP q cfg env s inp (Just newErr)
               Res a newS newInp mb          -> Res a newS newInp mb
               MultiRes a newS newI newIs mb -> MultiRes a newS newI newIs mb
  {-# INLINE (<||) #-}

  -- Unbiased choice
  p ||| q = Parser \cfg env s inp err ->
              joinRes2
                 cfg
                 (runP p cfg env s inp err)
                 (runP q cfg env s inp err)
  {-# INLINE (|||) #-}

  -- Failure
  pFail e = Parser \cfg _ _ _ err -> NoResFail (mergeMb cfg LT err e)
  {-# INLINE pFail #-}

  -- Switch the error mode
  pErrorMode em (Parser p) = Parser \cfg env s inp err ->
                             case p cfg env s inp err of
                               NoResAbort msg -> newErr msg
                               NoResFail  msg -> newErr msg
                               x              -> x
    where newErr = case em of
                     Abort -> NoResAbort
                     Fail  -> NoResFail
  {-# INLINE pErrorMode #-}
