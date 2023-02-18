{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Talos.Strategy.DFST (DFST, runDFST, onBacktrack) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Talos.Strategy.Monad
import Control.Monad.Catch

-- =============================================================================
-- DFS monad transformer
--
-- This is similar to the list monad, but it wraps another monad and
-- hence has to be a bit careful about what to do when --- if we use
-- ListT, we get effects from all the alternatives, which could be
-- expensive.  This is similar to ContT, but we also keep around a
-- failure continuation.
--

-- The dfsCont takes the return value, and also an updated failure
-- continuation, as we may want to backtrack into a completed
-- computation.
data DFSTContext r m a =
  DFSTContext { dfsCont :: a -> m r -> m r
              , dfsFail :: m r
              }
  
newtype DFST r m a = DFST { getDFST :: DFSTContext r m a -> m r }

runDFST :: DFST r m a -> (a -> m r) -> m r -> m r
runDFST m cont fl = getDFST m (DFSTContext (\v _ -> cont v) fl)

onBacktrack :: DFST r m a -> DFST r m a -> DFST r m a
onBacktrack m handler =
  DFST $ \ctxt -> getDFST m (ctxt { dfsFail = getDFST handler ctxt })
  
-- ----------------------------------------------------------------------------------------
-- Instances

instance Functor (DFST r m) where
  fmap f (DFST m) = DFST $ \ctxt -> m (ctxt { dfsCont = dfsCont ctxt . f })

instance Applicative (DFST r m) where
  pure v              = DFST $ \ctxt -> dfsCont ctxt v (dfsFail ctxt)
  (<*>)               = ap
  -- DFST fm <*> DFST vm = DFST $ \ctxt ->
  --   let vCont f = \v -> dfsCont ctxt (f v)
  --       fCont   = \f -> vm (ctxt { dfsCont = vCont f })
  --   in fm (ctxt { dfsCont = fCont })

instance Monad (DFST r m) where
  DFST m >>= f = DFST $ \ctxt ->
    let cont v fl = getDFST (f v) ( ctxt { dfsFail = fl })
    in m (ctxt { dfsCont = cont })

-- | We want
--
-- (a `mplus` b) >>= f == (a >>= f) `mplus` (b >>= f)
--
-- i.e., we give f the result of a, but if that fails, we run f on b's
-- result.

instance Alternative (DFST r m) where
  (DFST m1) <|> (DFST m2) = DFST $ \ctxt ->
    let ctxt1 = ctxt { dfsFail = m2 ctxt } in m1 ctxt1
  empty = DFST dfsFail 

instance MonadPlus (DFST r m) where -- default body (Alternative)
                     
instance MonadTrans (DFST r) where
  lift m = DFST $ \ctxt -> m >>= \v -> dfsCont ctxt v (dfsFail ctxt)

instance MonadIO m => MonadIO (DFST r m) where
  liftIO = lift . liftIO
  
instance LiftStrategyM m => LiftStrategyM (DFST r m) where
  liftStrategy m = lift (liftStrategy m)
    

instance MonadThrow m => MonadThrow (DFST r m) where
    throwM e = lift $ throwM e

instance MonadCatch m => MonadCatch (DFST r m) where
  catch (DFST f) hndl = DFST $ \ctxt ->
    catch (f ctxt) (\e -> getDFST (hndl e) ctxt )

liftDFST ::
  (forall a. m a -> m a) ->
  DFST r m b -> 
  DFST r m b
liftDFST f g = DFST $ \ctxt -> f (getDFST g ctxt)

-- FIXME: the intent here is to define a function that executes the inner function
-- but skips the first backtracking result
skip :: DFST r m a -> DFST r m a
skip f = DFST $ \ctxt ->
  let ctxt' = ctxt { dfsCont = \_ _ -> dfsFail ctxt}
  in getDFST f ctxt'

instance MonadMask m => MonadMask (DFST r m) where
  mask f = DFST $ \ctxt -> mask (\g -> getDFST (f (liftDFST g)) ctxt)
  uninterruptibleMask f = DFST $ \ctxt -> uninterruptibleMask (\g -> getDFST (f (liftDFST g)) ctxt)
  -- FIXME: this isn't great because it doesn't defer to the underlying mask instance, but it's
  -- very unclear how to do that here
  generalBracket acquire release act = 
    let go act_ = do
          a <- acquire
          result <- onBacktrack (catch (act_ a >>= \c -> return $ ExitCaseSuccess c) (\e -> return $ ExitCaseException e)) (return ExitCaseAbort)
          release_result <- release a result
          case result of
            ExitCaseSuccess c -> return (c, release_result)
            ExitCaseException e -> throwM e
            ExitCaseAbort -> DFST dfsFail
        go_next act_ = go act_ -- <|> go (\a -> skip (act_ a))
    in go_next act

        
        
        

          

