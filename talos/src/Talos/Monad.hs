{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Services monad for Talos in general, provides mainly debugging etc.

module Talos.Monad where

import qualified Colog.Core                   as Log
import           Control.Monad.Reader         (ReaderT)
import           Control.Monad.RWS            (RWST)
import qualified Control.Monad.State          as St
import           Control.Monad.State.Strict   (MonadIO, MonadTrans (lift),
                                               StateT (..), evalStateT, gets,
                                               state)
import           Control.Monad.Trans.Free     (FreeT)
import           Control.Monad.Trans.Maybe    (MaybeT)
import           Control.Monad.Writer         (WriterT)
import           Data.Foldable                (find)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text as Text

import qualified Streaming                    as S

import           Daedalus.Core
import qualified Daedalus.Core.Semantics.Decl as I
import qualified Daedalus.Core.Semantics.Env  as I
import           Daedalus.GUID                (GUID, HasGUID, guidState)
import           Daedalus.Panic               (panic)
import           Daedalus.PP                  (showPP)
import           Daedalus.Rec                 (forgetRecs)
import           Talos.Solver.SolverT        (SolverT)
import Data.String (IsString(..))
import Control.Monad.Except (ExceptT)

data TalosMState  = TalosMState
  { tmModule    :: !Module
    -- Derived from the module
  , tmFunDefs   :: !(Map FName (Fun Expr))
  , tmBFunDefs  :: !(Map FName (Fun ByteSet))
  , tmIEnv      :: !I.Env
  -- Logging and Statistics
  , tmStatAction :: !(Log.LogAction TalosM (LogKey, Statistic))
  , tmLogAction  :: !(Log.LogAction TalosM (LogKey, (LogLevel, String)))
    -- GUIDs
  , tmNextGUID  :: !GUID
  }

newtype TalosM a =
  TalosM { getTalosM :: StateT TalosMState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTalosStream :: Functor f => Module -> GUID ->
                  Log.LogAction TalosM (LogKey, Statistic) ->
                  Log.LogAction TalosM (LogKey, (LogLevel, String)) ->
                  S.Stream f TalosM r ->
                  S.Stream f IO r
runTalosStream md nguid statsact logact strm =
  evalStateT (S.distribute (S.hoist getTalosM strm)) (emptyTalosMState md nguid statsact logact)

runTalosM :: Module -> GUID ->
             Log.LogAction TalosM (LogKey, Statistic) ->
             Log.LogAction TalosM (LogKey, (LogLevel, String)) ->
             TalosM a -> IO a
runTalosM md nguid statsact logact m = evalStateT (getTalosM m) (emptyTalosMState md nguid statsact logact)

emptyTalosMState :: Module -> GUID ->
                    Log.LogAction TalosM (LogKey, Statistic) ->
                    Log.LogAction TalosM (LogKey, (LogLevel, String)) ->
                    TalosMState
emptyTalosMState md nguid statsact logact = TalosMState
  { tmModule    = md
  -- Derived from the module
  , tmFunDefs   = funDefs
  , tmBFunDefs  = bfunDefs
  , tmIEnv      = env0
  , tmStatAction = statsact
  , tmLogAction  = logact
  , tmNextGUID  = nguid
  }
  where
    env0 = I.defTypes tyDefs (I.evalModule md I.emptyEnv)
    tyDefs  = Map.fromList [ (tName td, td) | td <- forgetRecs (mTypes md) ]
    funDefs = Map.fromList [ (fName f, f) | f <- mFFuns md ]
    bfunDefs = Map.fromList [ (fName f, f) | f <- mBFuns md ]
    

getGFun :: LiftTalosM m => FName -> m (Fun Grammar)
getGFun f = getFun <$> liftTalosM (TalosM (gets tmModule))
  where
    getFun md = case find ((==) f . fName) (mGFuns md) of -- FIXME: us a map or something
      Nothing -> panic "Missing function" [showPP f]
      Just v  -> v

getModule :: LiftTalosM m => m Module
getModule = liftTalosM (TalosM (gets tmModule))

getTypeDefs :: LiftTalosM m => m (Map TName TDecl)
getTypeDefs = liftTalosM (TalosM (gets (I.tEnv . tmIEnv)))

getFunDefs :: LiftTalosM m => m (Map FName (Fun Expr))
getFunDefs = liftTalosM (TalosM (gets tmFunDefs))

getBFunDefs :: LiftTalosM m => m (Map FName (Fun ByteSet))
getBFunDefs = liftTalosM (TalosM (gets tmBFunDefs))

getIEnv :: LiftTalosM m => m I.Env
getIEnv = liftTalosM (TalosM (gets tmIEnv))

-- For debugging
getRawGUID :: LiftTalosM m => m GUID
getRawGUID = liftTalosM (TalosM (gets tmNextGUID))

-- -----------------------------------------------------------------------------
-- Logging and Statistics

newtype LogKey = LogKey { getLogKey :: Text }
type Statistic = Text

logKeyEnabled :: LogKey -> Text -> Bool
logKeyEnabled k t = t `Text.isPrefixOf` (getLogKey k)

-- We use this over Colog.Core.Severity as we don't need all the
-- levels from that type.
data LogLevel = Warning | Info | Debug
  deriving Enum

instance Show LogLevel where
  show lvl = case lvl of
               Debug -> "DEBUG"
               Info  -> "INFO"
               Warning -> "WARNING"

statistic :: LiftTalosM m => LogKey -> Statistic -> m ()
statistic key val = liftTalosM $ do
  action <- TalosM $ gets tmStatAction
  action Log.<& (key, val)

statS :: (LiftTalosM m, Show a) => LogKey -> a -> m ()
statS key val = liftTalosM $ do
  action <- TalosM $ gets tmStatAction
  action Log.<& (key, Text.pack (show val))

logMessage :: LiftTalosM m => LogLevel -> LogKey -> String -> m ()
logMessage lvl key msg = liftTalosM $ do
  action <- TalosM $ gets tmLogAction
  action Log.<& (key, (lvl, msg))

debug, info, warning :: LiftTalosM m => LogKey -> String -> m ()
debug   = logMessage Info 
info    = logMessage Debug 
warning = logMessage Warning 

-- -----------------------------------------------------------------------------
-- Lifting

class Monad m => LiftTalosM m where
  liftTalosM :: TalosM a -> m a

instance LiftTalosM TalosM where
  liftTalosM = id

instance LiftTalosM m => LiftTalosM (StateT s m) where
  liftTalosM = lift . liftTalosM
instance LiftTalosM m => LiftTalosM (St.StateT s m) where
  liftTalosM = lift . liftTalosM
 
instance LiftTalosM m => LiftTalosM (ReaderT s m) where
  liftTalosM = lift . liftTalosM
instance (Monoid w, LiftTalosM m) => LiftTalosM (WriterT w m) where
  liftTalosM = lift . liftTalosM
instance LiftTalosM m => LiftTalosM (MaybeT m) where
  liftTalosM = lift . liftTalosM
instance LiftTalosM m => LiftTalosM (ExceptT e m) where
  liftTalosM = lift . liftTalosM  
instance LiftTalosM m => LiftTalosM (SolverT m) where
  liftTalosM = lift . liftTalosM
instance (Functor f, LiftTalosM m) => LiftTalosM (FreeT f m) where
  liftTalosM = lift . liftTalosM
instance (Monoid w, LiftTalosM m) => LiftTalosM (RWST r w s m) where
  liftTalosM = lift . liftTalosM

-- -----------------------------------------------------------------------------
-- Instances

instance HasGUID TalosM where
  guidState f = TalosM (state go)
    where
      go s = let (r, guid') = f (tmNextGUID s)
             in (r, s { tmNextGUID = guid' })

instance IsString LogKey where
  fromString = LogKey . fromString

-- Not a Monoid as the empty key makes no sense
instance Semigroup LogKey where
  (LogKey k1) <> (LogKey k2) = LogKey (k1 <> "." <> k2)

  
