{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Services monad for Talos in general, provides mainly debugging etc.

module Talos.Monad where

import           Control.Monad.RWS         (RWST)
import           Control.Monad.Reader      (ReaderT)
import           Control.Monad.State       (MonadIO, MonadTrans (lift),
                                            StateT (..), gets, state, evalStateT)
import           Control.Monad.Trans.Free  (FreeT)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Writer      (WriterT)
import           Data.Foldable             (find)
import           Data.Map                  (Map)

import           Daedalus.Core
import qualified Daedalus.Core.Semantics.Decl as I
import qualified Daedalus.Core.Semantics.Env  as I
import           Daedalus.GUID                (GUID, HasGUID, guidState)
import           Daedalus.PP                  (showPP)
import           Daedalus.Panic               (panic)
import           Daedalus.Rec                 (forgetRecs)
import qualified Data.Map                     as Map
import           Talos.SymExec.SolverT        (SolverT)
import System.IO (Handle, hPutStrLn, hFlush)
import Control.Monad.IO.Class (liftIO)
import qualified Streaming as S

data TalosMState  = TalosMState
  { tmModule    :: !Module
    -- Derived from the module
  , tmFunDefs   :: !(Map FName (Fun Expr))
  , tmBFunDefs  :: !(Map FName (Fun ByteSet))
  , tmIEnv      :: !I.Env
  -- Statistics
  , tmStatHandle :: !(Maybe Handle)
    -- GUIDs
  , tmNextGUID  :: !GUID
  }

newtype TalosM a =
  TalosM { getTalosM :: StateT TalosMState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTalosStream :: Functor f => Module -> GUID -> Maybe Handle -> S.Stream f TalosM r ->
                  S.Stream f IO r
runTalosStream md nguid statshdl strm =
  evalStateT (S.distribute (S.hoist getTalosM strm)) (emptyTalosMState md nguid statshdl)

runTalosM :: Module -> GUID -> Maybe Handle -> TalosM a -> IO a
runTalosM md nguid statshdl m = evalStateT (getTalosM m) (emptyTalosMState md nguid statshdl)

emptyTalosMState :: Module -> GUID -> Maybe Handle -> TalosMState
emptyTalosMState md nguid statshdl = TalosMState
  { tmModule    = md
  -- Derived from the module
  , tmFunDefs   = funDefs
  , tmBFunDefs  = bfunDefs
  , tmIEnv      = env0
  , tmStatHandle = statshdl
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
-- Statistic collection

statistic :: LiftTalosM m => String -> String -> m ()
statistic name val = liftTalosM $ TalosM $ do
  m_hdl <- gets tmStatHandle
  case m_hdl of
    Nothing -> pure ()
    Just hdl -> liftIO $ do
      hPutStrLn hdl (name ++ ": " ++ val)
      hFlush hdl
       
-- -----------------------------------------------------------------------------
-- Lifting

class Monad m => LiftTalosM m where
  liftTalosM :: TalosM a -> m a

instance LiftTalosM TalosM where
  liftTalosM = id

instance LiftTalosM m => LiftTalosM (StateT s m) where
  liftTalosM = lift . liftTalosM
instance LiftTalosM m => LiftTalosM (ReaderT s m) where
  liftTalosM = lift . liftTalosM
instance (Monoid w, LiftTalosM m) => LiftTalosM (WriterT w m) where
  liftTalosM = lift . liftTalosM
instance LiftTalosM m => LiftTalosM (MaybeT m) where
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
