{-# Language BlockArguments, OverloadedStrings, DataKinds #-}
{-# Language NamedFieldPuns #-}
{-# Language TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
module Daedalus.Type.Monad
  ( 
    -- * Typechking a module
    MTypeM, runMTypeM, MTCMonad

    -- * Typechecking a group of declarations
  , STypeM, runSTypeM, STCMonad

    -- * Typechecking a single declart
  , TypeM, runTypeM

    -- * Error reporting
  , TypeError(..)
  , reportError
  , reportDetailedError

    -- * Local Environemnt
  , getEnv
  , extEnv

    -- * Rule Environemnt
  , RuleEnv
  , getRuleEnv
  , extEnvManyRules
  , lookupRuleTypeOf
  , RuleInfo(..)

    -- * Local type variables
  , lookupLocalTyVar
  , newLocalTyVar

    -- * Name generation
  , newName

    -- * Type definitions
  , newTyDefName
  , newTypeDef
  , getNewTypeDefs
  , replaceNewTypeDefs
  , lookupTypeDef
  , extGlobTyDefs
  , getGlobTypeDefs

  -- * Contexts
  , inContext
  , getContext
  , allowPartialApps
  , arePartialAppsOK

    -- * Unification variables
  , newTVar
  , addTVarDef
  , zonkT
  , getTypeSubst
  , instantiate

    -- * Constraints
  , addConstraint
  , removeConstraints
  , needsDef
  , getNeedsDef
  ) where


import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Exception(Exception(..))
import MonadLib

import Daedalus.SourceRange
import Daedalus.PP

import Daedalus.Type.AST
import Daedalus.Type.Subst
import Daedalus.Type.Traverse


--------------------------------------------------------------------------------
-- Type errors

newtype TypeError = TypeError (Located Doc)
                      deriving Show

instance PP TypeError where
  pp (TypeError l) =
    text (prettySourceRange (thingRange l)) <.> colon <+> thingValue l

instance Exception TypeError where
  displayException = show . pp


class Monad m => MTCMonad m where
  reportError     :: HasRange a => a -> Doc  -> m b
  newName         :: HasRange r => r -> Type -> m (TCName Value)
  getRuleEnv      :: m RuleEnv
  getGlobTypeDefs :: m (Map TCTyName TCTyDecl)
  extEnvManyRules :: [(Name,Poly RuleType)] -> m a -> m a


reportDetailedError :: MTCMonad m => HasRange a => a -> Doc -> [Doc] -> m b
reportDetailedError r d ds = reportError r (d $$ nest 2 (bullets ds))





--------------------------------------------------------------------------------
-- Module-level typing monad

newtype MTypeM a = MTypeM ( WithBase Id
                              '[ ReaderT MRO
                               , StateT  MRW
                               , ExceptionT TypeError
                               ] a
                          ) deriving (Functor,Applicative,Monad)

type RuleEnv  = Map Name (Poly RuleType)

data MRO = MRO
  { roRuleTypes     :: !RuleEnv
  , roTypeDefs      :: !(Map TCTyName TCTyDecl)
  }

data MRW = MRW
  { sNextValName    :: !Int -- ^ Generate names.
  }



-- XXX: maybe preserve something about the state?
runMTypeM :: Map TCTyName TCTyDecl ->
             RuleEnv ->
             MTypeM a -> Either TypeError a
runMTypeM tenv renv (MTypeM m) =
  case runId $ runExceptionT $ runStateT s0 $ runReaderT r0 m of
    Left err    -> Left err
    Right (a,_) -> Right a

  where r0   = MRO { roRuleTypes = renv
                   , roTypeDefs  = tenv
                   }
        s0   = MRW { sNextValName   = 0 -- XXX: 
                   }



instance MTCMonad MTypeM where
  reportError r s =
    MTypeM (raise (TypeError Located { thingRange = range r, thingValue = s }))

  newName r ty = MTypeM $ sets' \s ->
    let n  = sNextValName s
        nm = case kindOf ty of
               KValue ->
                 let txt = Text.pack ("_" ++ show n)
                 in TCName { tcName =
                               Name { nameScope   = Local txt
                                    , nameContext = AValue
                                    , nameRange   = range r
                                    }
                           , tcType = ty
                           , tcNameCtx = AValue
                           }
               k -> error ("bug: new name of unexpected kind: " ++ show k)
    in (nm, s { sNextValName = n + 1 })


  getRuleEnv = MTypeM (roRuleTypes <$> ask)

  extEnvManyRules xs (MTypeM m) = MTypeM (mapReader upd m)
    where
    newEnv = Map.fromList xs
    upd ro = ro { roRuleTypes = Map.union newEnv (roRuleTypes ro) }


  getGlobTypeDefs = MTypeM (roTypeDefs <$> ask)

extGlobTyDefs :: Map TCTyName TCTyDecl -> MTypeM a -> MTypeM a
extGlobTyDefs mp (MTypeM m) = MTypeM $
  mapReader (\ro -> ro { roTypeDefs = Map.union mp (roTypeDefs ro) }) m


--------------------------------------------------------------------------------

-- | Monad used to type-check a group of top-level declaraionts.
newtype STypeM a = STypeM ( WithBase MTypeM '[ StateT SRW ] a )
                    deriving (Functor,Applicative,Monad)

type Env = Map Name Type

data SRW = SRW
  { sSubst        :: !(Map TVar Type)       -- ^ Idempotent substitution
  , sNextTName    :: !Int                   -- ^ Generate type variables
  , sConstraints  :: ![Located Constraint]  -- ^ Constraints on type varis
  , sNeedDef      :: ![Located TCTyName]    -- ^ Types used in sigs that
                                            --   were used before defined
  , sTypeDefs     :: !(Map TCTyName TCTyDecl)
  }

instance MTCMonad STypeM where
  reportError r e     = mType (reportError r e)
  newName r t         = mType (newName r t)
  getRuleEnv          = mType getRuleEnv
  getGlobTypeDefs     = mType getGlobTypeDefs
  extEnvManyRules rs (STypeM m) =
    STypeM
    do rw <- get
       (a,rw1) <- lift $ extEnvManyRules rs $ runStateT rw m
       set rw1
       pure a


runSTypeM :: STypeM a -> MTypeM a
runSTypeM (STypeM m) = fst <$> runStateT rw m
  where
  rw = SRW { sSubst       = Map.empty
           , sNextTName   = 0
           , sConstraints = []
           , sNeedDef     = []
           , sTypeDefs    = Map.empty
           }

mType :: MTypeM a -> STypeM a
mType m = STypeM (lift m)


class MTCMonad m => STCMonad m where
  getTypeSubst      :: m (Map TVar Type)
  getNewTypeDefs    :: m (Map TCTyName TCTyDecl)

  newTVar'          :: HasRange r => r -> Kind -> m TVar
  newTypeDef        :: TCTyName -> TCTyDef -> m ()
  replaceNewTypeDefs :: Map TCTyName TCTyDecl -> m ()

  removeConstraints :: m [Located Constraint]
  addConstraint     :: HasRange r => r -> Constraint -> m ()
  needsDef          :: HasRange r => r -> TCTyName -> m ()
  getNeedsDef       :: m [Located TCTyName]
  addTVarDef        :: TVar -> Type -> m ()



lookupTypeDef :: STCMonad m => TCTyName -> m (Maybe TCTyDecl)
lookupTypeDef x =
  do defs <- getNewTypeDefs
     case Map.lookup x defs of
       Just d -> pure (Just d)
       Nothing ->
        do gdefs <- getGlobTypeDefs
           pure (Map.lookup x gdefs)


instance STCMonad STypeM where

  getTypeSubst      = STypeM (sSubst <$> get)

  getNewTypeDefs    = do ds <- STypeM (sTypeDefs <$> get)
                         su <- getTypeSubst
                         pure (mapTypes (apSubstT su) <$> ds)

  replaceNewTypeDefs mp = STypeM $ sets_ \s -> s { sTypeDefs = mp }

  newTVar' r k = STypeM $ sets' \s ->
    let n = sNextTName s
        x = TV { tvarId    = n
               , tvarKind  = k
               , tvarRange = range r
               }
    in (x, s { sNextTName = n + 1 })

  needsDef r d =
    STypeM (sets_ \s -> s { sNeedDef = Located { thingRange = range r
                                               , thingValue = d
                                               } : sNeedDef s })

  getNeedsDef = STypeM (sNeedDef <$> get)

  removeConstraints =
    STypeM (sets' \s -> (sConstraints s, s { sConstraints = [] }))

  addConstraint r c =
    STypeM (sets_ \s -> s { sConstraints = lc : sConstraints s })
    where lc = Located { thingRange = range r, thingValue = c }

  addTVarDef x t = STypeM $ sets_ \s ->
      let su = Map.singleton x t
      in s { sSubst    = Map.insert x t (apSubstT su <$> sSubst s)
           , sTypeDefs = apSuTCTyDecl su <$> sTypeDefs s
           }

  newTypeDef x def' =
    do def <- traverseTypes zonkT def'
       let decl = TCTyDecl { tctyName = x
                           , tctyParams = []
                           , tctyDef = def
                           }
       STypeM $ sets_ $ \s -> s { sTypeDefs = Map.insert x decl (sTypeDefs s) }


zonkT :: (ApSubst t, STCMonad m) => t -> m t
zonkT t = do su  <- getTypeSubst
             pure (apSubstT su t)

newTVar :: (STCMonad m, HasRange r) => r -> Kind -> m Type
newTVar r k = TVar <$> newTVar' r k


data RuleInfo = TopRule [Type] RuleType
              | LocalRule Type


lookupRuleTypeOf :: Name -> TypeM ctx RuleInfo
lookupRuleTypeOf x =
  do mb <- Map.lookup x <$> getEnv
     case mb of
       Just t -> pure (LocalRule t)
       Nothing ->
         do mbr <- Map.lookup x <$> getRuleEnv
            case mbr of
              Nothing -> reportError x ("Undeclared name:" <+> pp x)
              Just rt -> uncurry TopRule <$> instantiate x rt

instantiate :: HasRange r => r -> Poly RuleType -> TypeM ctx ([Type], RuleType)
instantiate r (Poly as cs t) =
  do ts <- forM as \a -> newTVar r (tvarKind a)
     let su = Map.fromList (as `zip` ts)
         fresh ty = apSubstT su ty
     mapM_ (addConstraint r) (apSubstT su cs)
     pure (ts,mapTypes fresh t)


--------------------------------------------------------------------------------


-- | Monad for type-checking a single declaration
newtype TypeM ctx a = TypeM ( WithBase STypeM '[ ReaderT (RO ctx)
                                               , StateT  RW
                                               ] a )
                              deriving (Functor,Applicative,Monad)



data RO ctx = RO
  { roEnv         :: !Env            -- ^ Types for locals
  , roName        :: !Name           -- ^ Root name for generating type decls
  , allowPartial  :: !Bool           -- ^ Are partial apps OK?
  , roContext     :: !(Context ctx)  -- ^ Current context (lazy)
  }

data RW = RW
  { sLocalTyVars  :: !(Map Name Type) -- ^ Names for local types (from sigs)
  , sNextType     :: !Int             -- ^ Used to generate variants of the root
  }


runTypeM :: Name -> TypeM Grammar a -> STypeM a
runTypeM n (TypeM m) = fst <$> runStateT rw (runReaderT ro m)
  where
  ro = RO { roEnv        = Map.empty
          , roName       = n
          , allowPartial = False
          , roContext    = AGrammar
          }
  rw = RW { sLocalTyVars = Map.empty, sNextType = 0 }

sType :: STypeM a -> TypeM ctx a
sType m = TypeM (lift (lift m))

instance MTCMonad (TypeM ctx) where
  reportError r e     = sType (reportError r e)
  newName r t         = sType (newName r t)
  getRuleEnv          = sType getRuleEnv
  getGlobTypeDefs     = sType getGlobTypeDefs
  extEnvManyRules rs (TypeM m) =
    TypeM
    do ro <- ask
       rw <- get
       (a,rw1) <- lift $ lift
               $ extEnvManyRules rs $ runStateT rw $ runReaderT ro m
       set rw1
       pure a

instance STCMonad (TypeM ctx) where
  getTypeSubst            = sType getTypeSubst
  getNewTypeDefs          = sType getNewTypeDefs
  replaceNewTypeDefs xs   = sType (replaceNewTypeDefs xs)
  newTVar' r k            = sType (newTVar' r k)
  newTypeDef x d          = sType (newTypeDef x d)
  removeConstraints       = sType removeConstraints
  addConstraint r c       = sType (addConstraint r c)
  addTVarDef x t          = sType (addTVarDef x t)
  needsDef r d            = sType (needsDef r d)
  getNeedsDef             = sType getNeedsDef


allowPartialApps :: TypeM ctx a -> TypeM ctx a
allowPartialApps (TypeM m) = TypeM (mapReader upd m)
  where upd ro = ro { allowPartial = True }

arePartialAppsOK :: TypeM ctx Bool
arePartialAppsOK = TypeM (allowPartial <$> ask)


lookupLocalTyVar :: Name -> TypeM ctx (Maybe Type)
lookupLocalTyVar x = TypeM (Map.lookup x . sLocalTyVars <$> get)

newLocalTyVar :: Name -> Type -> TypeM ctx ()
newLocalTyVar x t =
  TypeM (sets_ \s -> s { sLocalTyVars = Map.insert x t (sLocalTyVars s) })

inContext :: Context ctx -> TypeM ctx a -> TypeM ctx1 a
inContext c (TypeM m) = TypeM do ro <- ask
                                 let ro1 = ro { roContext = c }
                                 lift (runReaderT ro1 m)

getContext :: TypeM ctx (Context ctx)
getContext = TypeM (roContext <$> ask)

getEnv :: TypeM ctx Env
getEnv = TypeM (roEnv <$> ask)

extEnv :: Name -> Type -> TypeM ctx a -> TypeM ctx a
extEnv x t (TypeM m) = TypeM (mapReader upd m)
  where upd ro = ro { roEnv = Map.insert x t (roEnv ro) }

newTyDefName :: TypeM ctx TCTyName
newTyDefName = TypeM $
    do root <- roName <$> ask
       sets' \s ->
        let n  = sNextType s
            nm = TCTyAnon root n
        in (nm, s { sNextType = n + 1 })




apSuTCTyDecl :: Map TVar Type -> TCTyDecl -> TCTyDecl
apSuTCTyDecl su d = d { tctyDef = apSubstTCTyDef su (tctyDef d) }

apSubstTCTyDef :: Map TVar Type -> TCTyDef -> TCTyDef
apSubstTCTyDef su def =
  case def of
    TCTyStruct fs -> TCTyStruct (map doField fs)
    TCTyUnion  fs -> TCTyUnion  (map doField fs)
  where
  doField (f,t) = (f,apSubstT su t)


--------------------------------------------------------------------------------
sets' :: StateM m s => (s -> (a,s)) -> m a
sets' f = sets $ \s -> case f s of
                        (a,s1) -> a `seq` s1 `seq` (a,s1)
{-# INLINE sets' #-}



