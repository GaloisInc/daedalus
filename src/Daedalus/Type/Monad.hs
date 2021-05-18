{-# Language BlockArguments, OverloadedStrings, DataKinds #-}
{-# Language NamedFieldPuns #-}
{-# Language TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveFunctor #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
module Daedalus.Type.Monad
  ( 
    -- * Typechking a module
    MTypeM, runMTypeM, MTCMonad

    -- * Typechecking a group of declarations
  , STypeM, runSTypeM, STCMonad

    -- * Typechecking a single declartion
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
  , addCon
  , lookupTypeDef
  , lookupTypeDefMaybe
  , isBitData
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

    -- * Implicit parameters
  , addIPUse
  , removeIPUses
  , withIP
  , lookupIP
  , getUndefinedIPs
  ) where


import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Exception(Exception(..))
import MonadLib hiding (Label)

import qualified Daedalus.BDD as BDD
import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.GUID
import Daedalus.Pass
import Daedalus.Panic(panic)

import Daedalus.Type.AST
import Daedalus.Type.Subst
import Daedalus.Type.Traverse


--------------------------------------------------------------------------------
-- Type errors

newtype TypeError = TypeError (Located Doc)
                      deriving Show

instance PP TypeError where
  pp (TypeError l) =
    hang (text (prettySourceRangeLong (thingRange l)) <.> colon)
        2 (thingValue l)

instance Exception TypeError where
  displayException = show . pp


class Monad m => MTCMonad m where
  reportError     :: HasRange a => a -> Doc  -> m b
  newName'        :: HasRange r => r -> Context a -> Type -> m (TCName a)
  getRuleEnv      :: m RuleEnv
  getGlobTypeDefs :: m (Map TCTyName TCTyDecl)
  extEnvManyRules :: [(Name,Poly RuleType)] -> m a -> m a


reportDetailedError :: MTCMonad m => HasRange a => a -> Doc -> [Doc] -> m b
reportDetailedError r d ds = reportError r (d $$ nest 2 (bullets ds))

newName :: (MTCMonad m, HasRange r) => r -> Type -> m (TCName Value)
newName r t = newName' r AValue t



--------------------------------------------------------------------------------
-- Module-level typing monad

newtype MTypeM a = MTypeM { getMTypeM :: 
                            WithBase PassM
                              '[ ReaderT MRO
                               , ExceptionT TypeError
                               ] a
                          }
                   
deriving instance Functor MTypeM

instance Applicative MTypeM where
  MTypeM m <*> MTypeM m' = MTypeM (m <*> m')
  pure v = MTypeM $ pure v
  
instance Monad MTypeM where
  MTypeM m >>= f = MTypeM (m >>= getMTypeM . f)

type RuleEnv  = Map Name (Poly RuleType)

data MRO = MRO
  { roRuleTypes     :: !RuleEnv
  , roTypeDefs      :: !(Map TCTyName TCTyDecl)
  }

-- XXX: maybe preserve something about the state?
runMTypeM :: Map TCTyName TCTyDecl ->
             RuleEnv ->
             MTypeM a -> PassM (Either TypeError a)
runMTypeM tenv renv (MTypeM m) = runExceptionT $ runReaderT r0 m 
  where r0   = MRO { roRuleTypes = renv
                   , roTypeDefs  = tenv
                   }

instance HasGUID MTypeM where
  guidState f = MTypeM $ inBase (guidState f)

instance MTCMonad MTypeM where
  reportError r s =
    MTypeM (raise (TypeError Located { thingRange = range r, thingValue = s }))

  newName' r ctx ty = do
    n <- getNextGUID
    pure let txt = Text.pack ("_" ++ show (pp n))
         in TCName { tcName =
                             Name { nameScopedIdent = Local txt
                                  , nameContext     = ctx
                                  , nameRange       = range r
                                  , nameID          = n
                                  }
                         , tcType = ty
                         , tcNameCtx = ctx
                         }

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
  , sIP           :: !(Map IPName Param)    -- ^ Implicit param constraint
  , sNeedDef      :: ![Located TCTyName]    -- ^ Types used in sigs that
                                            --   were used before defined
  , sTypeDefs     :: !(Map TCTyName TCTyDecl)
  }

instance HasGUID STypeM where
  guidState f = mType (guidState f)

instance MTCMonad STypeM where
  reportError r e     = mType (reportError r e)
  newName' r c t      = mType (newName' r c t)
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
           , sIP          = Map.empty
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
  addIPUse          :: IPName -> m Param
  removeIPUses      :: m [(IPName,Param)]



-- | The Bool indicates if this is a type that is in the process of being 
-- defined.
lookupTypeDef :: STCMonad m => TCTyName -> m (Maybe (TCTyDecl,Bool))
lookupTypeDef x =
  do defs <- getNewTypeDefs
     case Map.lookup x defs of
       Just d -> pure (Just (d, True))
       Nothing ->
        do gdefs <- getGlobTypeDefs
           pure case Map.lookup x gdefs of
                  Just g -> Just (g, False)
                  Nothing -> Nothing

-- | Check if this type is a bitdata, and if so tell us what we know about it
isBitData :: STCMonad m => TCTyName -> m (Maybe BDD.Pat)
isBitData x =
  do mb <- lookupTypeDef x
     case mb of
       Nothing -> pure Nothing
       Just (td,_) -> pure (tctyBD td)

lookupTypeDefMaybe :: STCMonad m => TCTyName -> m (Maybe TCTyDecl)
lookupTypeDefMaybe x = fmap fst <$> lookupTypeDef x

addCon :: STCMonad m => TCTyName -> Label -> Type -> m ()
addCon x l ft =
  do defs <- getNewTypeDefs
     case Map.lookup x defs of
       Just decl | TCTyUnion fs <- tctyDef decl ->
         do let d1 = decl { tctyDef = TCTyUnion ((l,(ft,Nothing)):fs) }
                ds1 = Map.insert x d1 defs
            replaceNewTypeDefs ds1
       _ -> panic "addCon" [ "Cannot add constructor to a struct/undefined." ]


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
                           , tctyBD = Nothing
                           , tctyDef = def
                           }
       STypeM $ sets_ $ \s -> s { sTypeDefs = Map.insert x decl (sTypeDefs s) }

  addIPUse x =
    do mb <- STypeM (Map.lookup x . sIP <$> get)
       case mb of
         Just p -> pure p
         Nothing ->
           do p <- newIPParam x
              STypeM $ sets \s -> (p, s { sIP = Map.insert x p (sIP s) })

  removeIPUses =
    do xs <- STypeM (Map.toList . sIP <$> get)
       forM xs \(i,t) -> (,) i <$> traverseTypes zonkT t

newIPParam :: STCMonad m => IPName -> m Param
newIPParam x@IPName { ipContext } =
  case ipContext of
    AValue ->
      do t <- newTVar x KValue
         ValParam <$> newName' x AValue t

    AClass ->
      ClassParam <$> newName' x AClass tByteClass

    AGrammar ->
      do t <- newTVar x KValue
         GrammarParam <$> newName' x AGrammar (tGrammar t)

 

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
  , roIP          :: !(Map IPName (Arg SourceRange))  -- ^ IPs in scope
  , allowPartial  :: !Bool           -- ^ Are partial apps OK?
  , roContext     :: !(Context ctx)  -- ^ Current context (lazy)
  }

data RW = RW
  { sLocalTyVars  :: !(Map Name Type) -- ^ Names for local types (from sigs)
  , sNextType     :: !Int             -- ^ Used to generate variants of the root
  , sIPUsed       :: !(Set IPName)
    -- ^ IPs (from the ones in scope) that were used.  We keep track of this
    -- so that we can report "undefined IP" for IPs that weren't used
  }


runTypeM :: Name -> TypeM Grammar a -> STypeM a
runTypeM n (TypeM m) = fst <$> runStateT rw (runReaderT ro m)
  where
  ro = RO { roEnv        = Map.empty
          , roName       = n
          , roIP         = Map.empty
          , allowPartial = False
          , roContext    = AGrammar
          }
  rw = RW { sLocalTyVars = Map.empty, sNextType = 0, sIPUsed = Set.empty }

sType :: STypeM a -> TypeM ctx a
sType m = TypeM (lift (lift m))

instance MTCMonad (TypeM ctx) where
  reportError r e     = sType (reportError r e)
  newName' r c t      = sType (newName' r c t)
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
  addIPUse x              = sType (addIPUse x)
  removeIPUses            = sType removeIPUses

instance HasGUID (TypeM ctx) where
  guidState f = sType (guidState f)

allowPartialApps :: Bool -> TypeM ctx a -> TypeM ctx a
allowPartialApps yes (TypeM m) = TypeM (mapReader upd m)
  where upd ro = ro { allowPartial = yes }

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

withIP :: IPName -> Arg SourceRange -> TypeM ctx a -> TypeM ctx a
withIP x t (TypeM m) = TypeM
  do ro   <- ask
     a    <- local ro { roIP = Map.insert x t (roIP ro) } m
     sets \s -> (a, s { sIPUsed = Set.delete x (sIPUsed s) })

lookupIP :: IPName -> TypeM ctx (Maybe (Arg SourceRange))
lookupIP x = TypeM
  do ro <- ask
     case Map.lookup x (roIP ro) of
       Nothing -> pure Nothing
       Just i  -> sets \s -> (Just i, s { sIPUsed = Set.insert x (sIPUsed s) })

getUndefinedIPs :: TypeM ctx [ IPName ]
getUndefinedIPs = TypeM
  do ro <- ask
     s  <- get
     pure [ x | x <- Map.keys (roIP ro), not (x `Set.member` sIPUsed s) ]


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
  doField :: (Label, (Type, a)) -> (Label, (Type, a))
  doField (f,(t,m)) = (f,(apSubstT su t, m))


--------------------------------------------------------------------------------
sets' :: StateM m s => (s -> (a,s)) -> m a
sets' f = sets $ \s -> case f s of
                        (a,s1) -> a `seq` s1 `seq` (a,s1)
{-# INLINE sets' #-}



