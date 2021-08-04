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


import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Exception(Exception(..))
import MonadLib hiding (Label)

import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.GUID
import Daedalus.Pass

import Daedalus.Type.AST
import Daedalus.Type.Subst
import Daedalus.Type.Traverse
import Daedalus.Type.MonadClasses
import Daedalus.Type.Constraints

import Debug.Trace

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
  , sConstraints  :: !Constraints           -- ^ Constraints on type varis
  , sIP           :: !(Map IPName Param)    -- ^ Implicit param constraint
  , sNeedDef      :: ![Located TCTyName]    -- ^ Types used in sigs that
                                            --   were used before defined
  , sTypeDefs     :: !(Map TCTyName TCTyDecl)
  }

--------------------------------------------------------------------------------
data Constraints = Constraints
  { cLiteral :: Map TVar (Located Integer, Located Integer)
  , cCtr     :: [Located Constraint]
  }

noConstraints :: Constraints
noConstraints = Constraints { cLiteral = mempty, cCtr = mempty }

addCtr :: Located Constraint -> Constraints -> Constraints
addCtr c cs =
  case thingValue c of
    Literal n (TVar a) ->
      let i = c { thingValue = n }
      in cs { cLiteral = Map.insertWith jn a (i,i) (cLiteral cs) }

    _ -> cs { cCtr = c : cCtr cs }
  where
  jn (aL,aU) (bL,bU) =
    let it = (lmin aL bL, lmax aU bU)
    in trace ("Joining " ++ show (thingValue aL, thingValue aU)
                          ++ " with " ++ show (thingValue bL, thingValue bU)
                          ++ " -> " ++ show (thingValue (fst it), thingValue (snd it))) it
  lmin x y = if thingValue x < thingValue y then x else y
  lmax x y = if thingValue x < thingValue y then y else x

ctrLits :: (Located Integer, Located Integer) -> Type -> [Located Constraint]
ctrLits (l,u) t
  | x == y = [ l { thingValue = Literal x t } ]
  | otherwise = [ b { thingValue = Literal (thingValue b) t } | b <- [ l,u ] ]
  where
  x = thingValue l
  y = thingValue u

updLitCtr :: TVar -> Type -> Constraints -> ([Located Constraint], Constraints)
updLitCtr x t cs =
  case Map.lookup x (cLiteral cs) of
    Just bnds -> (ctrLits bnds t, cs { cLiteral = Map.delete x (cLiteral cs) })
    Nothing -> ([], cs)

ctrToList :: Constraints -> [Located Constraint]
ctrToList cs = [ c | (a,bnds) <- Map.toList (cLiteral cs)
                   , c <- ctrLits bnds (TVar a) ] ++ cCtr cs


--------------------------------------------------------------------------------

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
           , sConstraints = noConstraints
           , sIP          = Map.empty
           , sNeedDef     = []
           , sTypeDefs    = Map.empty
           }

mType :: MTypeM a -> STypeM a
mType m = STypeM (lift m)



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
    STypeM (sets' \s -> (ctrToList (sConstraints s), s { sConstraints = noConstraints }))

  addConstraint r c =
    do c' <- zonkT c
       let lc = Located { thingRange = range r, thingValue = c' }
       res <- solveConstraint lc
       case res of
         Solved -> traceM ("Solved " ++ show (pp c')) >> pure ()
         Unsolved ->
               STypeM (sets_ \s -> s { sConstraints = addCtr lc (sConstraints s) })

  addTVarDef x t =
    do todo <- STypeM $ sets \s ->
                 let su = Map.singleton x t
                     (new,cs) = updLitCtr x t (sConstraints s)
                 in (new
                    , s { sSubst    = Map.insert x t (apSubstT su <$> sSubst s)
                        , sTypeDefs = apSuTCTyDecl su <$> sTypeDefs s
                        , sConstraints = cs
                        }
                    )
       mapM_ (\lc -> addConstraint lc (thingValue lc)) todo

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
  { sLocalTyVars  :: !(Map Text Type) -- ^ Names for local types (from sigs)
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


lookupLocalTyVar :: Text -> TypeM ctx (Maybe Type)
lookupLocalTyVar x = TypeM (Map.lookup x . sLocalTyVars <$> get)

newLocalTyVar :: Text -> Type -> TypeM ctx ()
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



