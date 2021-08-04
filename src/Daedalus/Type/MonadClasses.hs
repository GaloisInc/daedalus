{-# Language BlockArguments, OverloadedStrings, DataKinds #-}
{-# Language NamedFieldPuns #-}
{-# Language TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveFunctor #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
module Daedalus.Type.MonadClasses where


import Data.Text(Text)
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

type RuleEnv  = Map Name (Poly RuleType)

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


-- | Add a constructor to an existing type declaration.
addCon :: STCMonad m => TCTyName -> Label -> Type -> m ()
addCon x l ft =
  do defs <- getNewTypeDefs
     case Map.lookup x defs of
       Just decl | TCTyUnion fs <- tctyDef decl ->
         do let d1 = decl { tctyDef = TCTyUnion ((l,(ft,Nothing)):fs) }
                ds1 = Map.insert x d1 defs
            replaceNewTypeDefs ds1
       _ -> panic "addCon" [ "Cannot add constructor to a struct/undefined." ]



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


