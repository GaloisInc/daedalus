{-# Language GeneralizedNewtypeDeriving, DefaultSignatures, TypeFamilies #-}

module Daedalus.Core.SpecialiseType (specialiseTypes) where

-- Instantiates polymorphic instances of types to be monomorphic.
-- Note that functions are already monomorphic, so we just need to
-- find instances of usertypes with arguments and construct
-- specialised versions
--
-- We do this in 2 phases: a linear pass over the functions, and then
-- a fixpoint over the types (processing a type may introduce further
-- specialised types)

import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MonadLib

import Daedalus.GUID
import Daedalus.Panic
import Daedalus.Rec

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.TraverseUserTypes

-- -----------------------------------------------------------------------------
-- Top level function

specialiseTypes :: HasGUID m => Module -> m  Module
specialiseTypes m = fst <$> runStateT initSpecTyState (getSpecTyM (specTyM m))

specTyM :: HasGUID m => Module -> SpecTyM m Module
specTyM m = do
  ffs  <- traverse specTy (mFFuns m)
  bfs  <- traverse specTy (mBFuns m)
  gfs  <- traverse specTy (mGFuns m)
  tys' <- traverse specTy tys
    
  newTys <- specTyDecls pTys
    
  let mTys = topoOrder (\td -> (tName td, freeTCons td)) (newTys ++ tys')

  pure $ m { mFFuns = ffs, mBFuns = bfs, mGFuns = gfs, mTypes = mTys }
  where
    (pTys, tys) = partition isParamTy allTys        
    allTys = forgetRecs (mTypes m)
    isParamTy td = not (null (tTParamKNumber td) && null (tTParamKValue td))

-- -----------------------------------------------------------------------------
-- Monad

data SpecTyState =
  SpecTyState { stsSeenTypes :: Map UserType UserType
              , stsWorklist  :: Set UserType -- ^ subset of (dom seenTypes)
              }

initSpecTyState :: SpecTyState
initSpecTyState = SpecTyState { stsSeenTypes = mempty, stsWorklist = mempty }

newtype SpecTyM m a =
  SpecTyM { getSpecTyM :: StateT SpecTyState m a }
  deriving (Functor, Applicative, Monad, HasGUID)
  
-- Base case, the argument is normalised (this should not be called
-- directly, use specTy on UserTypes)
processUserType :: HasGUID m => UserType -> SpecTyM m UserType
processUserType ut = do
  m_ut' <- Map.lookup ut . stsSeenTypes <$> SpecTyM get 
  case m_ut' of
    Just ut' -> pure ut'
    Nothing  -> do
      n' <- freshTName (utName ut)
      let ut' = UserType { utName = n', utNumArgs = [], utTyArgs = [] }
      SpecTyM $ sets_ (\s -> s { stsSeenTypes = Map.insert ut ut' (stsSeenTypes s)
                               , stsWorklist  = Set.insert ut (stsWorklist s)
                               })
      pure ut'

-- Gets the next element of the worklist
getNextWL :: Monad m => SpecTyM m (Maybe (UserType, UserType))
getNextWL = SpecTyM (sets go)
  where
    go s
      | Just (ut, wl) <- Set.minView (stsWorklist s)
      , Just ut'      <- Map.lookup ut (stsSeenTypes s) =
          (Just (ut, ut'), s { stsWorklist = wl })
      | otherwise = (Nothing, s)
    
-- -----------------------------------------------------------------------------
-- Worker functions (second phase)

specTyDecls :: HasGUID m => [TDecl] -> SpecTyM m [TDecl]
specTyDecls polyTys = go []
  where
    go acc = do
      m_next <- getNextWL
      case m_next of
        Nothing -> pure acc
        Just (ut, ut') | Just td <- Map.lookup (utName ut) tyMap -> do
              let td' = instantiateTDecl ut ut' td
              td'' <- specTy td'
              go (td'' : acc)

        Just {} -> panic "Missing instantiation" []
        
    tyMap = Map.fromList [ (tName td, td) | td <- polyTys ]

instantiateTDecl :: UserType -> UserType -> TDecl -> TDecl
instantiateTDecl orig new td =
  TDecl { tName = utName new
        , tTParamKNumber = []
        , tTParamKValue  = []
        , tDef = def'
        }
  where
    def' = case tDef td of
             TStruct ls -> TStruct (goLabeled ls)
             TUnion  ls -> TUnion  (goLabeled ls)
             TBitdata b d -> TBitdata b d -- nothing to instnatiate here

    goTy ty =
      case ty of
        TStream   -> ty
        TUInt tsz -> TUInt (goSize tsz)
        TSInt tsz -> TSInt (goSize tsz)
        TInteger  -> ty
        TBool     -> ty
        TFloat    -> ty
        TDouble   -> ty
        TUnit     -> ty
        TArray ty' -> TArray (goTy ty')
        TMaybe ty' -> TMaybe (goTy ty')
        TMap dTy rTy -> TMap (goTy dTy) (goTy rTy)
        TBuilder ty' -> TBuilder (goTy ty')
        TIterator ty' -> TIterator (goTy ty')
        TUser ut      -> TUser (ut { utNumArgs = map goSize (utNumArgs ut)
                                   , utTyArgs  = map goTy (utTyArgs ut)
                                   })
        TParam p
          | Just ty' <- Map.lookup p tenv -> ty'
          | otherwise -> panic "Missing type param" []

    goSize (TSizeParam p) 
      | Just n <- Map.lookup p nenv = n -- _should_ be a size? 
      | otherwise = panic "Missing type param" []
    goSize sz = sz

    goLabeled = map (\(l, t) -> (l, goTy t))

    nenv = Map.fromList (zip (tTParamKNumber td) (utNumArgs orig))
    tenv = Map.fromList (zip (tTParamKValue  td) (utTyArgs orig))
        
-- -----------------------------------------------------------------------------
-- Worker functions (first phase)

specTy :: (HasGUID m, TraverseUserTypes t) => t -> SpecTyM m t
specTy = traverseUserTypes go
  where
    go ut = 
      case ut of
        UserType { utNumArgs = [], utTyArgs = [] } -> pure ut
        _ -> do 
          targs' <- specTy (utTyArgs ut)
          processUserType (ut { utTyArgs = targs' })
