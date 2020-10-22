{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-} 
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Daedalus.Specialise.Monad where

import MonadLib
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Daedalus.PP
import Daedalus.Panic
import Daedalus.GUID

import Daedalus.Specialise.PartialApply
import Daedalus.Type.AST

-- INV: None of the below contain free TVs, and only variables in
-- instNewParams may appear free in instArgs
data Instantiation =
  Instantiation { instNewName   :: Name
                , instTys       :: [Type]
                , instNewParams :: [TCName Value]
                , instArgs      :: [Maybe (Arg SourceRange)]
                }

apInst :: Instantiation -> TCDecl SourceRange -> TCDecl SourceRange
apInst Instantiation {..} =
  partialApply instNewName instTys instNewParams instArgs

instance PP Instantiation where
  pp Instantiation {..} =
    text (show instNewName)
    <+> hsep (map pp instNewParams)
    <+> parens (hsep (map pp instTys ++ map ppA instArgs))
    where
      ppA Nothing = text "_"
      ppA (Just v) = pp v

data PApplyState =
  PApplyState { requestedSpecs :: Map Name [ Instantiation ]
              -- Subset of the above
              , pendingSpecs   :: Map Name [ Instantiation ]
              , otherSeenRules :: Set Name
              }

emptyPApplyState :: PApplyState
emptyPApplyState = PApplyState Map.empty Map.empty Set.empty 

newtype PApplyM a =
  PApplyM { getPApplyM :: forall m. HasGUID m => ExceptionT String (StateT PApplyState m) a }

deriving instance Functor PApplyM

instance Applicative PApplyM where
  PApplyM m <*> PApplyM m' = PApplyM (m <*> m')
  pure v = PApplyM $ pure v
  
instance Monad PApplyM where
  PApplyM m >>= f = PApplyM (m >>= getPApplyM . f)

instance ExceptionM PApplyM [Char] where
  raise s = PApplyM (raise s)

instance HasGUID PApplyM where
  getNextGUID = PApplyM $ lift (lift getNextGUID)

runPApplyM :: forall f a. HasGUID f => [Name] -> PApplyM a -> f (Either String a)
runPApplyM roots m = fst <$> runStateT s0 (runExceptionT (getPApplyM m @f))
  where s0 = emptyPApplyState  { otherSeenRules = Set.fromList roots }

-- clearSpecRequests :: Name -> PApplyM ()
-- clearSpecRequests nm =
--   -- Do we need pendingSpecs?  Maybe only for recursive calls.
--   PApplyM $ modify (\s -> s { pendingSpecs = Map.delete nm (pendingSpecs s) })


getPendingSpecs :: [Name] -> PApplyM (Map Name [Instantiation])
getPendingSpecs ns = PApplyM $ sets go
  where
    go s = let (ret, keep) = Map.partitionWithKey (\k _ -> k `elem` ns) (pendingSpecs s)
           in (ret, s { pendingSpecs = keep })

-- | Add a new instance of declaration to the work queue.
-- We know that we haven't seen the spec request before,
-- so add it and mark as pending
addSpecRequest :: ModuleName
               -> Name
               -> [Type]
               -> [TCName Value]
               -> [Maybe (Arg SourceRange)]
               -> PApplyM Name
addSpecRequest modName nm ts newPs args = do
  nguid <- getNextGUID
  let nm'  = freshDeclName nguid
      inst = Instantiation nm' ts newPs args
  PApplyM $ sets_ $ \s -> s { requestedSpecs = Map.insertWith (++) nm [inst] (requestedSpecs s)
                            , pendingSpecs   = Map.insertWith (++) nm [inst] (pendingSpecs s)
                            }
  pure nm'
  where
    freshDeclName guid  =
      nm { nameScopedIdent = case nameScopedIdent nm of
             ModScope _ n -> ModScope modName (n <> "__" <> T.pack (show (pp guid)))
             _            -> panic "Expected ModScope" []
         , nameID = guid
         }

lookupRequestedSpecs :: Name -> PApplyM (Maybe [Instantiation])
lookupRequestedSpecs nm = PApplyM $ (Map.lookup nm . requestedSpecs) <$> get

addSeenRule :: Name -> PApplyM ()
addSeenRule n = PApplyM $ sets_ go
  where
    go s = s { otherSeenRules = Set.insert n (otherSeenRules s) }

seenRule :: Name -> PApplyM Bool
seenRule n = PApplyM $ (Set.member n . otherSeenRules) <$> get
