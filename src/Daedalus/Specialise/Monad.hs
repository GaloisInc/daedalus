{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-} -- for dealing with TCDecl and existential k

module Daedalus.Specialise.Monad where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Daedalus.PP
import Daedalus.Panic

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
              , nextNameIdx    :: Int
              }

emptyPApplyState :: PApplyState
emptyPApplyState = PApplyState Map.empty Map.empty Set.empty 0

newtype PApplyM a =
  PApplyM { getPApplyM :: StateT PApplyState (Except String) a }
  deriving (Functor, Applicative, Monad, MonadError String)

runPApplyM :: [Name] -> PApplyM a -> Either String a
runPApplyM roots m = runExcept (evalStateT (getPApplyM m)
                                 (emptyPApplyState { otherSeenRules = Set.fromList roots}))

-- clearSpecRequests :: Name -> PApplyM ()
-- clearSpecRequests nm =
--   -- Do we need pendingSpecs?  Maybe only for recursive calls.
--   PApplyM $ modify (\s -> s { pendingSpecs = Map.delete nm (pendingSpecs s) })


getPendingSpecs :: [Name] -> PApplyM (Map Name [Instantiation])
getPendingSpecs ns = PApplyM $ state go
  where
    go s = let (ret, keep) = Map.partitionWithKey (\k _ -> k `elem` ns) (pendingSpecs s)
           in (ret, s { pendingSpecs = keep })

-- | Add a new instance of declaration to the work queue.
-- We know that we haven't seen the spec request before,
-- so add it and mark as pending
addSpecRequest :: Name -> [Type] -> [TCName Value] -> [Maybe (Arg SourceRange)]
               -> PApplyM Name
addSpecRequest nm ts newPs args = PApplyM $ state go
  where
    go s = let nm'  = freshDeclName (nextNameIdx s)
               inst = Instantiation nm' ts newPs args
           in (nm',  s { requestedSpecs = Map.insertWith (++) nm [inst] (requestedSpecs s)
                       , pendingSpecs   = Map.insertWith (++) nm [inst] (pendingSpecs s)
                       , nextNameIdx    = nextNameIdx s + 1
                       } )

    freshDeclName nxt =
      nm { nameScope = case nameScope nm of
             ModScope m n -> ModScope m (n <> "__" <> T.pack (show nxt))
             _            -> panic "Expected ModScope" []
         }

lookupRequestedSpecs :: Name -> PApplyM (Maybe [Instantiation])
lookupRequestedSpecs nm = PApplyM $ gets (Map.lookup nm . requestedSpecs)

addSeenRule :: Name -> PApplyM ()
addSeenRule n = PApplyM $ modify go
  where
    go s = s { otherSeenRules = Set.insert n (otherSeenRules s) }

seenRule :: Name -> PApplyM Bool
seenRule n = PApplyM $ gets (Set.member n . otherSeenRules)
