{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.PolyglotReader where

import           Control.Monad.Reader  (Reader, ask, runReader)
import qualified Data.List             as List
import qualified Data.Map              as Map

import           Daedalus.Core
import           Daedalus.Core.CFG
import Daedalus.Core.CFG
import Daedalus.PP
import           Talos.Polyglot.Location

type PolyglotReader a = Reader PolyglotReaderState a

data PolyglotReaderState = PolyglotReaderState
  { cfCFGModule :: CFGModule
  , cfModule :: Module
  }

runPolyglotReader :: PolyglotReader a -> PolyglotReaderState -> a
runPolyglotReader = runReader

getEntrypoint :: FName -> PolyglotReader NodeID
getEntrypoint fname = do
  PolyglotReaderState{cfCFGModule=CFGModule{..}} <- ask
  return $ cfgfunEntry (cfgFuns Map.! fname)

getGFunParameterNames :: FName -> PolyglotReader [Name]
getGFunParameterNames fname = do
  PolyglotReaderState{cfModule=Module{..}} <- ask
  let fun = case List.find (\Fun{..} -> fName == fname) mGFuns of
        Just f -> f
        Nothing -> error "No fun in GFuns"
  return $ fParams fun

getBFun :: FName -> PolyglotReader (Fun ByteSet)
getBFun fname = do
  PolyglotReaderState{cfModule=Module{..}} <- ask
  let fun = case List.find (\Fun{..} -> fName == fname) mBFuns of
        Just f -> f
        Nothing -> error "No fun in BFuns"
  return fun

getFFun :: FName -> PolyglotReader (Fun Expr)
getFFun fname = do
  PolyglotReaderState{cfModule=Module{..}} <- ask
  let fun = case List.find (\Fun{..} -> fName == fname) mFFuns of
        Just f -> f
        Nothing -> error "No fun in FFuns"
  return fun

-- | Get the CFG node for a given location.  Calls `error` if the location does not
-- exist in the CFG.
getNode :: Loc -> PolyglotReader CFGNode
getNode (fname, nodeID) = do
  PolyglotReaderState{cfCFGModule=CFGModule{..}} <- ask
  let noFun      = error . show $ text "no CFG fun named" <+> pp fname
      CFGFun{..} = Map.findWithDefault noFun fname cfgFuns
      noCFG      = error . show $ text "no CFG node" <+> pp nodeID <+> text "in" <+> pp fname
  return $ Map.findWithDefault noCFG nodeID cfgfunCFG