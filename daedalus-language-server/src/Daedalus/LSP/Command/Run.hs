{-# LANGUAGE OverloadedStrings #-}

-- | Runs a definition with the empty input
module Daedalus.LSP.Command.Run (runModule, watchModule) where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text

import qualified Data.Aeson                  as A
import qualified Language.LSP.Types          as J

import           Daedalus.Interp             (interpFile)
import           Daedalus.PP
import           Daedalus.Type.AST
import qualified RTS.ParserAPI as RTS

import           Daedalus.LSP.Monad
import           Daedalus.LSP.Position
import qualified Data.List.NonEmpty as NE
import Data.Functor (($>))
import Data.Aeson (KeyValue((.=)))
import Data.Function (on)
import Daedalus.AST (nameScopeAsModScope)
import Data.Foldable (find)
import Daedalus.Rec (forgetRecs)
import System.Log.Logger (debugM)
import Data.Either (isRight)

gatherModules :: Map ModuleName ModuleState -> [ModuleName] ->
                 STM (Maybe (Map ModuleName (ModuleSource, TCModule SourceRange)))
gatherModules mods = go mempty
  where
    go acc [] = pure (Just acc)
    go acc (mn : rest) | mn `Map.member` acc = go acc rest
    go acc (mn : rest) = do
      let getmr mr = (,) (mrSource mr) <$> mrTC mr
      m_mod <- join <$> traverse (fmap getmr . readTVar . moduleResults) (Map.lookup mn mods)
      let doRest mr = go (Map.insert (tcModuleName (snd mr)) mr acc)
                         (map thingValue (tcModuleImports (snd mr)) ++ rest)
                      
      join <$> traverse doRest m_mod

canRunDecl :: Name -> Map ModuleName (ModuleSource, TCModule SourceRange) -> Either String ()
canRunDecl nm mods = do
  m <- case Map.lookup mname mods of
         Nothing -> Left $ "module " ++ showPP mname ++ " has not passed type checking"
         Just m  -> Right m

  let err msg = Left $ showPP nm ++ msg
  case getDecl m of
    Just TCDecl { tcDeclTyParams = [], {- tcDeclCtrs = [], -} tcDeclImplicit = []
                , tcDeclParams = [] } -> Right ()
    Just TCDecl { tcDeclTyParams = _ : _} -> err " has type parameters"
    Just TCDecl { tcDeclImplicit = _ : _} -> err " has implicit parameters"
    Just TCDecl { tcDeclParams = _ : _}   -> err " has parameters"
    Nothing -> err " could not be found"
  where
    -- the GUID in names can change, we we need to just check the ident.
    getDecl (_, m) = find (\d -> nameScopedIdent (tcDeclName d) == nameScopedIdent nm) (forgetRecs (tcModuleDecls m))
    (mname, _) = nameScopeAsModScope nm

runModule :: J.Position -> ServerState -> TCModule SourceRange -> IO (Maybe A.Value)
runModule pos sst m = do
  m_ms <- atomically $ do
    mods <- readTVar (knownModules sst)
    gatherModules mods [tcModuleName m] -- this will re-read m
    
  let m_d = declAtPos pos m
  case (,) <$> m_ms <*> m_d of
    -- FIXME: we should probably say something about why we can't run it.
    Just (ms, d) | isRight (canRunDecl (tcDeclName d) ms) -> runIt (map snd (Map.elems ms)) d
    _ -> pure Nothing
    
  where
    runIt ms d = do
      (_, res) <- interpFile Nothing ms (nameScopedIdent (tcDeclName d))
      -- For now we just return the pretty-printed value (we could also return the json)
      let msg = case res of
            RTS.NoResults err -> show (RTS.ppParseError err)
            RTS.Results as    -> showPP (NE.head as) -- FIXME
      pure (Just $ A.String (Text.pack msg))

-- | Watches a module (and deps.) and reruns the given function when
-- something changes.  This could race as we see some modules change
-- before the top level module is updated.  We could maybe then
-- include all the imports in each TC'd modules results.
watchModule :: (A.Value -> IO ()) -> (J.ShowMessageParams -> IO ()) -> A.Value -> ServerState -> Name -> IO ()
watchModule report reportMsg clientHandle sst nm = go mempty
  where
    (rootModuleName, _) = nameScopeAsModScope nm
    go oldTCs = do
      newTCs <- atomically $ do
        mods <- readTVar (knownModules sst)
        m_newTCs <- gatherModules mods [rootModuleName]
        case m_newTCs of
          Nothing -> retry
          Just newTCs ->
            -- check that some source has changed (or we have newly available modules)
            check (Map.isProperSubmapOfBy (\_ _ -> True) oldTCs newTCs
                   || not (Map.isSubmapOfBy ((==) `on` fst) oldTCs newTCs)) $> newTCs

      -- debugDecl newTCs
      
      -- We need to check the start decl takes no arguments (incl. type arguments)
      case canRunDecl nm newTCs of
        Right _ -> do
          let tcs = snd <$> Map.elems newTCs
          (_, res) <- interpFile Nothing tcs (nameScopedIdent nm)
          -- For now we just return the pretty-printed value (we could also return the json)
          let resStr = case res of
                RTS.NoResults err -> show (RTS.ppParseError err)
                RTS.Results as    -> showPP (NE.head as) -- FIXME
              msg = A.object ["clientHandle" .= clientHandle, "result" .= A.String (Text.pack resStr)]
          report msg
        Left err -> reportMsg (J.ShowMessageParams J.MtWarning (Text.pack $ "Declaration " ++ showPP nm ++ " cannot be run: " ++ err))
      go newTCs

    -- debugDecl mods = do
    --   let getDecl (_, m) = find (\d -> tcDeclName d == nm) (forgetRecs (tcModuleDecls m))
    --   case Map.lookup rootModuleName mods >>= getDecl of
    --     Just d -> debugM "reactor.watch" (showPP d)
    --     _      -> debugM "reactor.watch" "No decl"


          

