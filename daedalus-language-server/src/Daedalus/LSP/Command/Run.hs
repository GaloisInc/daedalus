{-# LANGUAGE OverloadedStrings #-}

-- | Runs a definition with the empty input
module Daedalus.LSP.Command.Run (runModule, watchModule) where

import           Control.Concurrent.STM.TVar
import           Control.Lens hiding ((.=))
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Aeson                  (KeyValue ((.=)))
import qualified Data.Aeson                  as A
import           Data.Either                 (isRight)
import           Data.Foldable               (find)
import           Data.Function               (on)
import           Data.Functor                (($>))
import qualified Data.List.NonEmpty          as NE
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text

import qualified Language.LSP.Types          as J

import           System.Log.Logger           (debugM)

import           Daedalus.AST                (nameScopeAsModScope)
import           Daedalus.Interp             (interpFile)
import           Daedalus.PP
import           Daedalus.Rec                (forgetRecs, topoOrder)
import           Daedalus.Type.AST
import qualified RTS.ParserAPI               as RTS

import           Daedalus.LSP.Monad
import           Daedalus.LSP.Position
import qualified Data.Set as Set


gatherModules :: Map ModuleName ModuleState ->
                 Map ModuleName (ModuleSource, TCModule SourceRange)
gatherModules = Map.mapMaybe go
  where
    go mst = (,) (mst ^. msSource) . (\(tc, _, _) -> tc) <$> passStatusToMaybe (mst ^. msTCRes)

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
  ms <- gatherModules <$> atomically (readTVar (moduleStates sst))
  let m_d = declAtPos pos m
  case m_d of
    -- FIXME: we should probably say something about why we can't run it.
    Just d | isRight (canRunDecl (tcDeclName d) ms) -> runIt (map snd (Map.elems ms)) d
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
        newTCs <- gatherModules <$> readTVar (moduleStates sst)
        check (rootModuleName `Map.member` newTCs)        
        check (Map.isProperSubmapOfBy (\_ _ -> True) oldTCs newTCs
                || not (Map.isSubmapOfBy ((==) `on` fst) oldTCs newTCs))
        pure newTCs
  
            -- We need to check the start decl takes no arguments (incl. type arguments)
      case canRunDecl nm newTCs of
        Right _ -> do
          let unsortedTcs = snd <$> Map.elems newTCs
              tcs = forgetRecs $ topoOrder (\m -> (tcModuleName m, Set.fromList (map thingValue (tcModuleImports m)))) unsortedTcs
          
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
    --   -- let getDecl (_, m) = find (\d -> tcDeclName d == nm) (forgetRecs (tcModuleDecls m))
    --   debugM "reactor.watch" (show $ vcat [ pp mn $$ nest 4 (pp m) | (mn, (_, m)) <- Map.toList mods ])
    --   -- case Map.lookup rootModuleName mods >>= getDecl of
    --   --   Just d -> debugM "reactor.watch" (show 
    --   --   _      -> debugM "reactor.watch" "No decl"


          

