{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- More or less the features discussed as 'Language features' in the LSP spec.
module Daedalus.LSP.LanguageFeatures where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Reader
import           Data.Foldable
import qualified Data.HashMap.Strict         as HMap
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe, maybeToList)
import           Data.Monoid
import qualified Data.Text                   as Text

import           Language.LSP.Server         (getClientCapabilities)
import qualified Language.LSP.Types          as J
import qualified Language.LSP.Types.Lens     as J

import           Daedalus.PP
import           Daedalus.Rec                (forgetRecs)
import           Daedalus.Scope
import           Daedalus.SourceRange
import           Daedalus.Type.AST

import           Daedalus.LSP.Diagnostics    (sourceRangeToRange)
import           Daedalus.LSP.Monad
import           Daedalus.LSP.Position
import qualified Daedalus.LSP.SemanticTokens as SI

-- -----------------------------------------------------------------------------
-- Helpers

uriToModuleResults :: J.NormalizedUri -> ServerM (Either J.ResponseError ModuleResults)
uriToModuleResults uri = do
  sst <-  ask
  let Just mn = uriToModuleName uri -- FIXME
  liftIO $ atomically $ do
    mods <- readTVar (knownModules sst)
    case Map.lookup mn mods of
      Nothing -> pure $ Left $ J.ResponseError J.InvalidParams "Missing module" Nothing
      Just mi -> Right <$> readTVar (moduleResults mi)

-- -----------------------------------------------------------------------------
-- Semantic tokens (highlighting etc.)

semanticTokens :: (Either J.ResponseError (Maybe J.SemanticTokens) -> ServerM ()) -> 
                  Maybe J.Range -> J.NormalizedUri -> ServerM ()
semanticTokens resp m_range uri = do
  e_mr <- uriToModuleResults uri

  caps <- getClientCapabilities
  let m_semcaps = caps ^. J.textDocument >>= view J.semanticTokens

  -- case e_mr of
  --   Right mr | Just _tks <- mrTokens mr ->
  --              liftIO $ debugM "reactor.semanticTokens" $ "Got some tokens" ++ show (SI.semanticTokens m_range m_semcaps mr)
  --   Right _ -> liftIO $ debugM "reactor.semanticTokens" $ "No tokens"
  --   Left  _ -> liftIO $ debugM "reactor.semanticTokens" $ "No results"

  -- case m_semcaps of
  --   Nothing   -> liftIO $ debugM "reactor.semanticTokens" $ "No caps"
  --   Just caps -> liftIO $ debugM "reactor.semanticTokens" $ "Caps" ++ show (caps ^. J.tokenTypes)
    
  -- We might want to cache these?
  resp $ fmap (SI.semanticTokens m_range m_semcaps) e_mr

-- -----------------------------------------------------------------------------
-- Definition links


definition :: (Either J.ResponseError (J.Location J.|? (J.List J.Location J.|? J.List J.LocationLink)) -> ServerM ()) ->
              J.NormalizedUri -> J.Position -> ServerM ()
definition resp uri pos = do
  e_mr <- uriToModuleResults uri
  resp $ fmap (J.InR . J.InL . doDefinition uri pos) e_mr

-- This currently returns a location, but we could return a
-- locationlink --- we need the range of the target defn.
doDefinition :: J.NormalizedUri -> J.Position -> ModuleResults -> J.List J.Location
doDefinition _uri pos mr = J.List . maybeToList $ do
  m     <- mrTC mr
  gscope <- mrImportScope mr
  
  d <- declAtPos pos m
  let allnis = declToNames d
  ni <- find (positionInRange pos) allnis
  let isNameDef ni' = niName ni' == niName ni && niNameRefClass ni' == NameDef
  targetRange <-
    case nameScopedIdent (niName ni) of
      Unknown _ -> Nothing -- shouldn't happen
      Local _ -> range <$> find isNameDef allnis
      ModScope mn i -> do
        scope <- Map.lookup mn gscope
        range . snd <$> Map.lookup i (identScope scope)

  pure (sourceRangeToLocation targetRange)
  
-- -----------------------------------------------------------------------------
-- Renaming (locals only right now)

-- FIXME: we could do this on just the scoped module, we don't need TC (also for highlight)

-- | Renames a symbol, currently only in a single module.  It will
-- return Nothing if the symbol is defined in another module.  This
-- doesn't guarantee that the edits will result in a well formed module.

rename :: (Either J.ResponseError J.WorkspaceEdit -> ServerM ()) -> J.NormalizedUri ->
          J.Position -> Text.Text -> ServerM ()
rename resp uri pos newName = do
  e_tc <- fmap mrTC <$> uriToModuleResults uri
  resp $ fmap (fromMaybe mempty . (doRename pos uri newName =<<)) e_tc

doRename :: J.Position -> J.NormalizedUri -> Text.Text -> TCModule SourceRange -> Maybe J.WorkspaceEdit
doRename pos uri newName m = do
  d <- declAtPos pos m
  let allnis = declToNames d
  ni <- find (positionInRange pos) allnis
  let allnis' =
        if isLocalName (niName ni)
        then allnis
        -- This is pretty gross
        else concatMap declToNames (forgetRecs (tcModuleDecls m))
      nis = filter ((==) (niName ni) . niName) allnis'
  -- if we don't have a definition in this file, we do nothing
  guard (any ((==) NameDef . niNameRefClass) allnis')
  -- Otherwise replace everything.
  --
  -- FIXME: We use 'changes' instead of 'documentChanges' until we
  -- export the version etc. from the worker.
  let edits = J.List [ J.TextEdit (sourceRangeToRange (range ni')) newName | ni' <- nis ]
      emap = HMap.singleton (J.fromNormalizedUri uri) edits
  pure (set J.changes (Just emap) mempty)

-- -----------------------------------------------------------------------------
-- Highlighting
--
-- This returns a list of places that a symbol is referenced.

highlight :: (Either J.ResponseError (J.List J.DocumentHighlight) -> ServerM ()) -> J.NormalizedUri -> J.Position ->
             ServerM ()
highlight resp uri pos = do
  e_tc <- fmap mrTC <$> uriToModuleResults uri
  resp $ fmap (maybe mempty (doHighlight pos)) e_tc

doHighlight :: J.Position -> TCModule SourceRange -> J.List J.DocumentHighlight
doHighlight pos m = fromMaybe mempty $ do
  d <- declAtPos pos m
  let allnis = declToNames d
  ni <- find (positionInRange pos) allnis
  let allnis' =
        if isLocalName (niName ni)
        then allnis
        -- This is pretty gross
        else concatMap declToNames (forgetRecs (tcModuleDecls m))
      nis = filter ((==) (niName ni) . niName) allnis'
  pure $ J.List (map niToHighlight nis)
  where
    niToHighlight ni = J.DocumentHighlight (sourceRangeToRange (range ni))
                       (Just $ case niNameRefClass ni of { NameDef -> J.HkWrite ; NameUse -> J.HkRead })

-- FIXME: what about version ?
-- FIXME: check uri against module's URI
hover :: (Either J.ResponseError (Maybe J.Hover) -> ServerM ()) -> J.NormalizedUri -> J.Position -> ServerM ()
hover resp uri pos = do
  e_tc <- fmap mrTC <$> uriToModuleResults uri

  -- case e_tc of
  --   Right (Just m) -> do
  --     let Just d = declAtPos pos m
  --     liftIO $ debugM "reactor.hover" (show pos ++ "\n" ++ show (exprTree d))
      
  --   _ -> liftIO $ debugM "reactor.hover" "Didn't find a tc'd module"

  -- case e_tc of
  --   Right Nothing -> liftIO $ debugM "reactor.hover" "Module hasn't been type checked"
  --   Right (Just m) -> do
  --     liftIO $ debugM "reactor.hover" "Found a module"
  --     void $ traverse (liftIO . debugM "reactor.hover" . (show . bullets . map (viewSome pp)))
  --       $ typeAtModule pos m
        
  --   _ -> liftIO $ debugM "reactor.hover" "Didn't find a tc'd module"

  -- resp (Right Nothing)
  resp $ fmap (doHover pos =<<) e_tc

doHover :: J.Position -> TCModule SourceRange -> Maybe J.Hover
doHover pos m = do
  (ty, r) <- getAlt $ typeAtModule pos m
  let ms = J.HoverContents $ J.markedUpContent "lsp-daedalus" (Text.pack (showPP ty))
  pure $ J.Hover ms (Just (sourceRangeToRange r))

