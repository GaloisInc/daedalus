
-- | Runs a definition with the empty input
module Daedalus.LSP.Command.Run (runModule) where

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

gatherModules :: Map ModuleName ModuleState -> Map ModuleName (TCModule SourceRange) -> [ModuleName] ->
                   STM (Maybe [TCModule SourceRange])
gatherModules _ acc [] = pure (Just (Map.elems acc))
gatherModules mods acc (mn : rest) | mn `Map.member` acc = gatherModules mods acc rest
gatherModules mods acc (mn : rest) = do
  m_mod <- join <$> traverse (fmap  mrTC . readTVar . moduleResults) (Map.lookup mn mods)
  let doRest m = gatherModules mods (Map.insert (tcModuleName m) m acc)
                                    (map thingValue (tcModuleImports m) ++ rest)
  join <$> traverse doRest m_mod

runModule :: J.Position -> ServerState -> TCModule SourceRange -> IO (Maybe A.Value)
runModule pos sst m = do
  m_ms <- atomically $ do
    mods <- readTVar (knownModules sst)
    gatherModules mods (Map.singleton (tcModuleName m) m) (map thingValue (tcModuleImports m))
    
  let m_d = declAtPos pos m
  case (,) <$> m_ms <*> m_d of
    Nothing -> pure Nothing
    Just (ms, d) -> runIt ms d
    
  where
    runIt ms d = do
      (_, res) <- interpFile Nothing ms (nameScopedIdent (tcDeclName d))
      -- For now we just return the pretty-printed value (we could also return the json)
      let msg = case res of
            RTS.NoResults err -> show (RTS.ppParseError err)
            RTS.Results as    -> showPP (NE.head as) -- FIXME
      pure (Just $ A.String (Text.pack msg))
