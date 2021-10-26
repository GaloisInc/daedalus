{-# LANGUAGE OverloadedStrings #-}

module Daedalus.LSP.Command.Debug (debugCore) where

import qualified Data.Text              as Text
import qualified Data.Aeson             as A
import qualified Language.LSP.Types     as J

import           Control.Monad.IO.Class (liftIO)

import           Daedalus.Type.AST
import qualified Daedalus.Driver        as D
import           Daedalus.LSP.Monad
import           Daedalus.LSP.Position  (declAtPos)
import           Daedalus.PP            (showPP)


debugCore :: J.Position -> TCModule SourceRange -> ServerM (Maybe A.Value)
debugCore pos m = do
  let m_d = declAtPos pos m
  
  let entryName = maybe "Main" (nameToIdent . nameScopedIdent . tcDeclName) m_d
      specMod  = "DaedalusMain"

  e_md <- liftDaedalus $ do
    D.passSpecialize specMod [(tcModuleName m, entryName)]
    D.passCore specMod
    D.ddlGetAST specMod D.astCore

  msg <- case e_md of
        Left err -> liftIO $ D.prettyDaedalusError err
        Right md -> pure (showPP md)

  pure (Just $ A.String (Text.pack msg))
  where
    -- FIXME: move
    nameToIdent x = case x of
      Unknown  n -> n
      Local    n -> n
      ModScope _m n -> n
    
    -- runIt ms d = do
    --   (_, res) <- interpFile Nothing ms (nameScopedIdent (tcDeclName d))
    --   -- For now we just return the pretty-printed value (we could also return the json)
    --   let msg = case res of
    --         RTS.NoResults err -> show (RTS.ppParseError err)
    --         RTS.Results as    -> showPP (NE.head as) -- FIXME
    --   pure (Just $ A.String (Text.pack msg))

