{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Daedalus.LSP.Command.Debug (debugPass, passNames) where

import qualified Data.Text              as Text
import qualified Data.Aeson             as A
import qualified Language.LSP.Types     as J

import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (find)
import           Data.Text              (Text)

import           Daedalus.Driver
import           Daedalus.LSP.Monad
import           Daedalus.LSP.Position  (declAtPos)
import           Daedalus.PP            (Doc, pp)
import           Daedalus.Type.AST

data DDLPass = DDLPass
  { passName :: Text
  , passRun  :: TCModule SourceRange -> Ident -> ModuleName -> Daedalus Doc
  }

-- We could be clever and order these to reduce duplication, but this
-- is simpler
passes :: [DDLPass]
passes = [ DDLPass { passName = "tc"
                   , passRun = \md _ _ -> pure (pp md)
                   }
         , DDLPass { passName = "core"
                   , passRun = \m entryName specMod -> do
                       passSpecialize specMod [(tcModuleName m, entryName)]
                       passCore specMod
                       pp <$> ddlGetAST specMod astCore
                   }                       
         ]

passNames :: [Text]
passNames = map passName passes

debugPass :: J.Position -> TCModule SourceRange -> Text -> ServerM (Maybe A.Value)
debugPass pos m passN = do
  let m_d = declAtPos pos m
  
  let entryName = maybe "Main" (nameToIdent . nameScopedIdent . tcDeclName) m_d
      specMod  = "DaedalusMain"

  case find (\pass -> passName pass == passN) passes of
    Nothing -> pure Nothing
    Just pass -> do
      e_r <- liftDaedalus (passRun pass m entryName specMod)
      msg <- case e_r of
        Left err -> liftIO $ prettyDaedalusError err
        Right r -> pure (show r)

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

