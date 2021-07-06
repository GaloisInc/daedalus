{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Daedalus.LSP.Command (supportedCommands, executeCommand) where

import qualified Data.Map                    as Map
import Data.Map (Map)
import Control.Lens

import qualified Language.LSP.Types           as J
import qualified Data.Aeson as A
import Data.Aeson (ToJSON, FromJSON, Result(..), toJSON, fromJSON)

import Daedalus.LSP.Monad
import Daedalus.LSP.Position
import qualified Daedalus.LSP.Command.Regions as C
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Types.Lens as J

type CommandImplFun = [A.Value] -> Either String (ServerM (Either J.ResponseError A.Value))

data CommandImpl = forall a. CanInvoke a => CommandImpl a

class CanInvoke a where
  invoke :: a -> CommandImplFun

instance CanInvoke CommandImpl where
  invoke (CommandImpl impl) = invoke impl

instance ToJSON a => CanInvoke (ServerM (Either J.ResponseError a)) where
  invoke m  [] = Right (fmap toJSON <$> m)
  invoke _  _ = Left "Expecting empty args"
  
instance (FromJSON a, CanInvoke b) => CanInvoke (a -> b) where
  invoke f (j : js) =
    case fromJSON j of
      Success v -> invoke (f v) js
      Error msg -> Left ("Error parsing '" ++ show j ++ ": " ++ msg)
  invoke _ _ = Left "Too few arguments"

commands :: Map Text CommandImpl
commands = Map.fromList [ ("positionToRegions", CommandImpl positionToRegions) ]

executeCommand :: (Either J.ResponseError A.Value -> ServerM ()) -> Text -> [A.Value] -> ServerM ()
executeCommand resp nm vs
  | Just impl <- Map.lookup nm commands = do
      case invoke impl vs of
        Left err -> resp $ Left $ J.ResponseError J.InvalidParams (Text.pack err) Nothing
        Right m  -> resp =<< m
  | otherwise = resp (Left $ J.ResponseError J.InternalError ("Unknown command " <> nm) Nothing)

supportedCommands :: [Text]
supportedCommands = Map.keys commands

positionToRegions :: J.TextDocumentIdentifier -> J.Position -> ServerM (Either J.ResponseError (J.List J.Range))
positionToRegions doc pos = do
  e_mr <- uriToModuleResults (J.toNormalizedUri (doc ^. J.uri))
  pure $ do
    mr <- e_mr
    case mrTC mr of
      Nothing -> Left $ J.ResponseError J.ParseError "Missing module" Nothing
      Just m  -> Right $ C.positionToRegions pos m
  
  



                         
