{-# Language BlockArguments #-}
module Primitives.Resolve where

import qualified Data.ByteString as BS
import qualified Data.Map as Map

import RTS.Numeric(toInt)
import PdfMonad.Transformer

resolveImpl :: PdfParser parser => parser a
  -> (Integer -> Integer -> Integer -> parser a)
  -> Integer -> Integer -> parser (Maybe a)
resolveImpl pTop pCompressed obj gen =
  do oi <- toInt' obj
     gi <- toInt' gen
     let r = R { refObj = oi, refGen = gi }
     resolving r
       do mp <- getObjIndex
          case Map.lookup r mp of
            Nothing -> pure Nothing
            Just l  ->
              case l of
                InFileAt o ->
                  do bs  <- getTopBytes
                     cur <- pPeek
                     pSetInput Input { inputBytes = BS.drop o bs
                                     , inputOffset = o
                                     }
                     v <- pTop
                     pSetInput cur
                     pure (Just v)
                -- FIXME: we shouldn't always return Just x?
                InObj ro idx -> Just <$> pCompressed (fromIntegral (refObj ro))
                                                     (fromIntegral (refGen ro))
                                                     (fromIntegral idx)
  where
  toInt' x = case toInt x of
               Just i -> pure i
               Nothing -> pError FromUser "resolveImpl" "Number is too large"

