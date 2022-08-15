
module Main where

import System.Environment(getArgs)
import Data.Word(Word8)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Control.Monad(foldM)
import Control.Exception(Exception(..), catch)
import System.FilePath((</>), dropExtension, takeExtension)
import System.Directory(getDirectoryContents,doesFileExist)

import qualified Daedalus.RTS as RTS

import PP
import Parser
-- import LinkUses
import Graph



main :: IO ()
main =
  do dir : _ <- getArgs
     spec <- loadPDFSpec dir
     -- let us = linkUses spec
     -- dumpDOT us
     dumpSpec spec
  `catch` \(ParseErrorException err) -> print (RTS.ppParseError err)

dumpSpec :: PDFSpec -> IO ()
dumpSpec = print . pp

{-
dumpShapes :: LinkUses -> IO()
dumpShapes us = mapM_ (\(x,y) -> putStrLn (Text.unpack x ++ ": " ++ show y)) tys
  where tys = Map.toList (Map.keys <$> us)

dumpDOT :: LinkUses -> IO ()
dumpDOT us = putStrLn (dotGraph sc)
  where
  ug = usesToGraph us
  sc = sccG ug
-}

printField :: Show a => Field a -> IO ()
printField = print

byte :: Char -> Word8
byte = fromIntegral . fromEnum

loadPDFSpec :: FilePath -> IO PDFSpec
loadPDFSpec dir =
  do fs <- getDirectoryContents dir
     PDFSpec <$> foldM addType Map.empty fs
  where
  addType mp f =
    do let path = dir </> f
       yes <- doesFileExist path
       if yes && takeExtension f == ".tsv"
          then do ct <- loadType path
                  let name = Text.pack (dropExtension f)
                  pure $! Map.insert name ct mp
          else pure mp

loadType :: FilePath -> IO CompositeType
loadType f =
  do bs    <- BS.readFile f
     case parseType (BS8.pack f) bs of
       Right ct -> pure ct
       Left err ->
         do print (RTS.ppParseError err)
            pure (DictionaryType Vector.empty)

newtype ParseErrorException = ParseErrorException RTS.ParseError
  deriving Show

instance Exception ParseErrorException
-------------------------------------------------------------------------------
