{-# Language RecordWildCards, OverloadedStrings #-}
module Main where

import System.Environment(getArgs)
import Data.Word(Word8)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Vector(Vector)
import qualified Data.Vector as Vector
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(foldM)
import Control.Exception(Exception(..), catch, throwIO)
import System.FilePath((</>), dropExtension)
import System.Directory(getDirectoryContents,doesFileExist)
import Text.PrettyPrint

import qualified Daedalus.RTS as RTS
import Parser



main :: IO ()
main =
  do dir : _ <- getArgs
     ts      <- loadPDFSpec dir
     print (pp ts)
  `catch` \(ParseErrorException err) -> print (RTS.ppParseError err)

printField :: Field -> IO ()
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
       if yes
          then do ct <- loadType path
                  let name = Text.pack (dropExtension f)
                  pure $! Map.insert name ct mp
          else pure mp

loadType :: FilePath -> IO CompositeType
loadType f =
  do bs    <- BS.readFile f
     case parseType (BS8.pack f) bs of
       Right es -> pure (CompositeType es)
       Left err ->
         do print (RTS.ppParseError err)
            pure (CompositeType Vector.empty)

newtype ParseErrorException = ParseErrorException RTS.ParseError
  deriving Show

instance Exception ParseErrorException
--------------------------------------------------------------------------------

newtype PDFSpec       = PDFSpec (Map Text CompositeType)

newtype CompositeType = CompositeType (Vector Field)
  deriving Show


class PP a where
  pp :: a -> Doc

instance PP Text where
  pp = text . Text.unpack

instance PP PDFSpec where
  pp (PDFSpec mp) = vcat (map ppOne (Map.toList mp))
    where ppOne (x,y) = hcat [pp x, ":"] $$ nest 2 (pp y) $$ " "

instance PP CompositeType where
  pp (CompositeType vs) = vcat (map pp (Vector.toList vs))

instance PP Field where
  pp = text . show

