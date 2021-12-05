{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings #-}

module Main(main) where

-- base pkgs:
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import System.Environment
import System.Exit
import System.FilePath

-- aeson pkg:
import Data.Aeson

{-

This program is used to convert Kudu's "ground truth" file into these files:
  test_validatePDF_2020-03-eval/expctd/*.result-expctd

The full process is to run the following shell commands:

  # for march eval
  SAFEDOCS=~/src/sado
  cd ~/src/daedalus/pdf-driver/test
  jq -c ".[] | {testfile,status} " $SAFEDOCS/pdf-etl/decisions+data/EvalOneGroundTruth.json \
    > T-groundtruth.jsonlines
  cat T-groundtruth.jsonlines |
    runghc src/CreateExpected.hs test_validatePDF_2020-03-eval/expctd


  # for govdocs-subsets
  cd ~/src/daedalus/pdf-driver/test
  jq -c ".[] | {testfile,status} " ~/do/w/sado/govdocsGroundTruth.json |
    > T-govdocsgroundtruth.jsonlines
  egrep "000009|0000[12].|007526|099692|736151|974733" T-govdocsgroundtruth.jsonlines
    # and all give 'valid'

-}

data Status = Status { testfile :: String
                     , status   :: String
                     }
              deriving (Generic,Read,Show)

instance FromJSON Status where {}

quit s = putStrLn s >> exitFailure

main :: IO ()
main =
  do
  as <- getArgs
  d <- case as of
         [d] -> return d
         _   -> quit "Usage: CreateExpected.hs directory"
  s <- BS.getContents
  let ls = filter (not . BS.null) $ BS.split (BS.head "\n") s
           -- split not exactly same as unlines, might as well ignore
           -- blank lines
  rs <- mapM decode'' ls
  forM_ rs $
    \(Status f st) ->
        do
        x <- case st of
               "valid"    -> return "Good"
               "rejected" -> return "Bad"
               "nbcur"    -> return "NCBUR" -- sic
               _          -> quit $ "unexpected status: " ++ st
        writeFile (d </> f <.> "result-expctd") (x++"\n")
        
decode'' :: BS.ByteString -> IO Status
decode'' bs = case decode bs of
                Nothing -> quit $ "can't parse json: " ++ show bs
                Just x  -> return x
                     
