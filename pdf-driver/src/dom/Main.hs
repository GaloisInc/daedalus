{-# Language BlockArguments #-}
import qualified Data.ByteString          as BS
import           Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import System.IO(hPutStrLn,stderr)
import System.Exit(exitFailure)
import System.Environment(getArgs)

import RTS.Input(newInput)
import XRef(findStartXRef, parseXRefs)
import PdfMonad


main :: IO ()
main =
  do [file] <- getArgs
     bs <- BS.readFile file
     let topInput = newInput (Text.encodeUtf8 (Text.pack file)) bs
     idx <- case findStartXRef bs of
              Left err  -> quit err
              Right idx -> pure idx

     (refs, trail) <-
       parseXRefs topInput idx >>= \res ->
         case res of
           ParseOk a    -> pure a
           ParseAmbig _ -> error "BUG: Ambiguous XRef table."
           ParseErr e   -> quit (show e)

     return ()

{-
     let trailmap = getField @"all" trail
     xrefOK fmt refs (Map.mapKeys vecToString trailmap)
     when (Map.member (vecFromRep "Encrypt") trailmap) (warnEncrypt fmt)

     root <- case getField @"root" trail of
               Nothing -> rootMissing fmt >> exitFailure
               Just r -> pure r
     rootFound fmt root

     res <- runParser refs (pCatalogIsOK root) topInput
     case res of
       ParseOk ok    -> catalogOK fmt ok
       ParseAmbig _  -> error "BUG: Validation of the catalog is ambiguous?"
       ParseErr e    -> catalogParseError fmt e

     mapM_ (checkDecl fmt topInput refs) (Map.toList refs)
-}


quit :: String -> IO a
quit msg =
  do hPutStrLn stderr msg
     exitFailure

