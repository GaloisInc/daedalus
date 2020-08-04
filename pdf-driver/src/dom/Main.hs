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

import PdfDOM


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

     res <- runParser refs (pDOMTrailer trail) topInput
     case res of
       ParseOk _     -> putStrLn "OK"
       ParseAmbig _  -> error "BUG: Validation of the catalog is ambiguous?"
       ParseErr e    -> quit (show e)

quit :: String -> IO a
quit msg =
  do hPutStrLn stderr msg
     exitFailure

