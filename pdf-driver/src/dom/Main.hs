{-# Language OverloadedStrings, BlockArguments #-}
import qualified Data.ByteString          as BS
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Map as Map
import System.IO(hPutStrLn,stderr)
import System.Exit(exitFailure)
import SimpleGetOpt

import RTS.Input(newInput)
import XRef(findStartXRef, parseXRefs)
import PdfMonad
import PdfDecl(pResolveRef)
import PdfValue(Value(..),Ref(..))

import PdfDOM
import CommandLine
import PP


main :: IO ()
main =
  do opts <- getOpts options
     file <- case files opts of
               [] -> reportUsageError options ["No file to process."]
               [f] -> pure f
               _ -> reportUsageError options
                            ["Multiple files not yet supported."]

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

     let run p =
           do res <- runParser refs p topInput
              case res of
                ParseOk a     -> pure a
                ParseAmbig {} -> quit "BUG: Ambiguous result"
                ParseErr e    -> quit (show e)

     case command opts of
       ListXRefs -> print $ ppBlock "[" "]" (map ppXRef (Map.toList refs))

       PrettyPrint
         | object opts < 0 -> print (pp trail)
         | otherwise ->
           do mb <- run (pResolveRef (Ref (object opts) (generation opts)))
              case mb of
                Nothing -> print (pp (Value_null ()))
                Just d -> print (pp d)

       Validate ->
          do run (pPdfTrailer trail)
             putStrLn "OK"

       ShowHelp -> dumpUsage options


quit :: String -> IO a
quit msg =
  do hPutStrLn stderr msg
     exitFailure

