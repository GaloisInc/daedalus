{-# Language OverloadedStrings, TypeApplications, DataKinds, BlockArguments #-}
import qualified Data.ByteString          as BS
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Map as Map
import GHC.Records(getField) 
import System.IO(hPutStrLn,stderr)
import System.Exit(exitFailure)
import Text.PrettyPrint
import SimpleGetOpt

import RTS.Input(newInput)
import RTS.Vector(vecFromRep) 

import XRef(findStartXRef, parseXRefs)
import PdfMonad
import PdfDecl(pResolveRef)
import PdfXRef(pEncryptionDict) 
import PdfValue(Value(..),Ref(..))
import Primitives.Decrypt(makeFileKey)

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
           ParseErr e   -> quit (show (pp e))

     let run p =
           do res <- runParser refs Nothing p topInput
              case res of
                ParseOk a     -> pure a
                ParseAmbig {} -> quit "BUG: Ambiguous result"
                ParseErr e    -> quit (show (pp e))

         ppRef pref r =
           do res <- runParser refs Nothing (pResolveRef r) topInput
              case res of
                ParseOk a ->
                  case a of
                    Just d  -> print (pref <+> pp d)
                    Nothing -> print (pref <+> pp (Value_null ()))
                ParseErr e    -> print (pref <+> pp e)
                ParseAmbig {} -> quit "BUG: Ambiguous result"

         rToRef (R x y) = Ref (fromIntegral x) (fromIntegral y)

     case command opts of
       ListXRefs -> print $ ppBlock "[" "]" (map ppXRef (Map.toList refs))

       PrettyPrintAll ->
         case map rToRef (Map.keys refs) of
           [] -> putStrLn "[]"
           x : xs ->
             do ppRef "[" x
                mapM_ (ppRef ",") xs
                putStrLn "]"

       PrettyPrint
         | object opts < 0 -> print (pp trail)
         | otherwise -> ppRef "" (Ref (object opts) (generation opts))

       Validate ->
          do run (pPdfTrailer trail)
             putStrLn "OK"

       ShowHelp -> dumpUsage options

       ShowEncrypt -> 
         case getField @"encrypt" trail of 
           Nothing -> putStrLn "No encryption"
           Just d  -> do 
             enc <- run (pEncryptionDict d) 
             print (getField @"encLength" enc)
             print (getField @"encO" enc) 
             print (getField @"encP" enc) 
             print (getField @"id" trail)

quit :: String -> IO a
quit msg =
  do hPutStrLn stderr msg
     exitFailure

