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
import RTS.Vector(vecFromRep,vecToRep,toList) 

import XRef(findStartXRef, parseXRefs)
import PdfMonad
import PdfDecl(pResolveRef)
import PdfXRef(pEncryptionDict,TrailerDict) 
import PdfValue(Value(..),Ref(..))
import Primitives.Decrypt(makeFileKey)

import PdfDOM
import CommandLine
import PP

import Debug.Trace

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
        handlePdfResult (parseXRefs topInput idx) "BUG: Ambiguous XRef table."

     fileEC <- makeEncContextDom trail refs topInput (password opts) 

     let ppRef pref r =
           do res <- runParser refs (fileEC r) (pResolveRef r) topInput
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
          do handlePdfResult (runParser refs Nothing (pPdfTrailer trail) topInput) 
                              "BUG: Ambiguous result" 
             putStrLn "OK"

       ShowHelp -> dumpUsage options

handlePdfResult :: IO (PdfResult a) -> String -> IO a 
handlePdfResult x msg = 
  do  res <- x
      case res of
        ParseOk a     -> pure a
        ParseAmbig {} -> quit msg 
        ParseErr e    -> quit (show (pp e))

-- XXX: Very similar code in pdf-driver/src/driver/Main.hs. Should de-duplicate
makeEncContextDom :: TrailerDict 
                  -> ObjIndex 
                  -> Input  
                  -> BS.ByteString 
                  -> IO (Ref -> Maybe EncContext)  
makeEncContextDom trail refs topInput pwd = 
  case (getField @"encrypt" trail, getField @"id" trail) of 
    (Nothing, _) -> pure $ const Nothing -- No encryption 
    (_, Nothing) -> do hPutStrLn stderr "WARNING: Encryption error - missing document ID field. Decryption disabled."
                       pure $ const Nothing 
    (Just d, Just fileID) -> do 
      enc <- handlePdfResult (runParser refs Nothing (pEncryptionDict d) topInput) 
                              "Ambiguous encryption dictionary"
      if not $ elem (getField @"V" enc) [2,4] then 
        do hPutStrLn stderr "WARNING: Unsupported cipher mode. Decryption disabled" 
           pure $ const Nothing
      else do 
        let len = fromIntegral $ getField @"encLength" enc 
            encO = vecToRep $ getField @"encO" enc 
            encP = fromIntegral $ getField @"encP" enc
            firstid = vecToRep $ getField @"firstid" fileID 
            filekey = makeFileKey len pwd encO encP firstid  
        pure $ \(Ref ro rg) -> 
          Just EncContext { key = filekey, 
                            keylen = len, 
                            robj = fromIntegral ro, 
                            rgen = fromIntegral rg } 

quit :: String -> IO a
quit msg =
  do hPutStrLn stderr msg
     exitFailure

