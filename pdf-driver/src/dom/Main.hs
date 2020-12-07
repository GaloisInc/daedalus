{-# Language OverloadedStrings, TypeApplications, DataKinds, BlockArguments #-}
import           Data.ByteString(ByteString)
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
import PdfXRef(pEncryptionDict,TrailerDict,ChooseCiph(..),ChooseCiphV4(..)) 
import PdfValue(Value(..),Ref(..),pValue)
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
     case command opts of
       ParseValue ->
          do res <- runParser Map.empty Nothing pValue topInput
             case res of
                ParseOk a     -> print (pp a)
                ParseErr e    -> print (pp e)
                ParseAmbig {} -> quit "BUG: Ambiguous result"

       _ -> parsePdf opts file bs topInput


parsePdf :: Settings -> FilePath -> ByteString -> Input -> IO ()
parsePdf opts file bs topInput =
  do idx <- case findStartXRef bs of
              Left err  -> quit err
              Right idx -> pure idx

     (refs, trail) <- 
        handlePdfResult (parseXRefs topInput idx) "BUG: Ambiguous XRef table."

     fileEC <- makeEncContext trail refs topInput (password opts) 

     let ppRef pref r@(Ref ro rg) =
           do res <- runParser refs (fileEC (ro, rg)) (pResolveRef r) topInput
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

quit :: String -> IO a
quit msg =
  do hPutStrLn stderr msg
     exitFailure

handlePdfResult :: IO (PdfResult a) -> String -> IO a 
handlePdfResult x msg = 
  do  res <- x
      case res of
        ParseOk a     -> pure a
        ParseAmbig {} -> quit msg 
        ParseErr e    -> quit (show (pp e))

-- XXX: Identical code in pdf-driver/src/driver/Main.hs. Should de-duplicate
makeEncContext :: Integral a => 
                      TrailerDict  
                  -> ObjIndex 
                  -> Input 
                  -> BS.ByteString 
                  -> IO ((a, a) -> Maybe EncContext)
makeEncContext trail refs topInput pwd = 
  case getField @"encrypt" trail of 
    Nothing -> pure $ const Nothing -- No encryption 
    Just e -> do 
      let eref = getField @"eref" e 
      enc <- handlePdfResult (runParser refs Nothing (pEncryptionDict eref) topInput) 
                              "Ambiguous encryption dictionary"
      let encO = vecToRep $ getField @"encO" enc 
          encP = fromIntegral $ getField @"encP" enc
          id0 = vecToRep $ getField @"id0" e 
          filekey = makeFileKey pwd encO encP id0
      pure $ \(ro, rg) -> 
        Just EncContext { key  = filekey, 
                          robj = fromIntegral ro, 
                          rgen = fromIntegral rg, 
                          ver  = fromIntegral $ getField @"encV" enc, 
                          ciph = chooseCipher $ getField @"ciph" enc  } 

chooseCipher :: ChooseCiph -> Cipher 
chooseCipher enc = 
  case enc of 
    ChooseCiph_v2 _ -> V2 
    ChooseCiph_v4 i -> 
      case i of 
        ChooseCiphV4_v4AES () -> V4AES 
        ChooseCiphV4_v4RC4 () -> V4RC4
