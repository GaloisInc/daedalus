{-# Language OverloadedStrings, TypeApplications, DataKinds, BlockArguments #-}
import           Data.ByteString(ByteString)
import qualified Data.ByteString          as BS
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Map as Map
import Control.Monad(when)
import GHC.Records(getField) 
import System.IO(hPutStrLn,stderr)
import System.Exit(exitFailure)
import Text.PrettyPrint
import SimpleGetOpt

import RTS.Input(newInput)
import RTS.Vector(vecFromRep,vecToRep,toList) 

import XRef(findStartXRef
           ,parseXRefs1
           ,parseXRefs2
           ,printIncUpdateReport
           ,printObjIndex
           ,validateUpdates
           ,printCavityReport)
import PdfMonad
import Primitives.Decrypt(makeFileKey)

-- daedalus generated parsers:
import CMap(pToUnicodeCMap_simpleFont, pToUnicodeCMap_cidFont)
import XRef(findStartXRef, parseXRefs1, parseXRefs2)
import PdfDecl(pResolveRef)
import PdfXRef(TrailerDict) 
import PdfCrypto(pEncryptionDict,ChooseCiph(..),pMakeContext,MakeContext(..))
import PdfValue(Value(..),Ref(..),pValue)

import PdfDOM
import PdfPP
import CommandLine

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
       ParseType "Value" ->
          do res <- runParser Map.empty Nothing pValue topInput
             case res of
                ParseOk a     -> print (pp a)
                ParseErr e    -> print (pp e)
                ParseAmbig {} -> quit "BUG: Ambiguous result"
       ParseType "ToUnicodeCMap_simpleFont" ->
          do res <- runParser Map.empty Nothing pToUnicodeCMap_simpleFont topInput
             case res of
                ParseOk a     -> print a
                ParseErr e    -> print (pp e)
                ParseAmbig {} -> quit "BUG: Ambiguous result"
       ParseType "ToUnicodeCMap_cidFont" ->
          do res <- runParser Map.empty Nothing pToUnicodeCMap_cidFont topInput
             case res of
                ParseOk a     -> print a
                ParseErr e    -> print (pp e)
                ParseAmbig {} -> quit "BUG: Ambiguous result"
       ParseType s -> quit $ unwords ["'parse-type' does not recognize type", show s]

       _ -> parsePdf opts file bs topInput


parsePdf :: Settings -> FilePath -> ByteString -> Input -> IO ()
parsePdf opts file bs topInput =
  do idx <- case findStartXRef bs of
              Left err  -> quit err
              Right idx -> pure idx

     
     (refs, trail) <-
        handlePdfResult (parseXRefs1 topInput idx) "BUG: Ambiguous XRef table."
     (incUpdates, refs', trail') <-
        handlePdfResult (parseXRefs2 topInput idx) "BUG: Ambiguous XRef table (2)."
     validateUpdates (incUpdates, refs', trail')
     
     when (trail /= trail') $
        putStrLn "warn: trail-v1 /= trail-v2"
     when (refs /= refs') $
        putStrLn "warn: refs-v1 /= refs-v2"

     -- FIXME:
     --  - when more sure of the above, just remove the parseXRefs1 code.
     
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
       ListXRefs      -> printObjIndex 0 refs
       
       ListIncUpdates -> do
                         printIncUpdateReport incUpdates
                         putStrLn "Combined xref table:"
                         printObjIndex 2 refs

       ListCavities   -> printCavityReport topInput incUpdates

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
  do edict <- handlePdfResult (runParser refs Nothing (pMakeContext trail) topInput) 
                              "Ambiguous encryption dictionary"
     case edict of 
       MakeContext_noencryption _ -> pure $ const Nothing 
       MakeContext_encryption enc -> do 
        let encO = vecToRep $ getField @"encO" enc 
            encP = fromIntegral $ getField @"encP" enc
            id0 = vecToRep $ getField @"id0" enc 
            filekey = makeFileKey pwd encO encP id0
        pure $ \(ro, rg) -> 
          Just EncContext { key  = filekey, 
                            robj = fromIntegral ro, 
                            rgen = fromIntegral rg, 
                            ciph = chooseCipher $ getField @"ciph" enc  } 

chooseCipher :: ChooseCiph -> Cipher 
chooseCipher enc = 
  case enc of 
    ChooseCiph_v2RC4 _ -> V2RC4
    ChooseCiph_v4RC4 _ -> V4RC4
    ChooseCiph_v4AES _ -> V4AES
