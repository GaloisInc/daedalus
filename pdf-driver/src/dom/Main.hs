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

import RTS.Input(newInput,inputBytes)
import RTS.Vector(vecFromRep,vecToRep,toList) 
import RTS.Numeric

import XRef( findStartXRef
           , parseXRefsVersion1
           , parseXRefsVersion2
           , printIncUpdateReport
           , printObjIndex
           , validateUpdates
           , printCavityReport
           , FileOffset
           )
import PdfMonad
import Primitives.Decrypt(makeFileKey)

-- daedalus generated parsers:
import CMap(pToUnicodeCMap_simpleFont, pToUnicodeCMap_cidFont)
import PdfDecl(pResolveRef)
import PdfXRef(TrailerDict) 
import PdfCrypto(ChooseCiph(..),pMakeContext,MakeContext(..))
import PdfValue(Value(..),Ref(..),pValue,pHeader)

import PdfDOM
import PdfPP
import CommandLine

main :: IO ()
main =
  do opts <- getOpts options
     file <- case files opts of
               []  -> reportUsageError options ["No file to process."]
               [f] -> pure f
               _   -> reportUsageError options
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

     putStrLn "parseXRefsVersion2:"
     pResV2 <- parseXRefsVersion2 topInput idx 
     warnIfParseError "computing XRef table (v2)" pResV2
     
     putStrLn "parseXRefsVersion1:"
     pResV1 <- parseXRefsVersion1 topInput idx 
     warnIfParseError "computing XRef table (v1)" pResV1
     
     case (pResV1,pResV2) of
       (ParseOk _    , ParseOk _    ) -> return ()
       (ParseAmbig {}, ParseAmbig {}) -> putStrLn "WARN: xref V1 and V2: both ambiguous"
       (ParseErr _   , ParseErr _   ) -> return ()
       _                              -> putStrLn "WARN: xref V1 and V2 do not conform"
       
     (incUpdates, refs', trail') <- quitIfParseError "computing XRef table (v2)" pResV2
     (refs, trail)               <- quitIfParseError "computing XRef table (v1)" pResV1
                          
     -- run-time testing of equivalance of parseXRefsVersion{1,2}
     when (trail /= trail') $
       putStrLn "WARN: trail-v1 /= trail-v2"
     when (refs /= refs') $
       putStrLn "WARN: refs-v1 /= refs-v2"

     validateUpdates (incUpdates, refs', trail')

     -- FIXME[E2]: when more sure of v1 == v2, remove the parseXRefsVersion1 code.
     
     fileEC <- makeEncContext trail refs topInput (password opts)
               -- calls EncryptionDict which calls 'ResolveValRef'!

     let ppRef pref r@(Ref ro rg) =
           do res <- runParser refs (fileEC (ro, rg)) (pResolveRef r) topInput
              case res of
                ParseOk a ->
                  case a of
                    Just d  -> print (pref <+> pp d)
                    Nothing -> print (pref <+> pp (Value_null ()))
                                      -- FIXME: This what the user wants to see?
                ParseErr e    -> print (pref <+> pp e)
                ParseAmbig {} -> quit "BUG: Ambiguous result"

         rToRef (R x y) = Ref (fromIntegral x) (fromIntegral y)

     case command opts of
       ListXRefs      -> printObjIndex 0 refs
       
       ListIncUpdates -> do
                         printIncUpdateReport incUpdates
                         putStrLn "Combined xref table:"
                         printObjIndex 2 refs

       ListCavities   -> do
                         (_isBinary,baseBodyStart)
                           <- runParser (error "pHeader: no ref expected")
                                           Nothing
                                           ( do
                                             h <- pHeader
                                             o <- pOffset  -- byte offset after header             
                                             return (h,o)
                                           )
                                           topInput
                                >>= quitIfParseError "parsing PDF header"

                                -- FIXME[F1]: problem elsewhere: this the
                                -- only code that looks at header!

                         -- FIXME[F3]: check that !isBinary => no binary bytes in file.
                         printCavityReport baseBodyStart topInput incUpdates

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
          do runParser refs Nothing (pPdfTrailer trail) topInput
               >>= quitIfParseError "parsing PDF trailer"
                              
             putStrLn "PDF trailer is well-formed (use other options to check more)"

       ShowHelp -> dumpUsage options

quit :: String -> IO a
quit msg =
  do hPutStrLn stderr msg
     exitFailure

quitIfParseError :: String -> PdfResult a -> IO a 
quitIfParseError context r = 
 case r of
   ParseOk a     -> pure a
   ParseAmbig {} -> quit (msg ++ ": ambiguous.")
   ParseErr e    -> quit (msg ++ ":\n\n" ++ show (pp e))
  where
  msg = "Fatal error while " ++ context ++ ", cannot proceed"
    
warnIfParseError :: String -> PdfResult a -> IO ()
warnIfParseError context r = 
 case r of
   ParseOk a     -> return ()
   ParseAmbig {} -> warn (msg ++ ": ambiguous.")
   ParseErr e    -> warn (msg ++ ":\n\n" ++ show (pp e))
  where
  msg = "Fatal error while " ++ context ++ ", cannot proceed"
  warn s = hPutStrLn stderr s
  
-- XXX: Identical code in pdf-driver/src/driver/Main.hs. Should de-duplicate
makeEncContext :: Integral a => 
                     TrailerDict  
                  -> ObjIndex 
                  -> Input 
                  -> BS.ByteString 
                  -> IO ((a, a) -> Maybe EncContext)
makeEncContext trail refs topInput pwd = 
  do edict <- runParser refs Nothing (pMakeContext trail) topInput
              >>= quitIfParseError "extracting context for encryption"
                               
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
