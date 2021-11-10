{-# Language OverloadedStrings, TypeApplications, DataKinds, BlockArguments #-}
module Main(main) where

-- system:
import           Control.Monad(when,unless)
import           Data.ByteString(ByteString)
import qualified Data.ByteString          as BS
import           Data.Either
import qualified Data.Map as Map
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           GHC.Records(getField) 
import           System.IO(hPutStrLn,stderr,stdout)
import           System.Exit(exitFailure)
import           Text.PrettyPrint

import           SimpleGetOpt

-- daedalus:
import RTS.Input(newInput,inputBytes)
import RTS.Vector(vecFromRep,vecToRep,toList) 
import RTS.Numeric

-- local:
import qualified XRef
import XRef( findStartXRef
           , FileOffset
           )
import IncUpdates( parseXRefsVersion2
                 , printIncUpdateReport
                 , printObjIndex
                 , validateUpdates
                 , printCavityReport
                 , allOrNoUpdates
                 , fromPdfResult
                 )
import Logger
import Possibly    
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

parseXRefsVersion1 :: DbgMode => Input ->
                      FileOffset -> IO (Possibly (ObjIndex, TrailerDict))
parseXRefsVersion1 inp off0 =
  XRef.parseXRefsVersion1 inp off0
    >>= return . fromPdfResult "parsing XRef table (V1)"

     
-- FIXME[E2]: when more sure of v1 == v2, remove this and the
--            parseXRefsVersion1 code.
checkV1V2Consistency :: Input -> FileOffset ->
                        (Possibly ObjIndex, Possibly TrailerDict) -> IO ()
checkV1V2Consistency topInput idx (refs2, trailer2) =
  do
  logInfo "BEGIN checking conformity of V1 and V2 xref processing"
  resV1 <- parseXRefsVersion1 topInput idx 

  -- high level V1, V2 conformity:
  let v2IsGood    = isRight refs2 && isRight trailer2
      v1v2conform = case resV1 of
                      Right _ -> v2IsGood
                      Left  _ -> not v2IsGood
                      
  unless v1v2conform $        
    logWarnIndent 1 "V1 and V2 XRef parsers do not conform"

  case resV1 of
    Left  ss            ->
        logWarnSIndent 1 ss
    Right (refs1, trailer1)
        | Right refs2'    <- refs2
        , Right trailer2' <- trailer2
        ->
            -- run-time testing of equivalence of parseXRefsVersion{1,2}
            do
            when (trailer1 /= trailer2') $
              logWarnIndent 1 "trailer_V1 /= trailer_V2"
            when (refs1 /= refs2') $
              logWarnIndent 1 "refs_V1 /= refs_V2"
        | otherwise ->
            return ()
  logInfo "END checking conformity of ..."

parsePdf :: Settings -> FilePath -> ByteString -> Input -> IO ()
parsePdf opts file bs topInput =
  do idx <- case findStartXRef bs of
              Left err   -> quit err
              Right idx' -> pure idx'

     (incUpdates, mRefs, mTrailer) <- parseXRefsVersion2 topInput idx 
       -- shouldn't fail, but returns Possible's

     checkV1V2Consistency topInput idx (mRefs, mTrailer)
     validateUpdates (incUpdates, mRefs, mTrailer)

     -- let's not unnecessarily fail, only fail when these next three
     -- are called to acquire the necessary values:
     let getRefs    = logErrorIfFail
                      $ addContextToPossibly "parsing XRef table" mRefs
         getTrailer = logErrorIfFail
                      $ addContextToPossibly "parsing Trailer"    mTrailer
         mk_ppRef =
           do
           refs <- getRefs
           trailer <- getTrailer
           fileEC <- makeEncContext trailer refs topInput (password opts)
             -- calls EncryptionDict which calls 'ResolveValRef'!
             -- FIXME[F2]: feels odd, what happens when the values resolved
             -- here are updated??
           let ppRef pref r@(Ref ro rg) =
                 do
                 res <- runParser refs
                                  (fileEC (ro, rg))
                                  (pResolveRef r)
                                  topInput
                 case res of
                   ParseOk a     ->
                     case a of
                       Just d  -> print (pref <+> pp d)
                       Nothing -> print (pref <+> pp (Value_null ()))
                                    -- FIXME: This what the user wants to see?
                   ParseErr e    -> print (pref <+> pp e)
                   ParseAmbig {} -> quit "BUG: Ambiguous parse result"
           return ppRef

     case command opts of
       ListXRefs      ->
           do
           refs <- getRefs
           printObjIndex 0 refs
       
       ListIncUpdates ->
           do
           printIncUpdateReport incUpdates
           putStrLn "Combined XRef table:"
           refs <- getRefs
           printObjIndex 2 refs

       ListCavities->
           do
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
           do
           ppRef <- mk_ppRef
           refs <- getRefs
           case map rToRef (Map.keys refs) of
             []   -> putStrLn "[]"
             x:xs -> do
                     ppRef "[" x
                     mapM_ (ppRef ",") xs
                     putStrLn "]"
           where
           rToRef (R x y) = Ref (fromIntegral x) (fromIntegral y)


       PrettyPrint
         | object opts < 0 -> do
                              trailer <- getTrailer
                              print (pp trailer)
         | otherwise       -> do
                              ppRef <- mk_ppRef
                              ppRef "" (Ref (object opts) (generation opts))

       Validate ->
           do
           refs <- getRefs
           trailer <- getTrailer
           runParser refs Nothing (pPdfTrailer trailer) topInput
             >>= quitIfParseError "parsing PDF trailer"
                               
           putStrLn "PDF trailer & crossref table are valid (use other options to check more)"
           -- FIXME[EC2]: did we already check the PdfTrailer??
             
       ShowHelp ->
           dumpUsage options

quitIfParseError :: String -> PdfResult a -> IO a 
quitIfParseError context r = 
 case r of
   ParseOk a     -> pure a
   ParseAmbig {} -> quit (msg ++ ": ambiguous.")
   ParseErr e    -> quit (msg ++ ":\n\n" ++ render (pp e))
  where
  msg = "Fatal error while " ++ context ++ ", cannot proceed"

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
