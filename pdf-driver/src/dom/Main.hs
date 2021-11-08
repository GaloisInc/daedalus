{-# Language OverloadedStrings, TypeApplications, DataKinds, BlockArguments #-}
import           Data.ByteString(ByteString)
import qualified Data.ByteString          as BS
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Map as Map
import Control.Monad(when,unless)
import Data.Either
import GHC.Records(getField) 
import System.IO(hPutStrLn,stderr,stdout)
import System.Exit(exitFailure)
import Text.PrettyPrint
import SimpleGetOpt

import RTS.Input(newInput,inputBytes)
import RTS.Vector(vecFromRep,vecToRep,toList) 
import RTS.Numeric

import qualified XRef
import XRef( findStartXRef
           , parseXRefsVersion2
           , printIncUpdateReport
           , printObjIndex
           , validateUpdates
           , printCavityReport
           , allOrNoUpdates
           , fromPdfResult
           , FileOffset
           , Possibly
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

logMsg s = putStrLn s
           -- FIXME[F2] TODO: allow control by a verbose flag or the like.


parseXRefsVersion1 :: DbgMode => Input -> FileOffset -> IO (Possibly (ObjIndex, TrailerDict))
parseXRefsVersion1 inp off0 =
  XRef.parseXRefsVersion1 inp off0 >>= return . fromPdfResult "parsing xref (v1)"

     
-- FIXME[E2]: when more sure of v1 == v2, remove this and the
--            parseXRefsVersion1 code.
-- checkV1V2Consistency :: (Possibly ObjIndex, Possibly TrailerDict) -> IO ()
checkV1V2Consistency topInput idx (refs2, trailer2) =
  do
  logMsg "parseXRefsVersion1:"
  resV1 <- parseXRefsVersion1 topInput idx 

  -- high level V1, V2 conformity:
  let v2IsGood    = isRight refs2 && isRight trailer2
      v1v2conform = case resV1 of
                      Right _ -> v2IsGood
                      Left  _ -> not v2IsGood
  unless v1v2conform $        
    putStrLn "WARN: V1 and V2 xref parsers do not conform"

  case resV1 of
    Left  ss            ->
        warn $ addContextToMsg "computing XRef table (V1)" ss
    Right (refs1, trailer1)
        | Right refs2'    <- refs2
        , Right trailer2' <- trailer2
        ->
            do
            -- run-time testing of equivalence of parseXRefsVersion{1,2}
            when (trailer1 /= trailer2') $
              putStrLn "WARN: trailer_V1 /= trailer_V2"
            when (refs1 /= refs2') $
              putStrLn "WARN: refs_V1 /= refs_V2"
        | otherwise ->
            return ()

parsePdf :: Settings -> FilePath -> ByteString -> Input -> IO ()
parsePdf opts file bs topInput =
  do idx <- case findStartXRef bs of
              Left err   -> quit err
              Right idx' -> pure idx'

     (updates, mRefs, mTrailer) <- parseXRefsVersion2 topInput idx 
       -- shouldn't fail, but returns Possible's

     checkV1V2Consistency topInput idx (mRefs, mTrailer)
     validateUpdates (updates, mRefs, mTrailer)

     -- FIXME[F2]: let's not give up so easily!
     incUpdates <- quitOnFail "updates" (allOrNoUpdates updates)
     refs       <- quitOnFail "mRefs"   mRefs
     trailer    <- quitOnFail "trailer" mTrailer
     
     fileEC <- makeEncContext trailer refs topInput (password opts)
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
                ParseAmbig {} -> quit "BUG: Ambiguous parse result"

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
         | object opts < 0 -> print (pp trailer)
         | otherwise -> ppRef "" (Ref (object opts) (generation opts))

       Validate ->
          do runParser refs Nothing (pPdfTrailer trailer) topInput
               >>= quitIfParseError "parsing PDF trailer"
                              
             putStrLn "PDF trailer is well-formed (use other options to check more)"
             -- xref and such was also checked
             -- FIXME[EC2]: did we already check the PdfTrailer??
             
       ShowHelp -> dumpUsage options

quit :: String -> IO a
quit msg =
  do hPutStrLn stdout msg
     exitFailure


quitOnFail :: String -> Possibly a -> IO a 
quitOnFail context r = case r of
                         Right x -> pure x
                         Left ss -> quit (unlines (msg:ss))
  where
  msg = "Fatal error while " ++ context ++ ", cannot proceed"

quitIfParseError :: String -> PdfResult a -> IO a 
quitIfParseError context r = 
 case r of
   ParseOk a     -> pure a
   ParseAmbig {} -> quit (msg ++ ": ambiguous.")
   ParseErr e    -> quit (msg ++ ":\n\n" ++ show (pp e))
  where
  msg = "Fatal error while " ++ context ++ ", cannot proceed"

addContextToMsg contextAsVerb ss = msg : (map ("  "++) ss)
  where
  msg = "while " ++ contextAsVerb ++ ":"

warnIfFail :: String -> Possibly a -> IO ()
warnIfFail context r = 
 case r of
   Right a     -> return ()
   Left ss     -> warn ss

warn :: [String] -> IO ()
warn []     = quit "panic: warning with empty message"
warn (s:ss) = mapM_ putStrLn $ ("warning: "++s) : map ("  "++) ss
  
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
