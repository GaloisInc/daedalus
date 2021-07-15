{-# Language OverloadedStrings, TypeApplications, DataKinds, BlockArguments, ScopedTypeVariables #-}
module Main(main) where

import CommandLine

import Data.Text () -- IsString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map(Map)
import qualified Data.Map as Map
import GHC.Records(getField)
import System.Exit(exitFailure)
import System.CPUTime(getCPUTime)
import Text.PrettyPrint hiding ((<>))
import Control.Monad(when)
import Control.Monad.IO.Class(MonadIO(..))
import Control.Exception(evaluate,try,throwIO)
import RTS.Numeric(intToSize)
import RTS.Vector(vecFromRep,vecToString,vecToRep)
import RTS.Input

import Common
import PdfMonad
import XRef
import PdfParser
import PdfDemo
import PdfExtractText
import PdfCrypto(ChooseCiph(..),pMakeContext,MakeContext(..))
import Primitives.Decrypt(makeFileKey)

main :: IO ()
main =
  pdfMain
  do opts <- getOptions
     let fmt = case optMode opts of
                 Demo  -> demoFormat opts
                 FAW   -> fawFormat
     let inputFile = optPDFInput opts
     let pw = BS.pack $ optPassword opts
     case optMode opts of
       Demo  ->
         case optOps opts of
           Validate -> driverValidate opts
           ExtractText -> fmtDriver fmt inputFile pExtractCatalogText pw
       FAW   ->
         case optOps opts of
           Validate -> fmtDriver fmt inputFile pCatalogIsOK pw
           ExtractText -> fmtDriver fmt inputFile pExtractCatalogText pw



data Format = Format
  { onStart     :: FilePath -> ByteString -> IO ()
  , xrefMissing :: String -> IO ()
  , xrefFound   :: Int -> IO ()
  , xrefBad     :: ParseError -> IO ()
  , xrefOK      :: ObjIndex -> Map String Value -> IO ()
  , warnEncrypt :: IO()
  , rootMissing :: IO ()
  , rootFound   :: Ref -> IO ()
  , catalogParseError :: ParseError -> IO ()
  , catalogParsed :: String -> IO ()
  , declErr     :: R -> ObjLoc -> DeclResult' ParseError -> IO ()
  , declParsed  :: R -> ObjLoc -> DeclResult' CheckDecl -> IO ()
  }

fawFormat :: Format
fawFormat = Format
  { onStart =
      \_ bs -> putStrLn ("INFO: File size " ++ show (BS.length bs) ++ " bytes.")
  , xrefMissing =
      \x -> putStrLn ("ERROR: " ++ x)
  , xrefFound =
      \x -> putStrLn ("INFO:" ++ show x ++ " Found xref table.")
  , xrefBad =
      \p -> putStrLn ("ERROR:" ++ show (peOffset p) ++ " " ++ peMsg p)
  , xrefOK =
      \o _t -> putStrLn ("INFO: " ++ show (Map.size o) ++ " xref entries.")
  , warnEncrypt =
      putStrLn "WARNING: Encrypted document; spurious errors may follow."
  , rootMissing = putStrLn "ERROR: Trailer is missing `root` entry."
  , rootFound =
      \r -> putStrLn ("INFO: Root reference is " ++
                        showR (getField @"obj" r) (getField @"gen" r))
  , catalogParseError = \p ->
      putStrLn ("ERROR:" ++ show (peOffset p) ++ " " ++ peMsg p)
  , catalogParsed = \ok ->
      putStrLn ("INFO: Catalog value:\n" ++ ok)
  , declErr =
      \r l res ->
        let x = declResult res
        in putStrLn $ "ERROR:" ++ show (peOffset x) ++ " " ++
                      declTag r l ++ " " ++ peMsg x
  , declParsed = \r l res ->
      let saf   = getField @"isSafe" (declResult res)
          hasURI = [ "has URIs" | getField @"hasURI" saf ]
          hasJS  = [ "has JS"   | getField @"hasJS" saf ]
          (stat,msg) = case hasURI ++ hasJS of
                         [] -> ("INFO: ", "is OK")
                         ws -> ("WARNING: ", unwords ws)
      in putStrLn (stat ++ declTag r l ++ " " ++ msg ++ ".")

  }
  where
  showR x y = "R:" ++ show x ++ ":" ++ show y

  declTag r l = unwords [ showR (refObj r) (refGen r)
                        , declLoc l
                        ]
  declLoc l = case l of
                InFileAt x -> "@" ++ show x
                InObj {}   -> "@compresed"


-- demoFormat: format for reporting output directly. Tries to preserve
-- old implementation of driver.
demoFormat::Options -> Format
demoFormat opts =
  let optReport = runReport opts in
  let fileN = optPDFInput opts in
  let reportCritFile = \i d ->
        (fmap $ const ()) (reportCritical fileN i d) in
  let reportInfo = report RInfo fileN 0 in
  let reportErr = report RError fileN in Format
  { onStart = \_fp _bs -> return ()
    , xrefMissing = \s -> optReport $ reportCritFile
                          0 ("unable to find %%EOF" <+> parens (text s))
    , xrefFound = \r -> return ()
    , xrefBad = \err -> optReport $ reportCritFile
                        (peOffset err) (ppParserError err)
    , xrefOK = \idx m -> return ()
    , warnEncrypt = return ()
    , rootMissing = optReport $ reportCritFile
                    0 ("Missing document root")
    , rootFound = \r -> return ()
    , catalogParseError = \err -> optReport $ reportErr
      (peOffset err) (hang "Parsing Catalog/Page tree" 2 (ppParserError err))
    , catalogParsed = \isOk -> optReport $ reportInfo
      (text ("Catalog (page tree) result:\n" ++ isOk))
    , declErr = \res loc err -> return ()
    , declParsed = \res loc declRes -> return ()
  }


fmtDriver :: (DbgMode, Show a) => Format -> FilePath ->
  (Ref -> Parser a) ->
  BS.ByteString -> IO ()
fmtDriver fmt file pageTreeParser pwd =
  do bs <- BS.readFile file
     let topInput = newInput (Text.encodeUtf8 (Text.pack file)) bs
     onStart fmt file bs
     idx <- case findStartXRef bs of
              Left err -> xrefMissing fmt err >> exitFailure
              Right idx -> pure idx

     xrefFound fmt idx
     (refs, trail) <-
       parseXRefs topInput idx >>= \res ->
         case res of
           ParseOk a    -> pure a
           ParseAmbig _ -> error "BUG: Ambiguous XRef table."
           ParseErr e   -> xrefBad fmt e >> exitFailure

     let trailmap = getField @"all" trail
     xrefOK fmt refs (Map.mapKeys vecToString trailmap)
     when (Map.member (vecFromRep "Encrypt") trailmap) (warnEncrypt fmt)

     root <- case getField @"root" trail of
               Nothing -> rootMissing fmt >> exitFailure
               Just r -> pure r
     rootFound fmt root

     res <- runParser refs Nothing (pageTreeParser root) topInput
     case res of
       ParseOk r    -> catalogParsed fmt (show r)
       ParseAmbig _  -> error "BUG: Validation of the catalog is ambiguous?"
       ParseErr e    -> catalogParseError fmt e

     mb <- try (makeEncContext trail refs topInput pwd)
     case mb of
       Left err -> catalogParseError fmt err
       Right fileEC ->
         mapM_ (checkDecl fmt fileEC topInput refs) (Map.toList refs)



--------------------------------------------------------------------------------
type DeclResult = DeclResult' (PdfResult CheckDecl)

data DeclResult' a = DeclResult
                      { declTime       :: Int
                      , declCompressed :: Bool
                      , declResult     :: a
                      }

parseDecl ::
  DbgMode =>
  ((Int, Int) -> Maybe EncContext) ->
  Input -> ObjIndex -> (R, ObjLoc) -> IO DeclResult
parseDecl fileEC topInput refMap (ref,loc) =
  do start  <- getCPUTime
     result <- evaluate =<< runParser refMap objEC parser topInput
     end    <- getCPUTime
     pure DeclResult { declTime = fromIntegral ((end-start) `div` (10^(6::Int)))
                     , declCompressed = compressed
                     , declResult = result
                    }
  where
  (parser,compressed,objEC) =
    case loc of

      InFileAt off ->
        ( case advanceBy (intToSize off) topInput of
            Just i -> do pSetInput i
                         pTopDeclCheck (toInteger (refObj ref))
                                       (toInteger (refGen ref))
            Nothing -> pError' FromUser []
                       ("XRef entry outside file: " ++ show off)
        , False
        , fileEC (refObj ref, refGen ref)
        )

      InObj o idx ->
        ( pResolveObjectStreamEntryCheck (toInteger (refObj ref))
                                         (toInteger (refGen ref))
                                         (toInteger (refObj o))
                                         (toInteger (refGen o))
                                         (intToSize idx)
        , True
        , Nothing
        )

--------------------------------------------------------------------------------




checkDecl ::
  DbgMode =>
  Format ->
  ((Int, Int) -> Maybe EncContext) ->
  Input -> ObjIndex -> (R, ObjLoc) -> IO ()
checkDecl fmt fileEC topInput refMap d@(ref,loc) =
  do res <- parseDecl fileEC topInput refMap d
     case declResult res of
       ParseAmbig {} -> error "BUG: Ambiguous parse?"
       ParseErr e    -> declErr fmt ref loc res { declResult = e }
       ParseOk x     -> declParsed fmt ref loc res { declResult = x }


-- driver for Validate mode
driverValidate :: DbgMode => Options -> IO ()
driverValidate opts = runReport opts $
  do let file = optPDFInput opts
     bs   <- liftIO (BS.readFile file)
     let topInput = newInput (Text.encodeUtf8 (Text.pack file)) bs

     idx  <- case findStartXRef bs of
               Left err  -> reportCritical file 0
                                            ("unable to find %%EOF" <+> parens (text err))
               Right idx -> return idx

     (refs, root, trail) <-
            liftIO (parseXRefs topInput idx) >>= \res ->
            case res of
               ParseOk (r,t) -> case getField @"root" t of
                                  Nothing ->
                                    reportCritical file 0 "Missing document root"

                                  Just ro -> pure (r,ro,t)
               ParseAmbig _ ->
                 reportCritical file 0 "Ambiguous results?"
               ParseErr e ->
                 reportCritical file (peOffset e) (ppParserError e)

     res <- liftIO (runParser refs Nothing (pCatalogIsOK root) topInput)
     case res of
       ParseOk True  -> report RInfo file 0 "Catalog (page tree) is OK"
       ParseOk False -> report RUnsafe file 0 "Malformed Catalog (page tree)"
       ParseAmbig _  -> report RError file 0 "Ambiguous results?"
       ParseErr e    -> report RError file (peOffset e) (hang "Parsing Catalog/Page tree" 2 (ppParserError e))

     let pwd = BS.pack (optPassword opts)
     mb <- liftIO (try (makeEncContext trail refs topInput pwd))
     case mb of
       Left err -> reportCritical file (peOffset err) (ppParserError err)
       Right fileEC -> parseObjs file fileEC topInput refs


parseObjs ::
  DbgMode =>
  FilePath ->
  ((Int, Int) -> Maybe EncContext) ->
  Input -> ObjIndex -> ReportM ()
parseObjs fileN fileEC topInput refMap = mapM_ doOne (Map.toList refMap)
  where
  doOne d@(ref,_) =
    do res <- liftIO (parseDecl fileEC topInput refMap d)
       let sayTimed cl msg =
             do let timeMsg = parens (hcat [int (declTime res), "us"])
                    oidMsg = "OID" <+> int (refObj ref) <+> int (refGen ref)
                report cl fileN 0 (timeMsg <+> oidMsg <+> msg)

       let saySafety (x :: CheckDecl) (si :: TsafetyInfo) =
             case (getField @"hasJS" si, getField @"hasURI" si) of
               (False, False) -> sayTimed RInfo (sayObj (getField @"obj" x) <+> "parsed successfully")
               (True, False)  -> sayTimed RUnsafe "contains JavaScript"
               (False, True)  -> sayTimed RUnsafe "contains URIs"
               (True, True)   -> sayTimed RUnsafe "contains JavaScript and URIs"

       case declResult res of
         ParseAmbig {} -> sayTimed RError "Ambiguous parse?"
         ParseErr e    -> sayTimed RError (ppParserError e)
         ParseOk x     -> saySafety x (getField @"isSafe" x)

  sayObj :: TopDeclDef -> Doc
  sayObj def =
    case (getField @"stream" def, getField @"value" def) of
      (Just _,_) -> "stream"
      (_,Just v) -> sayVal v
      _ -> "unknown"

sayVal :: Value -> Doc
sayVal v = case v of
             Value_null {}   -> "null"
             Value_bool {}   -> "bool"
             Value_ref {}    -> "ref"
             Value_name {}   -> "name"
             Value_string {} -> "string"
             Value_number {} -> "number"
             Value_array {}  -> "array"
             Value_dict {}   -> "dict"


-- | Turn parse error into an exception
handlePdfResult :: IO (PdfResult a) -> String -> IO a
handlePdfResult x msg =
  do  res <- x
      case res of
        ParseOk a     -> pure a
        ParseAmbig {} -> error msg
        ParseErr e    -> throwIO e

-- XXX: Identical code in pdf-driver/src/dom/Main.hs. Should de-duplicate
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
