{-# LANGUAGE EmptyDataDecls, TypeOperators, LambdaCase #-}
module Primitives where

import           Control.Monad
import qualified Data.Map as M
import           Data.Map(Map)

import           Types
import           Utils

-- misc primitives

validate :: Bool -> P ()

validateAction :: P Bool -> P ()

-- primitive parsers

readToPrimitive :: Offset -> P String

seekPrimitive :: Offset -> P ()

keyword :: String -> P ()

(.|.) :: P a -> P a -> P a

parseString :: P a -> ByteString -> P a

-- higher level parsers

pTopLevelDef_UnDecStm :: P TopLevelDef_UnDecStm

pXrefRaw :: P (XRefRaw,Offset)

pSimpleWhiteSpace :: P String

pDictionary :: P Dict

pValue :: P PdfValue

-- complex parsers

findPDFHeader :: Int -> P ((Int,Int),Offset)

findStartxrefThenParseToEOF :: Int -> P Offset

-- various functions

decodeStream :: PdfValue -> a -> Offset -> P ByteString

dictToTrailerDict :: Dict -> P TrailerDict

trailerDict_getPrev :: TrailerDict -> Maybe Offset

getKeyOfType :: Name -> PdfType -> Dict -> P PdfValue

derefTLD :: Map ObjId (TopLevelDef' a :+: b) -> ObjId -> P (TopLevelDef' a)

derefValue :: Map ObjId (TopLevelDef' a :+: b) -> PdfValue -> P PdfValue

updateVersionFromCatalogDict :: DOM -> a -> P a

extractDecodingParameters :: Dict -> P a

removeFrees :: Map k XRefEntry -> Map k (Offset :+: Type2Ref)

thawXRefEntry :: SEEK -> XRefRaw -> ObjId -> P (ObjId, XRefEntry)

getObjIds :: XRefRaw -> [ObjId]

unknownKeysInTrailerDict :: [Update] -> Dict

-- various predicates for validation

versionAndDomConsistent :: (Int,Int) -> DOM -> Bool

trailersConsistentAcrossUpdates :: [Update] -> Bool

verifyXrefRaw :: XRefRaw -> Bool

-----------------------

pTopLevelDef_UnDecStm = stub


---- compiler ----------------------------------------------------------------

-- | validateAction - used when we need to read from file, or the validation
--                    itself calls a parser or could fail.
--                    NOTE: prefer validate!
validateAction a = do
                   b <- a
                   validate b
  
validate condition = if validatingParser then
                       do
                       unless condition $ fail "fails validation"
                     else
                       return ()
                     -- or maybe warn
  where
  validatingParser = False -- could be passed on command line

---- higher level 
updateVersionFromCatalogDict = stub


extractDecodingParameters = stub

versionAndDomConsistent = stub

---- Dictionaries ------------------------------------------------------------

-- validate the trailer dictionary:
dictToTrailerDict = stub

trailerDict_getPrev = stub

getKeyOfType _kname _pdftype _d = stub


---- unimplemented utility functions -----------------------------------------

derefValue _ (V_Ref _i _g) = stub "lookup ..." 
derefValue _ x             = return x

derefTLD m oi = stub "derefTLD" m oi


---- unimplemented, lower level parser functions -----------------------------

-- | pTopLevelDef_UnDecStm off - parse TopLevelDef at 'off' but if a stream,
--     we parse the dictionary but we don't yet decode and get the bytestring.
--   NOTE
--     - this needs to be a primitive parser if want to be efficient:
--       - parses top level values but if a stream, just record the offset
--         of 'stream' keyword.
--       - this allows us to parse top-levels without requiring a DOM to lookup objects in
--     - only the first two constructors of TopLevelDef' will be returned.


-- | pXrefRaw - finds the subsections

pXrefRaw = stub


verifyXrefRaw _ = stub

-- lower level "parsers":

decodeStream _len _etc _off = stub "stream-data"

findPDFHeader _byteLimit = stub

findStartxrefThenParseToEOF _byteLimit = stub

-- basic parsers:

pSimpleWhiteSpace = stub

pDictionary = stub

pValue = stub

-- parser P primitives:
parseString _p _s = stub

keyword s = stub s

seekPrimitive _offset = stub

readToPrimitive _offset = stub

(.|.) = stub

trailersConsistentAcrossUpdates = stub

removeFrees m = M.mapMaybe
                  (\x -> case x of Right y -> Just y
                                   _       -> Nothing)
                  m

thawXRefEntry = notImplementedYet

getObjIds = notImplementedYet

unknownKeysInTrailerDict = stub
