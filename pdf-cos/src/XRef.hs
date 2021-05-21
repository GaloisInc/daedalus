{-# Language TypeApplications, DataKinds #-}
{-# Language FlexibleContexts, ConstraintKinds #-}
{-# Language OverloadedStrings #-}
module XRef where

import Data.Char(isSpace,isNumber)
import Data.Foldable(foldlM)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Control.Monad(unless,foldM)
import Control.Applicative((<|>))
import GHC.Records(HasField, getField)
import System.Exit(exitFailure)
import System.IO(hPutStrLn,stderr)

import RTS.Vector(Vector,toList,VecElem)
import RTS.Numeric
import RTS.Input(advanceBy)

import PdfMonad
import PdfParser
import Stdlib(pManyWS)

-- | Construct the xref table.
parseIncUpdates ::
  DbgMode => Input -> Int -> IO ([(ObjIndex, TrailerDict)])
parseIncUpdates inp offset0 =
  do
  (x, next) <- parseOneIncUpdate offset0
  case next of
    Just offset -> (x:) <$> parseIncUpdates inp offset
    Nothing     -> return [x]
  
  where

  parseOneIncUpdate :: Int -> IO ((ObjIndex, TrailerDict), Maybe Int)
  parseOneIncUpdate offset =
    handlePdfResult (runParserWithoutObjects (parseOneIncUpdate' inp offset) inp)
                    "parsing single incremental update"
      

parseOneIncUpdate' :: Input -> Int -> Parser ((ObjIndex, TrailerDict), Maybe Int)
parseOneIncUpdate' inp offset = 
  case advanceBy (intToSize offset) inp of
    Just i ->
      do pSetInput i
         refSec <- pCrossRef  -- slight misnomer, this parses
                              --   - standard xref table OR xref streams
                              --   - the trailer too (in the former case)
         case refSec of
           CrossRef_oldXref x -> parseTrailer x
           CrossRef_newXref x -> parseTrailer x
    Nothing -> pError FromUser "parseOneIncUpdate"
                 ("Offset out of bounds: " ++ show offset)

  where
  
  parseTrailer :: ( VecElem s
                  , HasField "trailer" x TrailerDict
                  , HasField "xref"    x (Vector s)
                  , XRefSection s e o
                  ) => x -> Parser ((ObjIndex, TrailerDict), Maybe Int)
  parseTrailer x =
    do let t = getField @"trailer" x
       prev <- case getField @"prev" t of
                 Nothing -> pure Nothing
                 Just i ->
                    case toInt i of  -- FIXME: TODO: remember previous offsets
                                     -- to ensure we are not stuck in a loop.
                      Nothing  -> pError FromUser "parseTrailer"
                                                  "Prev offset too large."
                      Just off -> pure (Just off)

       tabs <- mapM xrefSubSectionToMap (toList (getField @"xref" x))
       let entries = Map.unions tabs
       unless (Map.size entries == sum (map Map.size tabs))
         (pError FromUser "parseXRefs.goWith(2)" "Duplicate entries in xref seciton")
         -- FIXME: put this into 'validate'
       -- let newRoot = mbRoot <|> Just t
       return ((entries, t), prev)

---- de-duplicate ------------------------------------------------------------

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
        ParseErr e    -> quit (show e) -- (pp e))

---- utilities ---------------------------------------------------------------

foldr1M :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldr1M f = g . reverse
  where
  g (x:xs) = foldlM (flip f) x xs
  g []     = error "foldr1M"

runParserWithoutObjects :: DbgMode => Parser a -> Input -> IO (PdfResult a)
runParserWithoutObjects p i = 
  runParser (error "Unexpected ObjIndex reference") Nothing p i
    -- the parser should not be attempting to deref any objects!
  
---- xref table: parse and construct -----------------------------------------

-- | Construct the xref table, version 2
parseXRefs2 ::
  DbgMode => Input -> Int -> IO (ObjIndex, TrailerDict)
parseXRefs2 inp off0 =
  do
  xrefs <- parseIncUpdates inp off0
  mapM_ validateXRef xrefs
  foldr1M applyIncUpdate xrefs

validateXRef :: DbgMode => (ObjIndex,TrailerDict) -> IO ()
validateXRef _ = return ()

-- | applyIncUpdate upd base = extend 'base' with the 'upd' incremental update
applyIncUpdate :: Monad m =>
                  (ObjIndex,TrailerDict) -> (ObjIndex,TrailerDict) -> m (ObjIndex,TrailerDict) 
applyIncUpdate (oi',trailer') (oi,_trailer) =
  return ( Map.union oi' oi, trailer')
    -- NOTE: Map.union is left-biased.
    -- FIXME[F2]: validate that trailers are consistent

---- xref table: parse and construct (old version) ---------------------------

parseXRefs1 inp off0 =
  handlePdfResult (parseXRefs' inp off0) "BUG: Ambiguous XRef table."

-- | Construct the xref table.
parseXRefs' ::
  DbgMode => Input -> Int -> IO (PdfResult (ObjIndex, TrailerDict))
parseXRefs' inp off0 = runParser Map.empty Nothing (go Nothing (Just off0)) inp
  where
  go :: Maybe TrailerDict -> Maybe Int -> Parser (ObjIndex, TrailerDict)
  go mbRoot Nothing =
    do oix <- getObjIndex
       case mbRoot of
         Just r -> return (oix, r)
         Nothing -> pError FromUser "parseXRefs.go" "Missing document root."

  go mbRoot (Just offset) =
    case advanceBy (intToSize offset) inp of
      Just i ->
        do pSetInput i
           pManyWS  -- FIXME: later: warning if this consumes anything
           refSec <- pCrossRef
           case refSec of
             CrossRef_oldXref x -> goWith mbRoot x
             CrossRef_newXref x -> goWith mbRoot x
      Nothing -> pError FromUser "parseXRefs.go"
                  ("Offset out of bounds: " ++ show offset)

  goWith :: ( VecElem s
            , HasField "trailer" x TrailerDict
            , HasField "xref"    x (Vector s)
            , XRefSection s e o
            ) => Maybe TrailerDict -> x -> Parser (ObjIndex, TrailerDict)
  goWith mbRoot x =
    do let t = getField @"trailer" x
       prev <- case getField @"prev" t of
                 Nothing -> pure Nothing
                 Just i ->
                    case toInt i of  -- XXX: remember previous offsets
                                     -- to ensure we are not stuck in a loop.
                      Nothing -> pError FromUser "parseXRefs.goWith(1)"
                                                "Prev offset too large."
                      Just off -> pure (Just off)

       tabs <- mapM xrefSubSectionToMap (toList (getField @"xref" x))
       let entries = Map.unions tabs
       unless (Map.size entries == sum (map Map.size tabs))
         (pError FromUser "parseXRefs.goWith(2)" "Duplicate entries in xref seciton")

       let newRoot = mbRoot <|> Just t

       extendObjIndex entries (go newRoot prev)
         -- entries may be shadowing previous entries
         -- note that the 'entries' are being added, in order, from the top of the file
         -- and in the reverse order in which 'extensions' would be applied.

type XRefSection s e o =
  ( VecElem e
  , HasField "firstId" s Integer
  , HasField "entries" s (Vector e)
  , SubSectionEntry e
  )

class SubSectionEntry t where
  processEntry :: Int -> t -> Parser (Maybe (R,ObjLoc))

instance SubSectionEntry XRefObjEntry where
  processEntry o t =
    case t of
      XRefObjEntry_inUse u ->
        do off <- integerToInt (getField @"offset" u)
           g   <- integerToInt (getField @"gen" u)
           pure (Just (R { refObj = o, refGen = g }, InFileAt off))

      XRefObjEntry_compressed u ->
        do cnt <- integerToInt (getField @"container_obj" u)
           let ref x = R { refObj = x, refGen = 0 }
           i <- integerToInt (getField @"obj_index" u)
           pure (Just (ref o, InObj (ref cnt) i))

      XRefObjEntry_free {} -> pure Nothing
      XRefObjEntry_null {} -> pure Nothing

instance SubSectionEntry CrossRefEntry where
  processEntry o t =
    case t of
      CrossRefEntry_inUse u ->
        do g   <- integerToInt (getField @"gen" u)
           off <- integerToInt (getField @"offset" u)
           pure (Just (R { refObj = o, refGen = g }, InFileAt off))

      CrossRefEntry_free {} -> pure Nothing





-- | Join together the entries in a single xref sub-section.
xrefSubSectionToMap :: XRefSection s e o => s -> Parser ObjIndex
xrefSubSectionToMap xrs = foldM entry Map.empty
                        $ zip [ getField @"firstId" xrs .. ]
                        $ toList (getField @"entries" xrs)
  where
  {- Note the file offset and generation will never really fail
     because in the format the offset is a 10 digit number, which
     always fits in a 64-bit Int.  We still need the check to get the types
     to work out, and this also makes the code more portable, in theory. -}
  entry mp (n,e) =
    do o <- integerToInt n
       mb <- processEntry o e
       case mb of
         Nothing -> pure mp    -- XXX: This skips compressed objects.
         Just (ref,oi) ->
           let (exists,newMap) = Map.insertLookupWithKey (\_ x _ -> x) ref oi mp
           in case exists of
                Nothing -> pure newMap
                Just _  ->
                  pError FromUser "xrefSubSectionToMap.entry"
                                      ("Multiple entries for " ++ show ref)


integerToInt :: Integer -> Parser Int
integerToInt i =
  case toInt i of
    Nothing -> pError FromUser "integerToInt" "Integer constant too large."
    Just x  -> pure x


--------------------------------------------------------------------------------

-- Pretty gross
findStartXRef :: BS.ByteString -> Either String Int
findStartXRef bs
  | BS.null post   = Left "Couldn't find EOF"
  | not (BS.isPrefixOf (BS.reverse "startxref") (BS.dropWhile isSpace rest)) =
      Left "Couldn't find startxref"
  | otherwise      = Right $ read $ BS.unpack $ BS.reverse numBits
  where
  eof             = "%%EOF"
  (_pre, post)    = BS.breakSubstring (BS.reverse eof) (BS.reverse lastChunk)
  (numBits, rest) = BS.span isNumber
                  $ BS.dropWhile isSpace
                  $ BS.drop (BS.length eof) post

  len             = BS.length bs

  -- Leave 100 bytes for number and startxref (FIXME)
  lastChunk       = BS.drop (len - 1024 - BS.length eof - 100) bs



