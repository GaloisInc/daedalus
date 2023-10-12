{-# Language TypeApplications, DataKinds #-}
{-# Language FlexibleContexts, ConstraintKinds #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
module XRef
  -- the high level interface, for the Version1 of parse xrefs
  ( findStartXRef
  , parseXRefsVersion1
  -- a lower level interface, used to define Version2 of parse xrefs
  , runParserWithoutObjectIndex
  , runParserWithoutObjectIndexFailOnRef
  , ppXRefType
  , ppXRefEntry
  , convertToXRefEntries
  , xrefEntriesToMap
  -- types:
  , FileOffset
  , PrevSet
  , XRefType(..)
  , XRefEntry(..)
  , XRefSection
  )
  where

-- system:
import           Control.Applicative((<|>))
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Char(isSpace,isNumber)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import           GHC.Records(HasField, getField)
import           Text.PrettyPrint

-- local:
import Daedalus.Panic 
import Daedalus.RTS.Vector(Vector,toList,VecElem)
import Daedalus.RTS.Numeric
import Daedalus.RTS.Input(advanceBy)
import RTS.ParseError

import PdfMonad
import PdfParser

import Possibly
import PdfPP

---- types -------------------------------------------------------------------

-- | set of all the "Prev" offsets we have encountered
type PrevSet = IntSet.IntSet

type FileOffset = UInt 64
data XRefType = XRefTable   -- traditional xref table
              | XRefStream  -- newer alternative to XRef table
              deriving (Eq,Ord)


ppXRefType :: XRefType -> Doc
ppXRefType XRefTable  = "traditional cross reference table"
ppXRefType XRefStream = "cross reference stream"
  
-- Features to add
--  - pass in flags
--    - flag for each exuberance?
--    - be able to specify sets of flags
--      - "strict PDF"
--      - commonly-allowed PDF

---- parsing when no Object Index yet available ------------------------------

-- a new exception
data DerefException = DerefException
                      deriving Show
instance Exception DerefException


runParserWithoutObjectIndexFailOnRef ::
  DbgMode => String -> Input -> Parser a -> IO (PdfResult a)
runParserWithoutObjectIndexFailOnRef contextMsg i p =
  runParser (errorIfDomDependentParser contextMsg)  Nothing p i
  -- error if the parser should attempt to deref any objects.

  where
  errorIfDomDependentParser :: String -> a
  errorIfDomDependentParser contextMsg' =
    panic
      ("dereferencing object index should not happen here: " ++ contextMsg')
      []
  -- FIXME[C2]

runParserWithoutObjectIndex :: DbgMode => Input -> Parser a -> IO (Maybe (PdfResult a))
runParserWithoutObjectIndex i p =
  do
  x <- try (runParser (throw DerefException) Nothing p i)
       -- catch if the parser should deref any objects!
  case x of
    Left DerefException -> pure Nothing
    Right x'            -> pure $ Just x'


---- xref table: parse and construct (old version) ---------------------------

-- | Construct the xref table, version 1
--
--   - the difference (vs version 2) is that we are doing all in one go and we
--   - need to stay inside the Parser monad because that's where the ObjIndex is
--   - being extended!
-- 
--   - Unfortunately: error messages will not be good as the following get all intermingled
--     with all the parsing errors inside parser code.

parseXRefsVersion1 :: DbgMode => Input -> FileOffset -> IO (PdfResult (ObjIndex, TrailerDict))
parseXRefsVersion1 inp off0 =
  runParser Map.empty Nothing
    (go Nothing (Just off0) (IntSet.singleton (sizeToInt off0)))
    inp
  
  where
  go :: Maybe TrailerDict -> Maybe FileOffset -> PrevSet -> Parser (ObjIndex, TrailerDict)
  go mbRoot Nothing _ =
    do oix <- getObjIndex
       case mbRoot of
         Just r -> return (oix, r)
         Nothing -> pError' FromUser [] "Missing document root."

  go mbRoot (Just offset) prevSet =
    -- precondition: offset `member` prevSet
    case advanceBy offset inp of
      Just i ->
        do pSetInput i
           pManyWS  -- FIXME: later: warning if this consumes anything
           refSec <- pCrossRef
           case refSec of
             CrossRef_oldXref x -> goWith mbRoot x prevSet
             CrossRef_newXref x -> goWith mbRoot x prevSet
      Nothing -> pError' FromUser [] ("Offset out of bounds: " ++ show offset)

  goWith :: ( VecElem s
            , HasField "trailer" x TrailerDict
            , HasField "xref"    x (Vector s)
            , XRefSection s e o
            ) => Maybe TrailerDict -> x -> PrevSet -> Parser (ObjIndex, TrailerDict)
  goWith mbRoot x prevSet =
    do let t = getField @"trailer" x
       prevOffset <-
         case getField @"prev" t of
           Nothing -> pure Nothing
           Just i ->
              case toInt i of
                Nothing  -> pError' FromUser [] "Prev offset too large."
                Just off -> do
                            -- ensure no infinite loop of incremental updates
                            when (off `IntSet.member` prevSet) $
                              pError' FromUser []
                                $ unwords[ "recursive incremental updates:"
                                         , "adding", show off, "to", show prevSet]
                            pure (Just $ intToSize off)
                            -- FIXME: would be even cleaner if we used the offset of 'xref'

       tabs <- pErrorIfFail $
                 mapM (\s->convertToXRefEntries s >>= xrefEntriesToMap)
                      (toList (getField @"xref" x))
       let entries = Map.unions tabs
       unless (Map.size entries == sum (map Map.size tabs))
         (pError' FromUser [] "Duplicate entries in xref section")

       let prevSet' = case prevOffset of
                        Nothing -> prevSet
                        Just o  -> IntSet.insert (sizeToInt o) prevSet

       extendObjIndex entries
         (go (mbRoot <|> Just t) prevOffset prevSet')
         -- entries may be shadowing previous entries
         -- note that the 'entries' are being added, in order, from the top of the file
         -- and in the reverse order in which 'extensions' would be applied.

pErrorIfFail :: Possibly a -> Parser a
pErrorIfFail = \case
                 Left ss -> pError' FromUser [] (unlines ss)
                 Right a -> return a

---- common code for V1 and V2 -----------------------------------------------
    
type XRefSection s e o =
  ( VecElem e
  , HasField "firstId" s Integer
  , HasField "entries" s (Vector e)
  , SubSectionEntry e
  )

class SubSectionEntry t where
  processEntry :: Int -> t -> Possibly XRefEntry

data XRefEntry = InUse R ObjLoc
               | Free  Int R     -- object is next free object, generation
               | Null    

ppXRefEntry :: XRefEntry -> Doc
ppXRefEntry =
  \case
    InUse (R o g) loc -> "inuse" <+> ppHDict [ "obj:" <+> pp o
                                             , "gen:" <+> pp g
                                             , ppObjLoc loc
                                             ]
    Free obj (R n g)  -> "free " <+> ppHDict [ "obj:" <+> pp obj
                                             , "next:" <+> pp n
                                             , "gen:" <+> pp g
                                             ]
    Null              -> "null"
      
instance SubSectionEntry XRefObjEntry where
  processEntry o t =
    case t of
      XRefObjEntry_inUse u ->
        do off <- integerToInt (getField @"offset" u)
           g   <- integerToInt (getField @"gen" u)
           pure $ InUse R{ refObj = o, refGen = g} (InFileAt off)

      XRefObjEntry_compressed u ->
        do cnt <- integerToInt (getField @"container_obj" u)
           let ref x = R { refObj = x, refGen = 0 }
           i <- integerToInt (getField @"obj_index" u)
           pure $ InUse (ref o) (InObj (ref cnt) i)

      XRefObjEntry_free u ->
        do obj <- integerToInt (getField @"obj" u)
           g   <- integerToInt (getField @"gen" u)
           pure $ Free o R{ refObj = obj, refGen = g}
      XRefObjEntry_null {} -> pure Null

instance SubSectionEntry CrossRefEntry where
  processEntry o t =
    case t of
      CrossRefEntry_inUse u ->
        do g   <- integerToInt (getField @"gen" u)
           off <- integerToInt (getField @"offset" u)
           pure $ InUse (R { refObj = o, refGen = g }) (InFileAt off)

      CrossRefEntry_free u ->
        do g   <- integerToInt (getField @"gen" u)
           obj <- integerToInt (getField @"obj" u)
           pure $ Free o R{ refObj = obj, refGen = g }

  {- Note the file offset and generation will never really fail
     because in the format the offset is a 10 digit number, which
     always fits in a 64-bit Int.  We still need the check to get the types
     to work out, and this also makes the code more portable, in theory.
  -}

-- | Join together the entries into a single xref sub-section
--    - merging XRefObjEntry and CrossRefEntry into XRefEntry
convertToXRefEntries :: XRefSection s e o => s -> Possibly [XRefEntry]
convertToXRefEntries xrs =
  forM (zip [ getField @"firstId" xrs .. ]
             (toList (getField @"entries" xrs))) $
       \(n,e)->     
          do o <- integerToInt n
             processEntry o e

-- | Create an Obj Index Map from the XRefEntry list
xrefEntriesToMap :: [XRefEntry] -> Possibly ObjIndex
xrefEntriesToMap = foldM entry Map.empty
  where
  entry mp xref =
       case xref of
         Free{} -> pure mp   
         Null   -> pure mp   -- XXX: This skips compressed objects. ??
         InUse ref oi ->
           let (exists,newMap) = Map.insertLookupWithKey (\_ x _ -> x) ref oi mp
           in case exists of
                Nothing -> pure newMap
                Just _  ->
                  failP ["Multiple entries for " ++ show ref]

integerToInt :: Integer -> Possibly Int
integerToInt i =
  case toInt i of
    Nothing -> failP ["Integer constant too large (" ++ show i ++ ")"]
    Just x  -> pure x

-----------------------------------------------------------------------------

-- Pretty gross
-- findStartXRef - find last "startxref", return the integer that follows (actual offset of xref)
findStartXRef :: BS.ByteString -> Either String FileOffset
findStartXRef bs
  | BS.null post   = Left "Couldn't find EOF"
  | not (BS.isPrefixOf (BS.reverse "startxref") (BS.dropWhile isSpace rest)) =
      Left "Couldn't find startxref"
  | otherwise      = Right $ intToSize $ read $ BS.unpack $ BS.reverse numBits
  where
  eof             = "%%EOF"
  (_pre, post)    = BS.breakSubstring (BS.reverse eof) (BS.reverse lastChunk)
  (numBits, rest) = BS.span isNumber
                  $ BS.dropWhile isSpace
                  $ BS.drop (BS.length eof) post

  len             = BS.length bs

  -- Leave 100 bytes for number and startxref (FIXME)
  lastChunk       = BS.drop (len - 1024 - BS.length eof - 100) bs

