{-# Language TypeApplications, DataKinds #-}
{-# Language FlexibleContexts, ConstraintKinds #-}
{-# Language OverloadedStrings #-}
module XRef
  ( findStartXRef
  , parseXRefsVersion1
  , parseXRefsVersion2
  , printObjIndex
  , printIncUpdateReport
  , printCavityReport
  , validateUpdates
  -- types:
  , FileOffset
  )
  where

import           Control.Applicative((<|>))
import           Control.Exception
import           Control.Monad(unless,forM,forM_,foldM)
import qualified Data.ByteString.Char8 as BS
import           Data.Char(isSpace,isNumber)
import           Data.Either
import           Data.Foldable(foldlM)
import qualified Data.Map as Map
import           GHC.Records(HasField, getField)
import           System.Exit(exitFailure)
import           System.IO(hPutStrLn,stderr)

-- pkg range-set-list:
import qualified Data.RangeSet.IntMap as RIntSet

import Text.PrettyPrint

import RTS.Vector(Vector,toList,VecElem)
import RTS.Numeric
import RTS.Input(advanceBy)

import PdfMonad
import PdfParser
import Stdlib(pManyWS)
import PdfPP

---- types -------------------------------------------------------------------

type FileOffset = UInt 64

-- an incremental update (or the base DOM/update, at start of file)
data IncUpdate = IU { iu_type      :: XRefType
                    , iu_xrefs     :: [[XRefEntry]]
                    , iu_trailer   :: TrailerDict
                    , iu_startxref :: TrailerEnd  -- last part, holds startxref offset
                    } 

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

---- additional validation ---------------------------------------------------
-- validation done in IO to allow for warns as well as errors.

validateUpdates :: ([IncUpdate], ObjIndex, TrailerDict) -> IO ()
validateUpdates (updates,_,_) =
  do
  validateBase (head updates)
  -- FIXME[F2]: more things to validate!
  return ()

validateBase :: IncUpdate -> IO ()
validateBase iu =
  do
  let xrefss = iu_xrefs iu
  unless (length xrefss == 1) $
    err "must have only one subsection"

    -- Section 7.5.4: For a PDF file that has never been incrementally
    -- updated, the cross-reference section shall contain only one subsection,
    -- whose object numbering begins at 0.

  let [xrefs] = xrefss
  case xrefs of
    []                              -> err "must not be empty"
    Free 0 (R _ n) : _ | n == 65535 -> return ()
                       | n == 0     -> warn "object 0 has generation 0 (should be 65535)"
    _                               -> err "first object must be object 0, free, generation 65535"

    -- The first entry in the table (object number 0) shall always be free and
    -- shall have a generation number of 65,535;

    -- [first entry] shall be the head of the linked list of free objects.
    -- The last free entry (the tail of the linked list) links back to object number 0.

  case [ g | InUse (R _ g) _ <- xrefs, g /= 0] of
    _:_ -> err "objects exist without generation number 0"
    []  -> return ()

    -- Except for object number 0, all objects in the cross-reference table
    -- shall initially have generation numbers of 0.
       
  where
  warn s = warning $ "in first(base) xref table: " ++ s
  err  s = quit ("Error: in first(base) xref table: " ++ s)

warning :: String -> IO ()
warning s = putStrLn $ "Warning: " ++ s

quit :: String -> IO a
quit msg = do hPutStrLn stderr msg
              exitFailure

---- parsing when no Object Index ... ---------------------------------------------------------------

-- a new exception
data DerefException = DerefException
                      deriving Show
instance Exception DerefException

errorIfDomDependentParser :: String -> a
errorIfDomDependentParser m = error $ unwords ["DomDependentParsers not supported: (",m,")"]


runParserWithoutObjectIndexFailOnRef
  :: DbgMode => String -> Input -> Parser a -> IO (PdfResult a)
runParserWithoutObjectIndexFailOnRef msg i p =
  runParser (errorIfDomDependentParser msg)  Nothing p i
  -- error if the parser should attempt to deref any objects.
  
runParserWithoutObjectIndex :: DbgMode => Input -> Parser a -> IO (Maybe (PdfResult a))
runParserWithoutObjectIndex i p =
  do
  x <- try (runParser (throw DerefException) Nothing p i)
       -- catch if the parser should deref any objects!
  case x of
    Left DerefException -> pure Nothing
    Right x             -> pure $ Just x

---- utilities ---------------------------------------------------------------

getXRefStart :: TrailerEnd -> UInt 64
getXRefStart x = getField @"xrefStart" x

getEndOfTrailerEnd :: TrailerEnd -> UInt 64
getEndOfTrailerEnd x = getField @"offset4" x

---- xref table: parse and construct -----------------------------------------

-- | Construct the xref map (and etc), version 2
parseXRefsVersion2 :: DbgMode => Input -> FileOffset -> IO (PdfResult ([IncUpdate], ObjIndex, TrailerDict))
parseXRefsVersion2 inp offset =
  runParserWithoutObjectIndexFailOnRef
    "dereferencing object index during parsing [parseXRefsVersion2]"
    inp
    $
    do
    updates <- parseAllIncUpdates inp offset
    
    -- create object index (oi) map:
    oi <- foldlM
            (\oi iu-> do oi' <- convertSubSectionsToObjMap (iu_xrefs iu)
                         return (Map.union oi' oi)
                                -- NOTE: Map.union is left-biased.
            )
            Map.empty
            updates
    pure (updates, oi, iu_trailer(last updates))

    -- updates == [base,upd1,upd2,...]
    -- length(updates) >= 1
    -- FIXME[F2]: validate that trailers are consistent

  where
    
  convertSubSectionsToObjMap :: [[XRefEntry]] -> Parser ObjIndex
  convertSubSectionsToObjMap xrefss =
    do objMaps  <- mapM xrefEntriesToMap xrefss
       let objMap = Map.unions objMaps
       unless (Map.size objMap == sum (map Map.size objMaps))
           (pError FromUser "convertSubSectionsToObjMap"
                   "Duplicate entries in xref section")
       pure objMap


---- parsing IncUpdates ------------------------------------------------------

parseAllIncUpdates :: Input -> FileOffset -> Parser [IncUpdate]
parseAllIncUpdates inp offset0 =
  do
  (x, next) <- parseOneIncUpdate inp offset0
  case next of
    Just offset -> (++[x]) <$> parseAllIncUpdates inp offset
    Nothing     -> return [x]
  
-- | parseOneIncUpdate - go to offset and parse xref table
parseOneIncUpdate :: Input -> FileOffset -> Parser (IncUpdate, Maybe FileOffset)
parseOneIncUpdate inp offset = 
  case advanceBy offset inp of
    Just i ->
      do pSetInput i
         refSec <- pCrossRef  -- slight misnomer, this parses
                              --   - standard xref table OR xref streams
                              --   - the trailer too (in the former case)
         case refSec of
           CrossRef_oldXref x -> processTrailer XRefTable  x
           CrossRef_newXref x -> processTrailer XRefStream x
    Nothing -> pError FromUser "parseOneIncUpdate'"
                 ("Offset out of bounds: " ++ show offset)

  where

  processTrailer :: ( VecElem s
                    , HasField "trailer" x TrailerDict
                    , HasField "xref"    x (Vector s)
                    , XRefSection s e o
                    ) => XRefType -> x -> Parser (IncUpdate, Maybe FileOffset)
  processTrailer xrefType x =
    do let t = getField @"trailer" x
       prev <- case getField @"prev" t of
                 Nothing -> pure Nothing
                 Just i ->
                    case toInt i of
                      Nothing  -> pError FromUser "parseTrailer"
                                                  "Prev offset too large."
                      Just offset' ->
                          let offset'' = intToSize offset' in
                          if offset'' /= offset then
                            pure (Just offset'')
                          else
                            pError FromUser "parseTrailer"
                                            "Prev offset unchanged"
                          -- FIXME: infinite loop possible!
                          -- FIXME: TODO: change to allow for warnings.
                          {-
                          -- and this is not a fix, thanks to Linearized files (?)
                           if offset'' < offset then
                             pure (Just offset'')
                           else
                             pError FromUser "parseTrailer"
                               (unwords ["Prev offset", show offset''
                                        ,"does not precede offset", show offset
                                        ,"in file."
                                        ])
                           -}
                           
       xrefss <- mapM convertToXRefEntries (toList (getField @"xref" x))
       te <- pTrailerEnd
       -- FIXME: ensure 'te' consistent with ...
       return ( IU{ iu_type      = xrefType
                  , iu_xrefs     = xrefss
                  , iu_trailer   = t
                  , iu_startxref = te
                  }
              , prev
              )


---- report ------------------------------------------------------------------

printIncUpdateReport :: [IncUpdate] -> IO ()
printIncUpdateReport updates =
  do
  let us = zip ("initial DOM" : map (\n->"incremental update " ++ show (n::Int)) [1..])
               updates 
  forM_ us $
    \(nm,iu)->
      do
      mapM_ putStrLn [ nm ++ ":"
                     , "  " ++ render(ppXRefType (iu_type iu))
                     , "  starts at byte offset "
                          ++ show (sizeToInt $ getXRefStart $ iu_startxref iu)
                     , "  xref entries:"
                     ]
      printXRefs 4 (iu_xrefs iu)
      putStrLn "  trailer dictionary:"
      print (nest 4 $ pp (iu_trailer iu))


printXRefs :: Int -> [[XRefEntry]] -> IO ()
printXRefs indent ess =
  print
  $ nest indent
  $ ppBlock "[" "]"
     [ ppBlock "[" "]" (map ppXRefEntry es) | es <- ess]
  
-- | printObjIndex - NOTE prints the Map/Index abstraction (not the entries)
printObjIndex :: Int -> ObjIndex -> IO ()
printObjIndex indent oi = print
                          $ nest indent
                          $ ppBlock "[" "]" (map ppXRef (Map.toList oi))


---- cavities report ---------------------------------------------------------

type Range = (Int,Int)

printCavityReport :: FileOffset -> Input -> [IncUpdate] -> IO ()
printCavityReport bodyStart_base inp updates =
  do
  let us = zip3 ("base DOM" : map (\n->"incremental update " ++ show (n::Int)) [1..])
                updates
                (sizeToInt bodyStart_base
                 : map (sizeToInt . getEndOfTrailerEnd . iu_startxref) updates)
                
      reportOneUpdate (numC, totalSizeC) (nm,iu,bodyStart) =
        do
        let xrefStart = sizeToInt $ getXRefStart $ iu_startxref iu
        mapM_ putStrLn [ nm ++ ":"
                       , "  " ++ render (ppXRefType (iu_type iu))
                       , "  body starts at byte offset " ++ show bodyStart
                       , "  xref starts at byte offset " ++ show xrefStart
                       ]
        es <- getObjectRanges inp iu
        let (errors,ranges) = partitionEithers es
        if not (null errors) then
          do
          putStrLn "  cavities are uncomputable due to object parsing errors:"
          mapM_ (mapM_ (\s->putStr "    " >> putStrLn s >> putChar '\n')) errors
          putChar '\n'
          return (numC,totalSizeC) -- unchanged
        else
          do
          let sr = RIntSet.singletonRange

              -- intersection:
              i = foldr (\x s-> RIntSet.intersection (sr x) s)
                        RIntSet.empty
                        ranges
            
              -- cavities:
              cs = RIntSet.toRangeList $
                       sr (bodyStart, xrefStart - 1)
                       RIntSet.\\
                       foldr RIntSet.insertRange RIntSet.empty ranges

          case cs of
            [] -> putStrLn "  cavities: NONE."
            _  -> do
                  putStrLn "  cavities:"
                  mapM_ (\r->putStrLn ("    " ++ ppCavity r)) cs

          unless (RIntSet.null i) $
            warning $
              "object definitions overlap (highly suspicious) on the following offsets: "
              ++ show (RIntSet.toRangeList i)

          putChar '\n'
          return ( numC + length cs :: Int
                 , totalSizeC + sum (map sizeC cs) :: Int
                 )

  (numCavities,totalSizeCavities) <- foldlM reportOneUpdate (0,0) us
  putStrLn $ "Total number of cavities: "   ++ show numCavities
  putStrLn $ "Total size of all cavities: " ++ show totalSizeCavities

  where
  sizeC (start, end) = end - start + 1

  -- FIXME[F1]: we are accidentally including the bytes from "xref\n" to "%%EOF"
  --  - must nab the locations when we parse these!

ppCavity :: (Int,Int) -> String
ppCavity (start,end) = unwords [ show start
                               , "--"
                               , show end
                               , "(" ++ show (end - start + 1) ++ " bytes)"
                               ]

-- | Left s - corresponds to parser failure
getObjectRanges :: Input -> IncUpdate -> IO [Either [String] Range]
getObjectRanges inp iu =
  do
  sequence [getObjectAt off r | InUse r (InFileAt off) <- concat (iu_xrefs iu)]

  where
  getObjectAt off r = 
    do
    mres <- runParserWithoutObjectIndex
              inp
              (parseObjectAt inp off)
    case mres of
      Nothing  -> fail' ["a Dom-Dependent parser [unsupported]"]
      Just res -> case res of
        ParseOk (rng, TopDecl o g _x) ->
            do let o' = fromIntegral (refObj r)
                   g' = fromIntegral (refGen r)
               unless (o' == o && g' == g) $
                 warning "xref object gen =/= object gen obj ... endobj"
               return $ Right rng
               -- FIXME[C2]: '_x' is dead, use?
        ParseErr e                  -> fail' ["unparseable:", show e]
        ParseAmbig {}               -> fail' ["ambiguous."]

    where
    fail' []     = error "fail'"
    fail' (s:ss) = return $ Left $
                       unwords ["object (", show r, ") at offset", show off, "is", s]
                     : map ("  "++) ss
           
parseObjectAt :: Input -> Int -> Parser (Range, TopDecl)
parseObjectAt inp offsetStart =
  case advanceBy (intToSize offsetStart) inp of
    Nothing -> pError FromUser "parseObjectAt"
                ("Offset out of bounds: " ++ show offsetStart)
    Just i ->
      do pSetInput i
         td <- pTopDecl
         offsetEnd <- pOffset               
         return $ ((offsetStart, sizeToInt offsetEnd), td)


---- xref table: parse and construct (old version) ---------------------------

-- | Construct the xref table, version 1

parseXRefsVersion1 :: DbgMode => Input -> FileOffset -> IO (PdfResult (ObjIndex, TrailerDict))
parseXRefsVersion1 inp off0 = runParser Map.empty Nothing (go Nothing (Just off0)) inp
  where
  go :: Maybe TrailerDict -> Maybe FileOffset -> Parser (ObjIndex, TrailerDict)
  go mbRoot Nothing =
    do oix <- getObjIndex
       case mbRoot of
         Just r -> return (oix, r)
         Nothing -> pError FromUser "parseXRefsVersion1.go" "Missing document root."

  go mbRoot (Just offset) =
    case advanceBy offset inp of
      Just i ->
        do pSetInput i
           pManyWS  -- FIXME: later: warning if this consumes anything
           refSec <- pCrossRef
           case refSec of
             CrossRef_oldXref x -> goWith mbRoot x
             CrossRef_newXref x -> goWith mbRoot x
      Nothing -> pError FromUser "parseXRefsVersion1.go"
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
                      Nothing -> pError FromUser "parseXRefsVersion1.goWith(1)"
                                                 "Prev offset too large."
                      Just off -> pure (Just $ intToSize off)

       tabs <- mapM (\s->convertToXRefEntries s >>= xrefEntriesToMap)
                    (toList (getField @"xref" x))
       let entries = Map.unions tabs
       unless (Map.size entries == sum (map Map.size tabs))
         (pError FromUser "parseXRefsVersion1.goWith(2)" "Duplicate entries in xref section")

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
  processEntry :: Int -> t -> Parser XRefEntry

data XRefEntry = InUse R ObjLoc
               | Free  Int R     -- object is next free object, generation
               | Null    

ppXRefEntry :: XRefEntry -> Doc
ppXRefEntry x =
  case x of
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
convertToXRefEntries :: XRefSection s e o => s -> Parser [XRefEntry]
convertToXRefEntries xrs =
  forM (zip [ getField @"firstId" xrs .. ]
             (toList (getField @"entries" xrs))) $
       \(n,e)->     
          do o <- integerToInt n
             processEntry o e

-- | Create an Obj Index Map from the XRefEntry list
xrefEntriesToMap :: [XRefEntry] -> Parser ObjIndex
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
                  pError FromUser "xrefEntriesToMap.entry"
                                  ("Multiple entries for " ++ show ref)

integerToInt :: Integer -> Parser Int
integerToInt i =
  case toInt i of
    Nothing -> pError FromUser "integerToInt" "Integer constant too large."
    Just x  -> pure x


--------------------------------------------------------------------------------
-- FIXME: dead code follows

parseFinalTrailerEnd :: Input -> BS.ByteString -> IO (PdfResult TrailerEnd)
parseFinalTrailerEnd inp bs =
  do
  offset <- case locationOf_startxref bs of
              Left s  -> quit s
              Right x -> return x
  case advanceBy offset inp of
    Nothing -> quit ("startxref offset out of bounds: " ++ show offset)    
    Just inp' ->
        runParserWithoutObjectIndexFailOnRef
            "dereferencing object index during parsing [parseFinalTrailerEnd]"
            inp'
            (do pSetInput inp'
                pTrailerEnd)


-- find the file byte offset of where we find (last) 'startxref'
locationOf_startxref :: BS.ByteString -> Either String FileOffset
locationOf_startxref bs =
  case BS.findSubstring (BS.reverse startxref) (BS.reverse lastChunk) of
    Nothing -> Left "Couldn't find startxref"
    Just i  -> Right (intToSize $ len - i - BS.length startxref)

  where
  len             = BS.length bs
  lastChunk       = BS.drop (len - magicNumber) bs
  startxref       = "startxref"
  magicNumber     = 500 -- stop searching after 500 bytes from EOF

  -- this does NOT parse or validate the EOF

---- ~ ad hoc: ---------------------------------------------------------------

-- Pretty gross
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



