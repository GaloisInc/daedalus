{-# Language TypeApplications, DataKinds #-}
{-# Language FlexibleContexts, ConstraintKinds #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
module XRef
  ( findStartXRef
  , parseXRefsVersion1
  , parseXRefsVersion2
  , printObjIndex
  , printIncUpdateReport
  , printCavityReport
  , validateUpdates
  , allOrNoUpdates
  , lastUpdate
  , fromPdfResult
  -- types:
  , FileOffset
  , Possibly
  )
  where

import           Control.Applicative((<|>))
import           Control.Exception
import           Control.Monad(unless,forM,forM_,foldM,when)
import qualified Data.ByteString.Char8 as BS
import           Data.Char(isSpace,isNumber)
import           Data.Either
import           Data.Foldable(foldlM)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import           GHC.Records(HasField, getField)
import           System.Exit(exitFailure)
import           System.IO(hPutStrLn,stdout)

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
type TrailerEnd' = Either
                     FileOffset -- where we expected to see the below
                     TrailerEnd -- found 'startxref ... %%EOF", this parsed

-- an incremental update (or the base DOM/update, at start of file)
data IncUpdate = IU { iu_offset     :: FileOffset
                    , iu_type       :: XRefType
                    , iu_xrefs      :: [[XRefEntry]]
                    , iu_trailer    :: TrailerDict
                    , iu_trailerEnd :: TrailerEnd'  -- last part, holds startxref offset
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

validateUpdates :: (Updates, Possibly ObjIndex, Possibly TrailerDict) -> IO ()
validateUpdates (updates,_,_) =
  case lastUpdate updates of
    Left _  -> return ()
    Right u -> validateBase u

-- FIXME: warn more, error less
-- FIXME: - label/categorize messages as not-PDF,NBCUR,info/warn,...
validateBase :: IncUpdate -> IO ()
validateBase iu =
  do
  let xrefss = iu_xrefs iu
  unless (length xrefss == 1) $
    quit' "must have only one subsection"

    -- Section 7.5.4: For a PDF file that has never been incrementally
    -- updated, the cross-reference section shall contain only one subsection,
    -- whose object numbering begins at 0.

  let [xrefs] = xrefss
  case xrefs of
    []                              -> quit' "must not be empty"
    Free 0 (R _ n) : _ | n == 65535 -> return ()
                       | n == 0     -> warn' "object 0 has generation 0 (should be 65535)"
    _                               -> quit' "first object must be object 0, free, generation 65535"

    -- The first entry in the table (object number 0) shall always be free and
    -- shall have a generation number of 65,535;

    -- [first entry] shall be the head of the linked list of free objects.
    -- The last free entry (the tail of the linked list) links back to object number 0.

  case [ g | InUse (R _ g) _ <- xrefs, g /= 0] of
    _:_ -> quit' "objects exist without generation number 0"
    []  -> return ()

    -- Except for object number 0, all objects in the cross-reference table
    -- shall initially have generation numbers of 0.
       
  where
  warn' s = warn $ "in first(base) xref table: " ++ s
  quit' s = quit ("Error: in first(base) xref table: " ++ s)


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
    error $ "panic: dereferencing object index should not happen here: " ++ contextMsg'
  -- FIXME[C2]

runParserWithoutObjectIndex :: DbgMode => Input -> Parser a -> IO (Maybe (PdfResult a))
runParserWithoutObjectIndex i p =
  do
  x <- try (runParser (throw DerefException) Nothing p i)
       -- catch if the parser should deref any objects!
  case x of
    Left DerefException -> pure Nothing
    Right x'            -> pure $ Just x'


---- Possibly ----------------------------------------------------------------

type Possibly a = Either [String] a
     -- FIXME[C]: hardly where this belongs, used by clients
     -- we often use term 'Fail' to refer to the 'Left' case
                
-- the bind for the IO(Possibly -) monad
-- (actually has more general type)
bind_IOPossibly ::
  IO (Possibly a) -> (a -> IO (Possibly b)) -> IO (Possibly b)
bind_IOPossibly ioA ioB =   
  do
  r1 <- ioA
  case r1 of
    Left s   -> return (Left s)
    Right v1 -> ioB v1

                  
---- utilities ---------------------------------------------------------------

-- | set of all the "Prev" offsets we have encountered
type PrevSet = IntSet.IntSet


getXRefStart :: TrailerEnd -> UInt 64
getXRefStart x = getField @"xrefStart" x
  -- dead now, later use for further validation
  
getEndOfTrailerEnd :: TrailerEnd -> UInt 64
getEndOfTrailerEnd x = getField @"offset4" x

fromPdfResult :: String -> PdfResult a -> Possibly a
fromPdfResult contextString r = 
 case r of
   ParseOk a     -> Right a
   ParseAmbig {} -> Left [msg ++ ": ambiguous."]
   ParseErr e    -> Left [msg ++ ":", [], show (pp e)]
  where
  msg = "while " ++ contextString

newError2 :: ParseErrorSource -> SourceRange -> String -> Possibly a
newError2 _ c msg = Left $ [concat [msg, " (", c, ")"]]

  -- FIXME: remove first arg in newError/newError2

warn :: String -> IO ()
warn s = putStrLn $ "Warning: " ++ s

quit :: String -> IO a
quit msg = do hPutStrLn stdout msg
              exitFailure


---- xref table: parse and construct from many updates -----------------------

-- | Construct the xref map (and etc), version 2
parseXRefsVersion2 :: DbgMode
                   => Input -> FileOffset
                   -> IO (Updates, Possibly ObjIndex, Possibly TrailerDict)
parseXRefsVersion2 inp offset =
    do
    updates <- parseAllIncUpdates inp offset

    let pObjectIndex = getObjIndexFromUpdates updates
    let pTrailer     = getTrailerFromUpdates updates
    return (updates, pObjectIndex, pTrailer)
      

-- Better
getTrailerFromUpdates  :: Updates -> Possibly TrailerDict
getTrailerFromUpdates = fmap iu_trailer . lastUpdate

-- create object index map
getObjIndexFromUpdates :: Updates -> Possibly ObjIndex
getObjIndexFromUpdates updates =
  do
  updates' <- allOrNoUpdates updates
              -- FIXME: extend error msg
  foldlM
    (\oi iu-> do oi' <- convertSubSectionsToObjMap (iu_xrefs iu)
                 return (Map.union oi' oi))
                         -- NOTE: Map.union is left-biased.
    Map.empty
    updates'
      
    -- updates == [base,upd1,upd2,...]
    -- length(updates) >= 1
    -- FIXME[F2]: validate that trailers are consistent

  where
    
  convertSubSectionsToObjMap :: [[XRefEntry]] -> Possibly ObjIndex
  convertSubSectionsToObjMap xrefss =
    do objMaps  <- mapM xrefEntriesToMap xrefss
       let objMap = Map.unions objMaps
       unless (Map.size objMap == sum (map Map.size objMaps))
           (newError2 FromUser "convertSubSectionsToObjMap"
                      "Duplicate entries in xref section")
       pure objMap


---- parsing IncUpdates ------------------------------------------------------

type ErrorMsg = [String]

-- Don't want errors here to be all or nothing:
data Updates = U_Success [IncUpdate]
             | U_Failure [IncUpdate] ErrorMsg

allOrNoUpdates :: Updates -> Possibly [IncUpdate]
allOrNoUpdates = \case
                   U_Success xs   -> Right xs
                   U_Failure _  e -> Left e

-- lastUpdate = last in file, first processed
lastUpdate :: Updates -> Possibly IncUpdate
lastUpdate = \case
                U_Failure [] e -> Left e
                U_Failure xs _ -> Right (last xs)
                U_Success []   -> error "lastUpdate: internal error"
                U_Success xs   -> Right (last xs)
                   

-- | parseAllIncUpdates - return IncUpdates, head is base, last is the first-processed at EOF
parseAllIncUpdates :: Input -> FileOffset -> IO Updates
parseAllIncUpdates = parseAllIncUpdates' IntSet.empty

parseAllIncUpdates' :: IntSet.IntSet -> Input -> FileOffset -> IO Updates
parseAllIncUpdates' prevSet inp offset0 =
  do
  r <- parseOneIncUpdate prevSet inp offset0
       -- end of file, first-processed update

  case r of
    Left ms  -> return $ U_Failure [] ms
    Right iu ->
        case getPrev iu of
          Left ms              -> return $ U_Failure [iu] ("getPrev:":ms)
          Right Nothing        -> return $ U_Success [iu]
          Right (Just offset1) -> do
                                  x <- parseAllIncUpdates'
                                         (IntSet.insert (sizeToInt offset0) prevSet)
                                         inp
                                         offset1
                                  return $ addUpdate iu x

  where
  addUpdate x = \case
                   U_Success xs   -> U_Success (xs++[x])
                   U_Failure xs e -> U_Failure (xs++[x]) e
                    
  getPrev :: IncUpdate -> Possibly (Maybe FileOffset)
  getPrev IU{iu_trailer=t} =
    case getField @"prev" t of
      Nothing -> pure Nothing
      Just i ->
         case toInt i of
           Nothing      -> newError2 FromUser "parseTrailer"
                                              "Prev offset too large to fit in Int"
           Just offset' -> return (Just (intToSize offset'))

-- | parseOneIncUpdate - go to offset and parse xref table
parseOneIncUpdate :: PrevSet -> Input -> FileOffset -> IO (Possibly IncUpdate)
parseOneIncUpdate prevSet input0 offset =
  if sizeToInt offset `IntSet.member` prevSet then
    fail' [unwords[ "recursive incremental updates:"
                  , "adding xref at "
                  , show offset, "where we already have", show prevSet]]
  else
    case advanceBy offset input0 of
      Nothing ->
        fail' ["Offset out of bounds: " ++ show offset]
      Just input1 ->
        parseXRefTable input1   `bind_IOPossibly` \(xref,xrefEnd) ->
        parseTrailerEnd xrefEnd `bind_IOPossibly` \trailerEnd ->
        return $ case xref of
          CrossRef_oldXref x -> processTrailer XRefTable  x trailerEnd
          CrossRef_newXref x -> processTrailer XRefStream x trailerEnd

  where
  fail' ss = return (Left ss)

  -- parseTrailerEnd:
  --   NOTE: The following parsing of TrailerEnd is where Version1 differs
  --         from Version2!
  --   FIXME[F1]: Are we overconstraining syntax?
  --   FIXME[F1]: on the first-found update, we don't really know if this
  --              trailerEnd is the same one we found at the end of the file!
  --   
  --   When do we need/want to parse the trailer end:
  --    - not to just create xref table
  --    - YES to compute cavities
  --    - YES to allow us to do some sanity checks
        
  parseTrailerEnd :: FileOffset -> IO (Possibly TrailerEnd')
  parseTrailerEnd xrefEnd =
    do
    let ctx = "parsing 'startxref' to '%%EOF'"
        Just input2 = advanceBy xrefEnd input0
        -- result of 'pOffset' must be good
        
    te <- runParserWithoutObjectIndexFailOnRef ctx input2 pTrailerEnd
    return $ Right $ case fromPdfResult ctx te of
                       Left _    -> Left xrefEnd
                       Right te' -> Right te'

  -- Parse the basic syntax of the xref table
  parseXRefTable :: Input -> IO (Possibly (CrossRef,FileOffset))
  parseXRefTable input1 =
    do
    let ctx = "parsing xref table (at byte offset "
              ++ show (sizeToInt offset) ++ ")"
    r <- runParserWithoutObjectIndexFailOnRef
           ctx
           input1
           (do
            pSetInput input1
            pManyWS     -- FIXME: later: warn if this consumes anything
            crossRef <- pCrossRef
            crossRefEnd <- pOffset
            return (crossRef, crossRefEnd))
    return $ fromPdfResult ctx r


  processTrailer :: ( VecElem s
                    , HasField "trailer" x TrailerDict
                    , HasField "xref"    x (Vector s)
                    , XRefSection s e o
                    ) => XRefType -> x -> TrailerEnd' -> Possibly IncUpdate
  processTrailer xrefType x trailerEnd =
    do let trailerDict = getField @"trailer" x
                           
       xrefss <- mapM convertToXRefEntries (toList (getField @"xref" x))
  -- FIXME[F3]: PDF requires that the trailers are consistent. check somewhere.
       return $ IU{ iu_offset    = offset
                  , iu_type      = xrefType
                  , iu_xrefs     = xrefss
                  , iu_trailer   = trailerDict
                  , iu_trailerEnd = trailerEnd
                  }
         
---- report ------------------------------------------------------------------

printIncUpdateReport :: Updates -> IO ()
printIncUpdateReport updates =
  do
  us' <- 
    do
    (us,es) <-
      case updates of
        U_Success xs   ->
            return (xs,[])
        U_Failure xs e ->
            do 
            warn "Error in parsing updates, showing only partial results:"
            return (xs, [e])
    return $
      zip
       ("initial DOM" :
         map (\n->"incremental update " ++ show (n::Int)) [1..])
       (map Right us ++ map Left es)
       
  forM_ us' $
    \(nm,x)->
      do
      case x of
        Left ss ->
            mapM_ putStrLn $
              [ nm ++ ":"
              , "  ERROR parsing update:"
              ]
              ++ map ("  "++) ss
        Right iu ->
            do
            mapM_ putStrLn [ nm ++ ":"
                           , "  " ++ render(ppXRefType (iu_type iu))
                           , "  starts at byte offset "
                                ++ show (sizeToInt $ iu_offset iu)
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

printCavityReport :: FileOffset -> Input -> Updates -> IO ()
printCavityReport bodyStart_base input updates =
  do
  (us,es) <-
    case updates of
      U_Success xs   ->
          return (xs,[])
      U_Failure xs e ->
          do 
          warn "Error in parsing updates, showing only partial results:"
          return (xs, [e])
           
  (numCavities,totalSizeCavities) <-
    foldlM (printOneUpdate' input) (0,0)
      $ zip3 ("base DOM" :
                map (\n->"incremental update " ++ show (n::Int)) [1..])
             (map Right us ++ map Left es)
             (bodyStart_base : map getEndOfUpdate us)
             -- FIXME[F1]: assumes updates are ordered from end to start!
             --  - which is not necessarily the case.
    
  putStrLn $ "Total number of cavities: "   ++ show numCavities
  putStrLn $ "Total size of all cavities: " ++ show totalSizeCavities

  -- FIXME[F1]: we are accidentally including the bytes from "xref\n" to
  --            "%%EOF"
  --            - must nab the locations when we parse these!

  where
  printOneUpdate' :: Input
                  -> (Int, Int)
                  -> (String, Either [String] IncUpdate, FileOffset)
                  -> IO (Int, Int)
  printOneUpdate' i x (nm, e, bodyStart') =
    case e of
      Right u -> printOneUpdate i x (nm, u, bodyStart')
      Left ss -> do
                 mapM_ putStrLn $
                   [ nm ++ ":"
                   , "  body starts at byte offset " ++ show bodyStart'
                   , "ERROR parsing update:"
                   ]
                   ++ map ("  "++) ss
                 return x  -- OK?
  
  getEndOfUpdate IU{iu_trailerEnd=te} =
    case te of
      Right te' -> getEndOfTrailerEnd te'
      Left  o   -> o


printOneUpdate :: Input
               -> (Int, Int)
               -> (String, IncUpdate, FileOffset)
               -> IO (Int, Int)
printOneUpdate input (numC, totalSizeC) (nm,iu,bodyStart') =
  do
  let xrefStart = sizeToInt $ iu_offset iu
      bodyStart = sizeToInt bodyStart'
  mapM_ putStrLn [ nm ++ ":"
                 , "  " ++ render (ppXRefType (iu_type iu))
                 , "  body starts at byte offset " ++ show bodyStart
                 , "  xref starts at byte offset " ++ show xrefStart
                 ]
  -- FIXME[F2]: want to warn when no valid trailerEnd
  es <- getObjectRanges input iu
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
      warn $
        "object definitions overlap (highly suspicious) on the following offsets: "
         ++ show (RIntSet.toRangeList i)

    putChar '\n'
    return ( numC + length cs :: Int
           , totalSizeC + sum (map sizeC cs) :: Int
           )

  where
  sizeC (start, end) = end - start + 1


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
                 warn "xref object gen =/= object gen obj ... endobj"
               return $ Right rng
               -- FIXME[C2]: '_x' is dead, use?
        ParseErr e                  -> fail' [ "unparseable:"
                                             , render (pp e)
                                             ]
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
         Nothing -> pError FromUser "parseXRefsVersion1.go" "Missing document root."

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
      Nothing -> pError FromUser "parseXRefsVersion1.go"
                  ("Offset out of bounds: " ++ show offset)

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
                Nothing  -> pError FromUser "parseXRefsVersion1.goWith(1)"
                                            "Prev offset too large."
                Just off -> do
                            -- ensure no infinite loop of incremental updates
                            when (off `IntSet.member` prevSet) $
                              pError FromUser "goWith"
                                $ unwords[ "recursive incremental updates:"
                                         , "adding", show off, "to", show prevSet]
                            pure (Just $ intToSize off)
                            -- FIXME: would be even cleaner if we used the offset of 'xref'

       tabs <- pErrorIfFail $
                 mapM (\s->convertToXRefEntries s >>= xrefEntriesToMap)
                      (toList (getField @"xref" x))
       let entries = Map.unions tabs
       unless (Map.size entries == sum (map Map.size tabs))
         (pError FromUser "parseXRefsVersion1.goWith(2)" "Duplicate entries in xref section")

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
                 Left ss -> pError FromUser "" (unlines ss)
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
                  newError2 FromUser "xrefEntriesToMap.entry"
                                     ("Multiple entries for " ++ show ref)

integerToInt :: Integer -> Possibly Int
integerToInt i =
  case toInt i of
    Nothing -> newError2 FromUser "integerToInt" "Integer constant too large."
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
            "parsing the final TrailerEnd"
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
  magicNumber     = 500 -- stop searching 500 bytes from EOF

  -- this does NOT parse or validate the EOF

---- little ad hoc: ---------------------------------------------------------------

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

