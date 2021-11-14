{-# Language TypeApplications, DataKinds #-}
{-# Language FlexibleContexts, ConstraintKinds #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module IncUpdates
  ( parseXRefsVersion2
  , printObjIndex
  , printIncUpdateReport
  , printCavityReport
  , validateUpdates
  , allOrNoUpdates
  , fstFndLstAppld
  , fstAppldLstFnd
  , fromPdfResult
  )
  where

-- system:
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Either
import           Data.Foldable(foldlM)
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import qualified Data.Text.Lazy as Text
import           GHC.Records(HasField, getField)
import           Text.PrettyPrint

-- pkg linguistic-ordinals:
import qualified Text.Ordinal

-- pkg range-set-list:
import qualified Data.RangeSet.IntMap as RIntSet

-- local:
import Daedalus.Panic 
import RTS.Vector(Vector,toList,VecElem)
import RTS.Numeric
import RTS.Input(advanceBy)
import Logger

import PdfMonad
import PdfParser
import Stdlib(pManyWS)

import PdfPP
import Possibly
import XRef

---- types -------------------------------------------------------------------

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

---- utilities ---------------------------------------------------------------

getEndOfTrailerEnd :: TrailerEnd -> UInt 64
getEndOfTrailerEnd x = getField @"offset4" x


---- misc --------------------------------------------------------------------

fromPdfResult :: String -> PdfResult a -> Possibly a
fromPdfResult contextString r = 
  case r of
    ParseOk a     -> Right a
    ParseAmbig {} -> Left [msg ++ ": ambiguous."]
    ParseErr e    -> Left $ (msg ++ ":") : lines (render(pp e))
  where
  msg = "while " ++ contextString


---- XRef table: parse and construct from many updates -----------------------

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
      

getTrailerFromUpdates  :: Updates -> Possibly TrailerDict
getTrailerFromUpdates = fmap iu_trailer . fstFndLstAppld

-- | create object index map
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
       unless (Map.size objMap == sum (map Map.size objMaps)) $
         -- FIXME[F2]: print out the duplicates!
         -- let unionDup m1 m2 = (Map.union m1 m1, Map.intersect m1 m2)
         failP ["Duplicate entries between xref sections"]
       pure objMap


---- parsing IncUpdates ------------------------------------------------------

-- | Updates
--
--   Don't want update errors to be all or nothing:
--    updates ordered
--      - from the base-dom to the last update
--      - reverse order in which we process them
--      - from start of file to end of file (when ordered typically!)

data Updates = U_Success [IncUpdate]
             | U_Failure ErrorMsg [IncUpdate]
                 -- ^ the error will be the last processed.
               
-- fstFndLstAppld = last in file, first processed
fstFndLstAppld :: Updates -> Possibly IncUpdate
fstFndLstAppld =
  \case
    U_Failure e [] -> Left e
    U_Failure _ xs -> Right (last xs)
    U_Success []   -> panic "fstFndLstAppld: internal error" []
    U_Success xs   -> Right (last xs)
                   
fstAppldLstFnd :: Updates -> Possibly IncUpdate
fstAppldLstFnd =
  \case
    U_Failure e [] -> Left e
    U_Failure _ xs -> Right (head xs) -- N.B.: dangerous, what you want?
    U_Success []   -> panic "fstAppldLstFn: internal error" []
    U_Success xs   -> Right (head xs)
                   
allOrNoUpdates :: Updates -> Possibly [IncUpdate]
allOrNoUpdates = \case
                   U_Success xs  -> Right xs
                   U_Failure e _ -> Left e


-- | parseAllIncUpdates - return IncUpdates, head is base, last is the
--                        first-processed at EOF
parseAllIncUpdates :: Input -> FileOffset -> IO Updates
parseAllIncUpdates = parseAllIncUpdates' IntSet.empty

parseAllIncUpdates' :: IntSet.IntSet -> Input -> FileOffset -> IO Updates
parseAllIncUpdates' prevSet inp offset0 =
  do
  r <- parseOneIncUpdate prevSet inp offset0
       -- end of file, first-processed update

  case r of
    Left ms  -> return $ U_Failure ms []
    Right iu ->
        case getPrev iu of
          Left ms              -> return $ U_Failure ("getPrev:":ms) [iu] 
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
                   U_Failure e xs -> U_Failure e (xs++[x])
                    
  getPrev :: IncUpdate -> Possibly (Maybe FileOffset)
  getPrev IU{iu_trailer=t} =
    case getField @"prev" t of
      Nothing -> pure Nothing
      Just i ->
         case toInt i of
           Nothing      -> failP ["Prev offset too large to fit in Int"]
           Just offset' -> return (Just (intToSize offset'))

-- | parseOneIncUpdate - go to offset and parse xref table
parseOneIncUpdate :: PrevSet -> Input -> FileOffset -> IO (Possibly IncUpdate)
parseOneIncUpdate prevSet input0 offset =
  if sizeToInt offset `IntSet.member` prevSet then
    return $ Left
               [ "Recursive incremental updates:"
               , unwords
                   [ "attempt to process xref table at offset"
                   , show (sizeToInt offset)
                   , ", which has been processed already"]
               , "set of processed xref tables (offsets): " ++ show prevSet
               ]
  else
    case advanceBy offset input0 of
      Nothing ->
        return $ Left ["Offset out of bounds: " ++ show offset]
      Just input1 ->
        parseXRefTable input1   `bind_IOPossibly` \(xref,xrefEnd) ->
        parseTrailerEnd xrefEnd `bind_IOPossibly` \trailerEnd ->
        return $ case xref of
          CrossRef_oldXref x -> processTrailer XRefTable  x trailerEnd
          CrossRef_newXref x -> processTrailer XRefStream x trailerEnd

  where

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
         
---- additional validation ---------------------------------------------------
-- validation done in IO to allow for warns as well as errors.

validateUpdates :: (Updates, Possibly ObjIndex, Possibly TrailerDict) -> IO ()
validateUpdates (updates,_,_) =
  case updates of
    U_Failure {}    -> return ()
    U_Success []    -> panic "validateUpates: internal error" []
    U_Success (x:_) -> validateBase x
    -- FIXME[F2]: validate others to some degree, even in U_Failure

validateBase :: IncUpdate -> IO ()
validateBase iu =
  do
  let xrefss = iu_xrefs iu
  unless (length xrefss == 1) $
    logError' "must have only one subsection"
  let [xrefs] = xrefss -- works because of last

    -- Section 7.5.4: For a PDF file that has never been incrementally
    -- updated, the cross-reference section shall contain only one subsection,
    -- whose object numbering begins at 0.

  case xrefs of
    []                              -> logWarn' "must not be empty"
    Free 0 (R _ n) : _ | n == 65535 -> return ()
                       | n == 0     -> logWarn' "object 0 has generation 0 (should be 65535)"
    _                               -> logWarn' "first object must be object 0, free, generation 65535"

    -- The first entry in the table (object number 0) shall always be free and
    -- shall have a generation number of 65,535;

    -- [first entry] shall be the head of the linked list of free objects.
    -- The last free entry (the tail of the linked list) links back to object number 0.

  case [ g | InUse (R _ g) _ <- xrefs, g /= 0] of
    _:_ -> logWarn' "objects exist without generation number 0"
    []  -> return ()

    -- Except for object number 0, all objects in the cross-reference table
    -- shall initially have generation numbers of 0.
       
  where
  logWarn'  s = logWarn  ("in first (base) xref table: " ++ s)
  logError' s = logError ("in first (base) xref table: " ++ s)


---- report ------------------------------------------------------------------

printUpdateSummary :: Updates -> IO [(String, Possibly IncUpdate)]
printUpdateSummary updates =
  do
  let (fs,ss,gd,len) = case updates of
                         U_Success xs   -> ( [], xs, True , length xs)
                         U_Failure e xs -> ([e], xs, False, length xs + 1)
             
      incUpdateName n =
        (if gd then
           if n == 0 then "base, " else showOrdinal' n ++ " applied, "
         else
           "")
        ++ showOrdinal' (len-n) ++ " found"
        where
        showOrdinal' = Text.unpack . Text.Ordinal.showOrdinal 

  case updates of
    U_Success{} -> logInfo $
                     "Found base and "
                     ++ show (len-1) ++ " incremental update(s):"
    U_Failure{} -> logWarn $
                     "cannot follow & parse all updates, showing "
                     ++ show len ++ " partial results:"

  return $ zip
             (map (\i->"UPDATE: " ++ incUpdateName i) [0..])
             (map Left fs ++ map Right ss)

    
printIncUpdateReport :: Updates -> IO ()
printIncUpdateReport updates =
  do
  us <- printUpdateSummary updates
  forM_ us $
    \(nm,x)->
      do
      case x of
        Left msg ->
            mapM_ putStrLn $
              [ nm ++ ":"
              , "  ERROR parsing update:"
              ]
              ++ map ("  "++) msg
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
      putChar '\n'

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
  us <- printUpdateSummary updates
  (numCavities,totalSizeCavities) <-
    foldlM (printCavityIncUpdate' input) (0,0)
      $ zip us
            (bodyStart_base : map getEndOfUpdate (rights $ map snd us))
             -- FIXME[F1]: assumes updates are ordered from end to start!
             --  - which is not necessarily the case.
    
  putStrLn $ "Total number of cavities: "   ++ show numCavities
  putStrLn $ "Total size of all cavities: " ++ show totalSizeCavities

  -- FIXME[F1]: we are accidentally including the bytes from "xref\n" to
  --            "%%EOF"
  --            - must nab the locations when we parse these!

  where
  printCavityIncUpdate' ::
       Input
    -> (Int, Int)
    -> ((String, Either [String] IncUpdate), FileOffset)
    -> IO (Int, Int)
  printCavityIncUpdate' i x ((nm, e), bodyStart') =
    case e of
      Right u -> printCavityIncUpdate i x (nm, u, bodyStart')
      Left ss -> do
                 mapM_ putStrLn $
                   [ nm ++ ":"
                   , "  body starts at byte offset " ++ show bodyStart'
                     ++ " (assumption)"
                   ]
                 mapM_ putStrLn $ "  ERROR parsing update:"
                                  : map ("    "++) ss
                 putChar '\n'
                 return x  -- OK?
  
  getEndOfUpdate IU{iu_trailerEnd=te} =
    case te of
      Right te' -> getEndOfTrailerEnd te'
      Left  o   -> o


printCavityIncUpdate :: Input
                     -> (Int, Int)
                     -> (String, IncUpdate, FileOffset)
                     -> IO (Int, Int)
printCavityIncUpdate input (numC, totalSizeC) (nm,iu,bodyStart') =
  do
  let xrefStart = sizeToInt $ iu_offset iu
      bodyStart = sizeToInt bodyStart'
  mapM_ putStrLn [ nm ++ ":"
                 , "  " ++ render (ppXRefType (iu_type iu))
                 , "  body starts at byte offset " ++ show bodyStart
                   ++ " (assumption)"
                 , "  xref starts at byte offset " ++ show xrefStart
                 ]
  -- FIXME[F2]: want to warn when no valid trailerEnd
  es <- getObjectRanges input iu
  let (errors,ranges) = partitionEithers es
  if not (null errors) then
    do
    putStrLn "  cavities are uncomputable due to object parsing errors:"
    mapM_
       (\ss-> mapM_ (\s->putStr "    " >> putStrLn s) ss
              >> putChar '\n')
       errors
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
      logWarn $
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
                 logWarn "xref object gen =/= object gen obj ... endobj"
               return $ Right rng
               -- FIXME[C2]: '_x' is dead, use?
        ParseErr e                  -> let ss = lines(render (pp e)) in
                                       fail' ("unparseable:" : ss)
        ParseAmbig {}               -> fail' ["ambiguous."]

    where
    fail' []     = panic "fail'" []
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


---- utilities: currently dead -----------------------------------------------
-- FIXME[CR]: remove or use

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


getXRefStart :: TrailerEnd -> UInt 64
getXRefStart x = getField @"xrefStart" x
  -- dead now, later use for further validation

