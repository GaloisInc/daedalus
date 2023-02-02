{-# Language RecordWildCards, OverloadedStrings, BlockArguments #-}
module RTS.ParseError where

import Data.List(transpose,sortBy)
import System.FilePath
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import Control.Exception(Exception)
import Data.Typeable(Typeable)
import Text.PrettyPrint
import Data.ByteString.Short(fromShort)

import Daedalus.RTS.HasInputs
import Daedalus.RTS.InputTrace
import Daedalus.RTS.Input
import Daedalus.RTS.JSON

data ParseErrorG e =
  PE { peInput   :: !Input
     , peStack   :: ![e]
     , peGrammar :: ![SourceRange]
     , peMsg     :: !String
     , peSource  :: !ParseErrorSource
     , peMore    :: ![ParseErrorG e]
     , peNumber  :: !Int
     , peITrace  :: !InputTrace
     } deriving Show

data ParseErrorSource = FromUser | FromSystem
  deriving Show

-- | Do we want to report only one or multiple errors.
data ErrorStyle = SingleError | MultiError

peOffset :: ParseErrorG a -> Int
peOffset = inputOffset . peInput

instance (Show e, Typeable e) => Exception (ParseErrorG e)

-- | Collect all errors, sorted by "niceness"
parseErrorToList :: ParseErrorG e -> [ParseErrorG e]
parseErrorToList = sortBy compareParseErrors . cvt
  where
  cvt pe = pe { peMore = [] } : concatMap cvt (peMore pe)



--------------------------------------------------------------------------------
-- Source Locations
--------------------------------------------------------------------------------

data SourceRange = SourceRange
  { srcFrom, srcTo :: SourcePos
  } deriving (Show,Eq,Ord)

data SourcePos = SourcePos { srcLine, srcCol :: Int, srcName :: String }
  deriving (Show,Eq,Ord)


class HasSourcePaths a where
  getSourcePaths :: a -> Set FilePath
  mapSourcePaths :: (FilePath -> FilePath) -> a -> a

instance HasSourcePaths SourcePos where
  getSourcePaths x   = Set.singleton (srcName x)
  mapSourcePaths f x = x { srcName = f (srcName x) }

instance HasSourcePaths SourceRange where
  getSourcePaths x = getSourcePaths (srcFrom x, srcTo x)
  mapSourcePaths f x = SourceRange { srcFrom = mapSourcePaths f (srcFrom x)
                                   , srcTo   = mapSourcePaths f (srcTo x)
                                   }

instance (HasSourcePaths a, HasSourcePaths b) => HasSourcePaths (a,b) where
  getSourcePaths (a,b) = Set.union (getSourcePaths a) (getSourcePaths b)
  mapSourcePaths f (a,b) = (mapSourcePaths f a, mapSourcePaths f b)

instance HasSourcePaths a => HasSourcePaths [a] where
  getSourcePaths   = Set.unions . map getSourcePaths
  mapSourcePaths f = fmap (mapSourcePaths f)

instance HasSourcePaths a => HasSourcePaths (Maybe a) where
  getSourcePaths   = maybe Set.empty getSourcePaths
  mapSourcePaths f = fmap (mapSourcePaths f)

instance HasSourcePaths e => HasSourcePaths (ParseErrorG e) where
  getSourcePaths err = getSourcePaths (peStack err, (peGrammar err, peMore err))
  mapSourcePaths f err =
    err { peStack   = mapSourcePaths f (peStack err)
        , peGrammar = mapSourcePaths f (peGrammar err)
        , peMore    = mapSourcePaths f (peMore err)
        }

normalizePaths :: HasSourcePaths a => a -> a
normalizePaths e = mapSourcePaths (normalizePathFun (getSourcePaths e)) e

normalizePathFun :: Set FilePath -> FilePath -> FilePath
normalizePathFun ps = joinPath . drop common . splitDirectories
  where
  common = length $ takeWhile allSame
                  $ transpose
                  $ map splitDirectories
                  $ Set.toList ps

  len = Set.size ps
  allSame xs =
    length xs == len &&
    case xs of
      []       -> True
      y : more -> all (== y) more

instance (HasInputs e) => HasInputs (ParseErrorG e) where
  getInputs pe = Map.unions [ getInputs (peITrace pe)
                            , getInputs (peStack pe)
                            , getInputs (peMore pe)
                            ]


--------------------------------------------------------------------------------
-- Merging errors
--------------------------------------------------------------------------------

joinSingleError :: ParseErrorG e -> ParseErrorG e -> ParseErrorG e
joinSingleError p1 p2 = fst (preferFirst p1 p2)

-- | Just store all errors without consideration for which one is better.
mergeError :: ParseErrorG e -> ParseErrorG e -> ParseErrorG e
mergeError p1 p2 = p1 { peMore = p2 : peMore p1 }

orderMultiError :: ParseErrorG e -> ParseErrorG e
orderMultiError pe =
  case parseErrorToList pe of
    x : xs -> x { peMore = xs }
    []     -> error "orderMultiError: No error!"

-- | "Better" errors are "smaller"
compareParseErrors :: ParseErrorG e -> ParseErrorG e -> Ordering
compareParseErrors p1 p2 =
  case (peSource p1, peSource p2) of
    (FromUser,FromSystem) -> LT
    (FromSystem,FromUser) -> GT
    _ -> case compare (peOffset p1) (peOffset p2) of
           LT -> GT
           EQ -> EQ
           GT -> LT

-- | Given two errors put the one we prefer in the first component of the result
preferFirst ::
  ParseErrorG e -> ParseErrorG e -> (ParseErrorG e, ParseErrorG e)
preferFirst p1 p2 =
  case compareParseErrors p1 p2 of
    GT -> (p2,p1)
    _  -> (p1,p2)



-------------------------------------------------------------------------------
-- Annotations
--------------------------------------------------------------------------------

class ToJSON a => IsAnnotation a where
  ppAnnot          :: a -> Doc


--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

ppParseError :: (IsAnnotation e) => ParseErrorG e -> Doc
ppParseError = vcat . map ppParseError1 . parseErrorToList

ppParseError1 :: (IsAnnotation e) => ParseErrorG e -> Doc
ppParseError1 pe@PE { .. } =
  brackets ("offset:" <+> int (peOffset pe)) $$
  nest 2 (bullets
           [ text peMsg, gram
           , "context:" $$ nest 2 (bullets (reverse (map ppAnnot peStack)))
           , "input trace:" $$ nest 2 (ppInputTrace peITrace)
           ]
         )
  where
  gram = case peGrammar of
           [] -> empty
           _  -> "see grammar at:" <+> commaSep (map ppSourceRange peGrammar)

  bullet      = if True then "•" else "*"
  buletItem d = bullet <+> d
  bullets ds  = vcat (map buletItem ds)
  commaSep ds = hsep (punctuate comma ds)

ppSourceRange :: SourceRange -> Doc
ppSourceRange r =
  hcat [ posLong (srcFrom r), "--"
       , if srcName (srcFrom r) == srcName (srcTo r)
          then posShort (srcTo r)
          else posLong (srcTo r)
       ]
  where
  posShort p = hcat [ int (srcLine p), ":", int (srcCol p) ]
  posLong p
    | null (srcName p) = hcat [ int (srcLine p), ":", int (srcCol p) ]
    | otherwise =
      hcat [ text (srcName p), ":", int (srcLine p), ":", int (srcCol p) ]


--------------------------------------------------------------------------------
-- JSON
--------------------------------------------------------------------------------

jsToDoc :: ToJSON a => a -> Doc
jsToDoc = text . Text.unpack . Text.decodeUtf8 . jsonToBytes . toJSON

instance (HasInputs e, ToJSON e) => ToJSON (ParseErrorG e) where
  toJSON pe =
    jsObject
      [ ("error",   jsString (peMsg pe))
      , ("input",   toJSON (inputName (peInput pe)))
      , ("offset",  toJSON (inputOffset (peInput pe)))
      , ("grammar", toJSON (peGrammar pe))
      , ("stack",   toJSON (peStack pe))
      , ("trace",   toJSON (peITrace pe))
      , ("inputs",  jsObject [ (fromShort k, jsText v)
                             | (k,v) <- Map.toList (getInputs pe)
                             ])
      , ("more",   toJSON (peMore pe))
      ]

instance ToJSON SourceRange where
  toJSON p = jsObject [ ("from", toJSON (srcFrom p)), ("to", toJSON (srcTo p)) ]

instance ToJSON SourcePos where
  toJSON p =
    jsObject [ ("file", jsString (srcName p))
             , ("line", toJSON (srcLine p))
             , ("col", toJSON (srcCol p))
             ]



