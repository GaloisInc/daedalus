{-# Language ImplicitParams #-}
module Results where

import qualified Data.ByteString as BS
import Data.List.NonEmpty(toList)
import System.Console.ANSI
import Hexdump

import qualified RTS.Annot as RTS
import qualified RTS.ParseError as RTS
import qualified RTS.ParserAPI as RTS
import qualified Daedalus.RTS.JSON as RTS
import qualified Daedalus.RTS.HasInputs as RTS
import qualified Daedalus.RTS.Input as RTS

import Daedalus.PP
import Daedalus.Value
import Daedalus.Interp.ErrorTrie(parseErrorTrieToJSON)
import Daedalus.Interp.DebugAnnot

import CommandLine

dumpResult ::
  (?opts :: Options, GroupedErr e) => (a -> Doc) -> RTS.ResultG e a -> Doc
dumpResult ppVal r =
  case r of
     RTS.NoResults err -> dumpErr err
     RTS.Results as    -> dumpValues ppVal' (toList as)
  where
  ppVal' (a,x) = ppVal a -- $$ "----" $$ RTS.ppInputTrace x

dumpValues :: (?opts :: Options) => (a -> Doc) -> [a] -> Doc
dumpValues ppVal as
  | optShowJS ?opts = brackets (vcat $ punctuate comma $ map ppVal as)
  | otherwise =
    vcat [ "--- Found" <+> int (length as) <+> "results:"
         , vcat' (map ppVal as)
         ]

dumpInterpVal :: (?opts :: Options) => Value -> Doc
dumpInterpVal = if optShowJS ?opts then valueToJS else pp

dumpErr ::
  (?opts :: Options, GroupedErr e) =>
  RTS.ParseErrorG e -> Doc
dumpErr err
  | optDetailedErrors ?opts = RTS.jsToDoc (jsGrouped err)
  | optShowJS ?opts = RTS.jsToDoc (RTS.toJSON err)
  | otherwise =
    vcat
      [ "--- Parse error: "
      , text (show (RTS.ppParseError err))
      , "File context:"
      , text (prettyHexCfg cfg ctx)
      ]
  where
  ctxtAmt = 32
  bs      = RTS.inputTopBytes (RTS.peInput err)
  errLoc  = RTS.peOffset err
  start = max 0 (errLoc - ctxtAmt)
  end   = errLoc + 10
  len   = end - start
  ctx = BS.take len (BS.drop start bs)
  startErr =
     setSGRCode [ SetConsoleIntensity
                  BoldIntensity
                , SetColor Foreground Vivid Red ]
  endErr = setSGRCode [ Reset ]
  cfg = defaultCfg { startByte = start
                   , transformByte =
                      wrapRange startErr endErr
                                errLoc errLoc }

class (RTS.HasInputs e, RTS.IsAnnotation e) => GroupedErr e where
  jsGrouped :: RTS.ParseErrorG e -> RTS.JSON

instance GroupedErr DebugAnnot where
  jsGrouped = parseErrorTrieToJSON

instance GroupedErr RTS.Annotation where
  jsGrouped = RTS.toJSON


