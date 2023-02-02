{-# Language ImplicitParams #-}
module Results where

import Control.Monad(forM_)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List.NonEmpty(toList)
import System.Console.ANSI
import System.IO(withBinaryFile, IOMode(WriteMode))
import qualified System.Directory as Dir
import System.FilePath((</>))
import Hexdump
import AlexTools

import qualified RTS.Annot as RTS
import qualified RTS.ParseError as RTS
import qualified RTS.ParserAPI as RTS
import qualified Daedalus.RTS.JSON as RTS
import qualified Daedalus.RTS.HasInputs as RTS
import qualified Daedalus.RTS.Input as RTS


import Daedalus.PP
import Daedalus.Parser.Lexer
import Daedalus.Value
import Daedalus.Interp.ErrorTrie(parseErrorTrieToJSON)
import Daedalus.Interp.DebugAnnot

import Templates
import CommandLine

-- | Show the results of running the interpreter.
-- Returns `True` if successful, or `False` on parse error
dumpResult ::
  (?opts :: Options, GroupedErr e) => (a -> Doc) -> RTS.ResultG e a -> Doc
dumpResult ppVal r =
  case r of
     RTS.NoResults err -> dumpErr err
     RTS.Results as    -> dumpValues ppVal' (toList as)
  where
  ppVal' (a,x) = ppVal a -- $$ "----" $$ RTS.ppInputTrace x

-- | Show some parsed values
dumpValues :: (?opts :: Options) => (a -> Doc) -> [a] -> Doc
dumpValues ppVal as
  | optShowJS ?opts = brackets (vcat $ punctuate comma $ map ppVal as)
  | otherwise =
    vcat [ "--- Found" <+> int (length as) <+> "results:"
         , vcat' (map ppVal as)
         ]

-- | Show the value of the interpreter either pretty printed or in JSON
dumpInterpVal :: (?opts :: Options) => Value -> Doc
dumpInterpVal = if optShowJS ?opts then valueToJS else pp

{- | Show the errors either pretty printed or in JSON
Note that the detailed error directory is handed in `saveDetailedError`,
not here. -}
dumpErr ::
  (?opts :: Options, GroupedErr e) =>
  RTS.ParseErrorG e -> Doc
dumpErr err
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

--------------------------------------------------------------------------------
-- Package up detailed errors

-- | Create an output directory with details about a failed parse.
saveDetailedError ::
  (?opts :: Options, GroupedErr e) => [FilePath] -> RTS.ParseErrorG e -> IO ()
saveDetailedError srcs err =
  case optDetailedErrors ?opts of
    Nothing -> pure ()
    Just dir ->
      do Dir.createDirectoryIfMissing True dir
         doFiles dir srcs
         doDetailedErr dir err
         forM_ error_viewer_files \(name,bytes) ->
            BS.writeFile (dir </> name) bytes



--------------------------------------------------------------------------------
-- Make error file

doDetailedErr :: (GroupedErr e) => FilePath -> RTS.ParseErrorG e -> IO ()
doDetailedErr outDir err =
  do let outFile = outDir </> "parse_error.js"
     withBinaryFile outFile WriteMode \h ->
       do BS8.hPutStrLn h "const parseError ="
          BS8.hPutStrLn h (RTS.jsonToBytes (jsGrouped err))

class (RTS.HasInputs e, RTS.IsAnnotation e) => GroupedErr e where
  jsGrouped :: RTS.ParseErrorG e -> RTS.JSON

instance GroupedErr DebugAnnot where
  jsGrouped = parseErrorTrieToJSON

instance GroupedErr RTS.Annotation where
  jsGrouped = RTS.toJSON




--------------------------------------------------------------------------------
-- Syntax highlighting

-- | Save syntax highlighted source files in `source_files.json`
doFiles :: FilePath -> [FilePath] -> IO ()
doFiles outDir filePaths =
  do fs <- mapM doFile filePaths
     let norm = RTS.normalizePathFun (Set.fromList (map fst fs))
         key  = Text.encodeUtf8 . Text.pack . norm
     let mp = RTS.jsObject [ (key f, j) | (f,j) <- fs ]

     let outFile = outDir </> "source_files.js"
     withBinaryFile outFile WriteMode \h ->
       do BS8.hPutStrLn h "const files ="
          BS8.hPutStrLn h (RTS.jsonToBytes mp)

-- | Synatx highlighting for a file.
doFile :: FilePath -> IO (FilePath, RTS.JSON)
doFile file =
  do txt <- Text.readFile file
     let nm = Text.pack file
     let ts = lexer nm txt
     a <- Dir.canonicalizePath file
     pure ( a
          , RTS.jsObject [ ("text",   RTS.toJSON txt)
                         , ("syntax", jsLexemes ts)
                         ]
          )

jsLexemes :: [Lexeme Token] -> RTS.JSON
jsLexemes ls =
  RTS.jsObject
    [ (BS8.pack k, RTS.jsArray v)
    | (k,v) <- Map.toList $ Map.delete "" $ Map.fromListWith (++) $ map toMap ls
    ]
  where
  rng p   = RTS.jsArray [ RTS.toJSON (sourceLine p)
                        , RTS.toJSON (sourceColumn p) ]
  toMap l =
    let t = lexemeToken l
        r = lexemeRange l
    in (tokenClass t, [ RTS.jsArray [ rng (sourceFrom r), rng (sourceTo r) ]])


type Class = String

clIdent, clLiteral, clPunct, clOp, clKW, clType, clNone :: Class
clIdent   = "identifier"
clLiteral = "literal"
clPunct   = "punctuation"
clOp      = "operator"
clKW      = "keyword"
clType    = "type"
clNone    = ""

tokenClass :: Token -> Class
tokenClass tok =
  case tok of
    BigIdent            -> clIdent
    SmallIdent          -> clIdent
    SetIdent            -> clIdent
    SmallIdentI         -> clIdent
    BigIdentI           -> clIdent
    SetIdentI           -> clIdent
    Number  {}          -> clLiteral
    Bytes   {}          -> clLiteral
    Byte    {}          -> clLiteral

    OpenBrace           -> clPunct
    CloseBrace          -> clPunct
    OpenBraceBar        -> clPunct
    CloseBraceBar       -> clPunct
    OpenParen           -> clPunct
    CloseParen          -> clPunct
    OpenBracket         -> clPunct
    CloseBracket        -> clPunct
    OpenTri             -> clPunct
    CloseTri            -> clPunct

    VOpen               -> clNone
    VSemi               -> clNone
    VClose              -> clNone

    Semi                -> clPunct
    Colon               -> clPunct
    Dot                 -> clPunct
    DotDot              -> clPunct
    Comma               -> clPunct
    AtSign              -> clPunct
    Equals              -> clPunct

    DoubleEquals        -> clOp
    BangEquals          -> clOp
    Bang                -> clOp
    Hat                 -> clOp
    Bar                 -> clOp
    DotBarDot           -> clOp
    DotAmpDot           -> clOp
    DotHatDot           -> clOp
    BarBar              -> clOp
    AmpAmp              -> clOp
    LtBar               -> clOp
    Dollar              -> clPunct
    DollarDollar        -> clPunct
    Plus                -> clOp
    Minus               -> clOp
    Star                -> clOp
    ForwardSlash        -> clOp
    Percent             -> clOp
    TokLeq              -> clOp
    TokGeq              -> clOp
    Hash                -> clOp
    LeftHash            -> clOp
    ShiftL              -> clOp
    ShiftR              -> clOp
    RightArrow          -> clPunct
    Underscore          -> clPunct

    BitwiseComplementT    -> clOp

    KWstruct              -> clKW
    KWunion               -> clKW
    KWChoose              -> clKW
    KWFirst               -> clKW
    KWAccept              -> clKW
    KWblock               -> clKW
    KWlet                 -> clKW
    KWTry                 -> clKW
    KWMatch               -> clKW
    KWMany                -> clKW
    KWManyQuestion        -> clKW
    KWmany                -> clKW
    KWmanyQuestion        -> clKW
    KWOptional            -> clKW
    KWOptionalQuestion    -> clKW
    KWUInt8               -> clKW

    KWTrue                -> clKW -- or literal?
    KWFalse               -> clKW -- or literal?

    KWFor                 -> clKW
    KWMap                 -> clKW
    KWIn                  -> clKW
    KWIs                  -> clKW
    KWOf                  -> clKW

    KWInt                 -> clType
    KWUInt                -> clType
    KWSInt                -> clType
    KWFloat               -> clType
    KWDouble              -> clType
    KWBool                -> clType
    KWMaybe               -> clType
    KWStream              -> clType

    KWIf                  -> clKW
    KWThen                -> clKW
    KWElse                -> clKW
    KWImport              -> clKW
    KWExtern              -> clKW
    KWAs                  -> clKW
    KWAsBang              -> clKW
    KWAsQuestion          -> clKW

    KWConcat              -> clIdent
    KWEND                 -> clKW
    KWCOMMIT              -> clKW

    KWMapEmpty            -> clIdent
    KWMapInsert           -> clIdent
    KWMapinsert           -> clIdent
    KWMapLookup           -> clIdent
    KWMaplookup           -> clIdent
    KWArrayLength         -> clIdent
    KWArrayIndex          -> clIdent
    KWRangeUp             -> clIdent
    KWRangeDown           -> clIdent
    KWOffset              -> clIdent
    KWDollarAny           -> clIdent
    KWGetStream           -> clIdent
    KWSetStream           -> clIdent
    KWTake                -> clIdent
    KWDrop                -> clIdent
    KWJust                -> clKW
    KWNothing             -> clKW
    KWBuilderbuild        -> clIdent
    KWBuilderemit         -> clIdent
    KWBuilderemitArray    -> clIdent
    KWBuilderemitBuilder  -> clIdent
    KWBuilderbuilder      -> clIdent
    KWDef                 -> clKW
    KWArrayStream         -> clIdent
    KWBytesOfStream       -> clIdent
    KWFail                -> clKW
    KWCase                -> clKW
    KWBitData             -> clKW
    KWWhere               -> clKW

    KWpi                  -> clLiteral
    KWWordToFloat         -> clIdent
    KWWordToDouble        -> clIdent
    KWIsNaN               -> clIdent
    KWIsInfinite          -> clIdent
    KWIsDenormalized      -> clIdent
    KWIsNegativeZero      -> clIdent

    TokError {}           -> clNone
    TokEOF                -> clNone




