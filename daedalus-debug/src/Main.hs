{-# Language OverloadedStrings #-}
module Main(main) where

import System.Environment
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified System.Directory as Dir
import System.FilePath ((</>), takeExtension)

import AlexTools
import Daedalus.Parser.Lexer

import RTS.ParseError(normalizePathFun)
import RTS.JSON

main :: IO ()
main =
  do args <- getArgs
     fs <- jsFiles 1 args
     let norm = normalizePathFun (Set.fromList (map fst fs))
         key  = Text.encodeUtf8 . Text.pack . norm
     let mp = jsObject [ (key f, j) | (f,j) <- fs ]
     BS8.putStrLn "const files ="
     BS8.putStrLn (jsonToBytes mp)

jsFiles :: Int -> [FilePath] -> IO [(FilePath, JSON)]
jsFiles d fs = concat <$> mapM (jsFile d) fs

jsFile :: Int -> FilePath -> IO [(FilePath, JSON)]
jsFile d file =
  do yes <- Dir.doesFileExist file
     if yes then doFile else doDirectory
  where
  doFile
    | let e = takeExtension file
    , e == ".ddl" || e == ".md" =
    do txt <- Text.readFile file
       let nm = Text.pack file
       let ts = lexer nm txt
       a <- Dir.canonicalizePath file
       pure [ ( a
              , jsObject [ ("text",  toJSON txt)
                         , ("syntax", jsLexemes ts)
                         ]
              ) ]
    | otherwise = pure []

  doDirectory
    | d > 0 =
      do fs <- Dir.listDirectory file
         jsFiles (d-1) [ file </> f | f <- fs ]
    | otherwise = pure []

jsLexemes :: [Lexeme Token] -> JSON
jsLexemes ls =
  jsObject
    [ (BS8.pack k, jsArray v)
    | (k,v) <- Map.toList $ Map.delete "" $ Map.fromListWith (++) $ map toMap ls
    ]
  where
  rng p   = jsArray [ toJSON (sourceLine p), toJSON (sourceColumn p) ]
  toMap l =
    let t = lexemeToken l
        r = lexemeRange l
    in (tokenClass t, [ jsArray [ rng (sourceFrom r), rng (sourceTo r) ]])


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





