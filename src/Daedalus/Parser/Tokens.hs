module Daedalus.Parser.Tokens where

import Data.ByteString(ByteString)
import Data.Word(Word8)

data Token =
    BigIdent
  | SmallIdent
  | SetIdent
  | SmallIdentI
  | BigIdentI
  | SetIdentI
  | Number  !Integer !(Maybe Int)
  | Bytes   !ByteString
  | Byte    !Word8

  | OpenBrace
  | CloseBrace
  | OpenBraceBar
  | CloseBraceBar
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | OpenTri
  | CloseTri

  | VOpen | VSemi | VClose    -- inserted via layout

  | Semi
  | Colon
  | Dot
  | DotDot
  | Comma
  | AtSign
  | Equals
  | DoubleEquals
  | BangEquals
  | Bang
  | Hat
  | Bar
  | DotBarDot
  | DotAmpDot
  | DotHatDot
  | BarBar
  | AmpAmp
  | LtBar
  | Dollar
  | DollarDollar
  | Plus
  | Minus
  | Star
  | ForwardSlash
  | Percent
  | TokLeq
  | TokGeq
  | Hash
  | LeftHash
  | ShiftL
  | ShiftR
  | RightArrow
  | Underscore

  | BitwiseComplementT

  | KWstruct
  | KWunion
  | KWChoose
  | KWFirst
  | KWAccept
  | KWblock
  | KWlet
  | KWTry
  | KWMatch
  | KWMany
  | KWManyQuestion
  | KWmany
  | KWmanyQuestion
  | KWOptional
  | KWOptionalQuestion
  | KWUInt8
  | KWTrue
  | KWFalse
  | KWFor
  | KWMap
  | KWIn
  | KWIs
  | KWOf
  | KWInt
  | KWUInt
  | KWSInt
  | KWFloat
  | KWDouble
  | KWBool
  | KWMaybe
  | KWStream
  | KWIf
  | KWThen
  | KWElse
  | KWImport
  | KWExtern
  | KWAs
  | KWAsBang
  | KWAsQuestion
  | KWConcat
  | KWEND
  | KWCOMMIT
  | KWMapEmpty
  | KWMapInsert
  | KWMapinsert
  | KWMapLookup
  | KWMaplookup
  | KWArrayLength
  | KWArrayIndex
  | KWRangeUp
  | KWRangeDown
  | KWOffset
  | KWDollarAny
  | KWGetStream
  | KWSetStream
  | KWTake
  | KWDrop
  | KWJust
  | KWNothing
  | KWBuilderbuild
  | KWBuilderemit
  | KWBuilderemitArray
  | KWBuilderemitBuilder
  | KWBuilderbuilder
  | KWDef
  | KWArrayStream
  | KWBytesOfStream
  | KWFail
  | KWCase
  | KWBitData
  | KWWhere

  | KWpi
  | KWWordToFloat
  | KWWordToDouble
  | KWIsNaN
  | KWIsInfinite
  | KWIsDenormalized
  | KWIsNegativeZero

  | TokError !String
  | TokEOF

    deriving Show



