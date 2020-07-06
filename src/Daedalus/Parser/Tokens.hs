module Daedalus.Parser.Tokens where

import Data.ByteString(ByteString)
import Data.Word(Word8)

data Token =
    BigIdent
  | SmallIdent
  | SetIdent
  | Number  !Integer
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
  | Amp
  | LtBar
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

  | BitwiseAndT
  | BitwiseOrT
  | BitwiseXorT
  | BitwiseComplementT

  | KWChoose
  | KWChoose1
  | KWTry
  | KWMany
  | KWManyQuestion
  | KWOptional
  | KWOptionalQuestion
  | KWUInt8
  | KWTrue
  | KWFalse
  | KWFor
  | KWMap
  | KWIn
  | KWIs
  | KWInt
  | KWUInt
  | KWSInt
  | KWBool
  | KWMaybe
  | KWStream
  | KWIf
  | KWThen
  | KWElse
  | KWImport
  | KWAs
  | KWAsBang
  | KWConcat
  | KWEND
  | KWCOMMIT
  | KWMapEmpty
  | KWMapInsert
  | KWMapLookup
  | KWArrayLength
  | KWArrayIndex
  | KWRangeUp
  | KWRangeDown
  | KWOffset
  | KWDollarUInt
  | KWGetStream
  | KWSetStream
  | KWTake
  | KWDrop
  | KWJust
  | KWNothing
  | KWDef
  | KWArrayStream
  | KWFail

  | TokError !String
  | TokEOF
    deriving Show



