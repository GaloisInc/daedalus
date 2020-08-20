{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char
import Numeric
import Text.PrettyPrint as P

import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.VM
import qualified Daedalus.Core as Src


cExpr :: E -> CExpr
cExpr expr =
  case expr of
    EUnit   -> call "Unit" []
    EBool b -> if b then "true" else "false"
    EByteArray bs -> cBytes bs
    ENum n ty -> call f [ integer n ]
      where
      f = case ty of
            Src.TUInt (Src.TSize n)
              | n <= 8  -> "UINT8_C"
              | n <= 16 -> "UINT16_C"
              | n <= 32 -> "UINT32_C"
              | n <= 64 -> "UINT64_C"
              | otherwise -> panic "cExpr" [ "Uninplemented: unsigned > 64" ]

            Src.TSInt (Src.TSize n)
              | n <= 8  -> "INT8_C"
              | n <= 16 -> "INT16_C"
              | n <= 32 -> "INT32_C"
              | n <= 64 -> "INT64_C"
              | otherwise -> panic "cExpr" [ "Uninplemented: signed > 64" ]

            Src.TInteger -> panic "cExpr" [ "Uninplemented: integer" ]

            _ -> panic "cExpr" [ "Unexpected type for numeric constant"
                               , show (pp ty) ]

{-
    EMapEmpty Src.Type Src.Type
    ENothing Src.Type

    EBlockArg BA
    EVar      BV
-}


--------------------------------------------------------------------------------
type CExpr = Doc

call :: CExpr -> [CExpr] -> CExpr
call f es = f P.<> parens (fsep (punctuate comma es))

cBytes :: ByteString -> CExpr
cBytes bs = parens ("std::vector<uint8_t>" <+>
                       braces (fsep (punctuate comma cs)))
  where
  cs = map sh (BS8.unpack bs)
  sh c = if isAscii c && isPrint c && (c /= '\'')
            then text (show c)
            else "0x" P.<> text (showHex (fromEnum c) "")

