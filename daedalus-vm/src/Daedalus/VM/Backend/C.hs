{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Data.Char
import Numeric
import Text.PrettyPrint as P

import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.Rec(Rec(..))
import Daedalus.VM
import qualified Daedalus.Core as Src

import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Types


cModule :: Module -> Doc
cModule m = vcat' (map cTypeGroup (mTypes m))


--------------------------------------------------------------------------------

cStmt :: Instr -> CStmt
cStmt instr =
  case instr of
    SetInput e      -> call "p.setInput" [ cExpr e]
    Say x           -> call "p.say"      [ cString x ]
    Output e        -> call "p.output"   [ cExpr e ]
    Notify e        -> call "p.notify"   [ cExpr e ]
    NoteFail        -> call "p.noteFail" []
    GetInput x      -> cVarDecl x (call "p.getInput" [])
    Spawn l x       -> cVarDecl x (call "p.spawn" [ cClo1 l ])
    CallPrim p es x -> "/* XXX cStmt: call primt */"


cExpr :: E -> CExpr
cExpr expr =
  case expr of
    EBlockArg x   -> cArgUse x
    EVar x        -> cVarUse x
    EUnit         -> call "Unit" []
    EBool b       -> if b then "true" else "false"
    EByteArray bs -> cBytes bs
    ENum n ty     -> call f [ integer n ]
      where
      f = case ty of
            Src.TUInt (Src.TSize n)
              | n <= 8  -> "UINT8_C"
              | n <= 16 -> "UINT16_C"
              | n <= 32 -> "UINT32_C"
              | n <= 64 -> "UINT64_C"
              | otherwise -> todo

            Src.TSInt (Src.TSize n)
              | n <= 8  -> "INT8_C"
              | n <= 16 -> "INT16_C"
              | n <= 32 -> "INT32_C"
              | n <= 64 -> "INT64_C"
              | otherwise -> todo

            Src.TInteger -> todo

            _ -> panic "cExpr" [ "Unexpected type for numeric constant"
                               , show (pp ty) ]

    EMapEmpty {} -> todo
    ENothing {}  -> todo

  where
  todo = "/* XXX cExpr:" <+> pp expr <+> "*/"

cClo1 :: JumpPoint -> CExpr
cClo1 (JumpPoint l es) = "/* XXX: closure */"

cVarUse :: BV -> CExpr
cVarUse (BV x _) = "x" P.<> int x

cVarDecl :: BV -> CExpr -> CStmt
cVarDecl v@(BV x t) e = cType t <+> cVarUse v <+> "=" <+> e P.<> semi

cArgUse :: BA -> CExpr
cArgUse (BA x _) = "a" P.<> int x

cBytes :: ByteString -> CExpr
cBytes bs = parens ("std::vector<uint8_t>" <+>
                       braces (fsep (punctuate comma cs)))
  where
  cs = map sh (BS8.unpack bs)
  sh c = if isAscii c && isPrint c && (c /= '\'')
            then text (show c)
            else "0x" P.<> text (showHex (fromEnum c) "")
