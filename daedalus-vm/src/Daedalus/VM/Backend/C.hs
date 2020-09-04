{-# Language OverloadedStrings, BlockArguments #-}
module Daedalus.VM.Backend.C where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Map as Map
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
cModule m =
  vcat' (map cTypeGroup (mTypes m))
  $$
  "// -----------------------------------------"
  $$
  vcat' (map cFun (mFuns m))


cFun :: VMFun -> CDecl
cFun fun = retTy <+> cFNameDecl nm <.> "() {" -- XXX: arguments for non-capture
  $$ nest 2 body $$ "}"

  where
  nm = vmfName fun
  ty = cSemType (Src.fnameType nm)
  retTy = ty -- XXXcase Src.fnameType nm of
  body = vcat' (params : map cBlock (Map.elems (vmfBlocks fun)))
  params = "/* params */" 

cBlock :: Block -> CStmt
cBlock b = cBlockLabel (blockName b) <.> ": {" $$ nest 2 body $$ "}"
  where
  body = vcat (map cStmt (blockInstrs b)) $$ cTermStmt (blockTerm b)


cBlockLabel :: Label -> Doc
cBlockLabel (Label _ x) = "L" <.> int x



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
    CallPrim p es x -> cVarDecl x
      case p of
        StructCon ut -> todo
        NewBuilder t -> todo
        Op1 op1 -> cOp1 op1 es
        Op2 op2 -> todo
        Op3 op3 -> todo
        OpN opN -> todo
  where
  todo = "/* todo */"

cOp1 :: Src.Op1 -> [E] -> CExpr
cOp1 op1 es =
  case op1 of
    Src.CoerceTo t -> todo
    Src.CoerceMaybeTo t -> todo
    Src.IsEmptyStream -> todo
    Src.Head -> todo
    Src.StreamOffset -> todo
    Src.StreamLen -> todo
    Src.OneOf bs -> todo
    Src.Neg -> todo
    Src.BitNot -> todo
    Src.Not -> todo
    Src.ArrayLen -> todo
    Src.Concat -> todo
    Src.FinishBuilder -> todo
    Src.NewIterator -> todo
    Src.IteratorDone -> todo
    Src.IteratorKey -> todo
    Src.IteratorVal -> todo
    Src.IteratorNext -> todo
    Src.EJust -> todo
    Src.IsJust -> todo
    Src.FromJust -> todo
    Src.SelStruct t l -> todo
    Src.InUnion ut l -> todo
    Src.HasTag l -> todo
    Src.FromUnion t l -> todo
  where
  todo = "/* todo" <+> pp op1 <+> "*/"
  args = map cExpr es



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


cTermStmt :: CInstr -> CStmt
cTermStmt cinstr =
  case cinstr of
    Jump (JumpPoint l es) -> todo
    JumpIf e (JumpPoint l1 es1) (JumpPoint l2 es2) -> todo
    Yield -> todo
    ReturnNo -> todo
    ReturnYes e -> todo
    Call f c (JumpPoint l1 es1) (JumpPoint l2 es2) args -> todo
    TailCall f c es -> todo
    ReturnPure e -> todo

  where
  todo = "/* TODO:" <+> pp cinstr <+> "*/"


cFNameDecl :: Src.FName -> Doc
cFNameDecl f = pp f

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
