{-# Language OverloadedStrings, BlockArguments #-}
{-# Language ImplicitParams, ConstraintKinds #-}
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


-- XXX: Avoid using the same name for types and values.

{- assumptions on all DDL types:
  * default constructors: for uninitialized block parameters
  * assignment: for passing block parameters
-}


cModule :: Module -> Doc
cModule m =
  vcat' (map cTypeGroup (mTypes m))
  $$
  "// -----------------------------------------"
  $$
  vcat' (map cFun (mFuns m))

type CurFun   = (?curFun   :: VMFun)
type CurBlock = (?curBlock :: Block)

cFun :: VMFun -> CDecl
cFun fun =
    retTy <+> cFNameDecl nm <.> "() {" -- XXX: arguments for non-capture
  $$ nest 2 body $$ "}"

  where
  nm = vmfName fun
  ty = cSemType (Src.fnameType nm)
  retTy = if vmfPure fun then ty else inst "std::optional" [ty]
  body = let ?curFun = fun
         in vcat' (params : map cBlock (Map.elems (vmfBlocks fun)))
  params = vcat (map cDeclareBlockParams (Map.elems (vmfBlocks fun)))

cDeclareBlockParams :: Block -> CStmt
cDeclareBlockParams b =
  let ?curBlock = b
  in vcat
      [ cArgDecl ba <.> semi | ba <- blockArgs b ]
  -- assumes default constructor

cBlock :: CurFun => Block -> CStmt
cBlock b = cBlockLabel (blockName b) <.> ": {" $$ nest 2 body $$ "}"
  where
  body = let ?curBlock = b
         in vcat (map cStmt (blockInstrs b)) $$ cTermStmt (blockTerm b)


cBlockLabel :: Label -> Doc
cBlockLabel (Label _ x) = "L" <.> int x



--------------------------------------------------------------------------------

cStmt :: CurBlock => Instr -> CStmt
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
        Op2 op2 -> cOp2 op2 es
        Op3 op3 -> todo
        OpN opN -> todo
  where
  todo = "/* todo (stmt)" <+> pp instr <+> "*/"

cOp1 :: CurBlock => Src.Op1 -> [E] -> CExpr
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
  todo = "/* todo (1)" <+> pp op1 <+> "*/"
  args = map cExpr es


cOp2 :: CurBlock => Src.Op2 -> [E] -> CExpr
cOp2 op2 es =
  case op2 of
    Src.IsPrefix -> todo
    Src.Drop -> todo
    Src.Take -> todo

    Src.Eq -> todo
    Src.NotEq -> todo
    Src.Leq -> todo
    Src.Lt -> todo

    Src.Add -> todo
    Src.Sub -> todo
    Src.Mul -> todo
    Src.Div -> todo
    Src.Mod -> todo

    Src.BitAnd -> todo
    Src.BitOr -> todo
    Src.BitXor -> todo
    Src.Cat -> todo
    Src.LCat -> todo
    Src.LShift -> todo
    Src.RShift -> todo

    Src.Or -> todo
    Src.And -> todo
    Src.ArrayIndex -> todo
    Src.ConsBuilder -> todo
    Src.MapLookup -> todo
    Src.MapMember -> todo

    Src.ArrayStream -> todo

  where
  todo = "/* todo (2)" <+> pp op2 <+> "*/"
  args = map cExpr es






cExpr :: CurBlock => E -> CExpr
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


cTermStmt :: (CurFun, CurBlock) => CInstr -> CStmt
cTermStmt cinstr =
  case cinstr of
    Jump jp -> cJump jp
    JumpIf e th el -> "if" <+> parens (cExpr e) <+> "{"
                      $$ nest 2 (cJump th)
                      $$ "} else {"
                      $$ nest 2 (cJump el)
                      $$ "}"
    Yield -> todo
    ReturnNo -> "return" <+> call "optional" [] <.> semi
    ReturnYes e -> "return" <+> call "optional" [cExpr e] <.> semi
    Call f c (JumpPoint l1 es1) (JumpPoint l2 es2) args -> todo
    TailCall f c es -> todo
    ReturnPure e -> "return" <+> cExpr e <.> semi

  where
  todo = "/* TODO:" <+> pp cinstr <+> "*/"

cJump :: (CurFun, CurBlock) => JumpPoint -> CStmt
cJump (JumpPoint l es) =
  case Map.lookup l (vmfBlocks ?curFun) of
    Just b  -> vcat (zipWith assignP (blockArgs b) es)
                 $$ "goto" <+> cBlockLabel l <.> semi
      where assignP ba e = cArgUse ba <+> "=" <+> cExpr e <.> semi
    Nothing -> panic "cJump" [ "Missing block: " ++ show (pp l) ]


cFNameDecl :: Src.FName -> Doc
cFNameDecl f = pp f

cClo1 :: JumpPoint -> CExpr
cClo1 (JumpPoint l es) = "/* XXX: closure */"

cVarUse :: BV -> CExpr
cVarUse (BV x _) = "x" P.<> int x

cVarDecl :: BV -> CExpr -> CStmt
cVarDecl v@(BV x t) e = cType t <+> cVarUse v <+> "=" <+> e P.<> semi

cArgDecl :: CurBlock => BA -> CExpr
cArgDecl ba@(BA x vmt) = cType vmt <+> cArgUse ba

cArgUse :: CurBlock => BA -> CExpr
cArgUse (BA x _) = cBlockLabel (blockName ?curBlock) <.> "a" <.> int x

cBytes :: ByteString -> CExpr
cBytes bs = "std::vector<uint8_t>" <+> braces (fsep (punctuate comma cs))
  where
  cs = map sh (BS8.unpack bs)
  sh c = if isAscii c && isPrint c && (c /= '\'')
            then text (show c)
            else "0x" P.<> text (showHex (fromEnum c) "")
