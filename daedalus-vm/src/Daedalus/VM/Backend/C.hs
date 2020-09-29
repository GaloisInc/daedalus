{-# Language OverloadedStrings, BlockArguments #-}
{-# Language ImplicitParams, ConstraintKinds #-}
module Daedalus.VM.Backend.C where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Numeric
import Text.PrettyPrint as P

import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.Rec(Rec(..),topoOrder,forgetRecs)
import Daedalus.VM
import qualified Daedalus.Core as Src

import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Types


-- XXX: Avoid using the same name for types and values.

{- assumptions on all DDL types:
  * default constructors: for uninitialized block parameters
  * assignment: for passing block parameters
-}

type AllFuns  = (?allFuns :: Map Src.FName VMFun)

cProgram :: Program -> Doc
cProgram prog =
  vcat
    [ includes
    , " "
    , "// --- Types --- //"
    , "  "
    ,  vcat' (map cTypeGroup allTypes)
    ,  "// --- End of Types //"
    , " "
    , "// --- Parser --- //"
    , " "
    , "void parser(" <.> inst "DDL::Parser" [ parserTy ] <+> "&p) {"
    , nest 2 parserDef
    , "}"
    ]

  where
  orderedModules = forgetRecs (topoOrder modDeps (pModules prog))
  modDeps m      = (mName m, Set.fromList (mImports m))

  allTypes       = concatMap mTypes orderedModules
  allFuns        = concatMap mFuns orderedModules
  allBlocks      = Map.unions (pBoot prog : map vmfBlocks allFuns)
  doBlock b      = let ?allBlocks = allBlocks in cBlock b

  parserTy       = cSemType (pType prog)
  parserDef      = vcat [ params
                        , " "
                        , "// --- States ---"
                        , " "
                        , vcat' (map doBlock (Map.elems allBlocks))
                        ]

  params         = vcat (map cDeclareBlockParams (Map.elems allBlocks))

  includes =
    vcat [ "#include <vector>"
         , "#include <unordered_map>"
         , "#include <memory>"
         , "#include <variant>"
         , ""
         , "#include <ddl/parser.h>"
         , "#include <ddl/unit.h>"
         , "#include <ddl/number.h>"
         , "#include <ddl/integer.h>"
         , "#include <ddl/input.h>"
         , "#include <ddl/array.h>"
         , "#include <ddl/map.h>"
         ]


type AllBlocks  = (?allBlocks :: Map Label Block)
type CurBlock   = (?curBlock  :: Block)

cDeclareBlockParams :: Block -> CStmt
cDeclareBlockParams b
  | null ps     = empty
  | otherwise   = vcat (header : ps)
  where
  header = "// Parameters for" <+> pp (blockName b)
  ps     = let ?curBlock = b
           in [ cArgDecl ba <.> semi | ba <- blockArgs b ]

cBlock :: AllBlocks => Block -> CStmt
cBlock b = cBlockLabel (blockName b) <.> ": {" $$ nest 2 body $$ "}"
  where
  body = let ?curBlock = b
         in vcat (map cStmt (blockInstrs b)) $$ cTermStmt (blockTerm b)


cBlockLabel :: Label -> Doc
cBlockLabel (Label f x) = pp f <.> "_" <.> int x



--------------------------------------------------------------------------------

cStmt :: CurBlock => Instr -> CStmt
cStmt instr =
  case instr of
    SetInput e      -> call "p.setInput" [ cExpr e] <.> semi
    Say x           -> call "p.say"      [ cString x ] <.> semi
    Output e        -> call "p.output"   [ cExpr e ] <.> semi
    Notify e        -> call "p.notify"   [ cExpr e ] <.> semi
    NoteFail        -> call "p.noteFail" [] <.> semi
    GetInput x      -> cVarDecl x (call "p.getInput" [])
    Spawn l x       -> cVarDecl x (call "p.spawn" [ cClo1 l ])
    CallPrim p es x ->
      case p of
        StructCon ut -> cVarDecl x todo
        NewBuilder t -> cVarDecl x todo
        Op1 op1 -> cVarDecl x (cOp1 op1 es)
        Op2 op2 -> cOp2 x op2 es
        Op3 op3 -> cVarDecl x todo
        OpN opN -> cVarDecl x todo
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


cOp2 :: CurBlock => BV -> Src.Op2 -> [E] -> CDecl
cOp2 x op2 ~[e1,e2] =
  case op2 of
    Src.IsPrefix -> cVarDecl x (call (cExpr e2 <.> ".hasPrefix") [ cExpr e1 ])
    Src.Drop -> cVarDecl x (call ((cExpr e2) <.> ".iDrop") [ cExpr e1 ])
    Src.Take -> cVarDecl x (call ((cExpr e2) <.> ".iTake") [ cExpr e1 ])

    Src.Eq    -> cVarDecl x (cExpr e1 <+> "==" <+> cExpr e2)
    Src.NotEq -> cVarDecl x (cExpr e1 <+> "!=" <+> cExpr e2)
    Src.Leq   -> cVarDecl x (cExpr e1 <+> "<=" <+> cExpr e2)
    Src.Lt    -> cVarDecl x (cExpr e1 <+> "<"  <+> cExpr e2)

    Src.Add   -> cVarDecl x (cExpr e1 <+> "+" <+> cExpr e2)
    Src.Sub   -> cVarDecl x (cExpr e1 <+> "-" <+> cExpr e2)
    Src.Mul   -> cVarDecl x (cExpr e1 <+> "*" <+> cExpr e2)
    Src.Div   -> cVarDecl x (cExpr e1 <+> "/" <+> cExpr e2)
    Src.Mod   -> cVarDecl x (cExpr e1 <+> "%" <+> cExpr e2)

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
      -- XXX: large constants should be turned into more complex expressions
      -- e.g., using add/multiply.
      f = case ty of
            Src.TUInt sz -> inst "UInt" [ cSizeType sz ]
            Src.TSInt sz -> inst "SInt" [ cSizeType sz ]
            Src.TInteger -> call "DDL::Integer" [ text (show n) ]

            _ -> panic "cExpr" [ "Unexpected type for numeric constant"
                               , show (pp ty) ]

    EMapEmpty {} -> todo
    ENothing {}  -> todo

  where
  todo = "/* XXX cExpr:" <+> pp expr <+> "*/"


cTermStmt :: (AllBlocks, CurBlock) => CInstr -> CStmt
cTermStmt cinstr =
  case cinstr of
    Jump jp -> cJump jp
    JumpIf e th el -> "if" <+> parens (cExpr e) <+> "{"
                      $$ nest 2 (cJump th)
                      $$ "} else {"
                      $$ nest 2 (cJump el)
                      $$ "}"
    TailCall f c es -> todo

    Yield -> todo
    ReturnNo -> todo
    ReturnYes e -> todo
    Call f c (JumpPoint l1 es1) (JumpPoint l2 es2) args -> todo
    ReturnPure e -> "return" <+> cExpr e <.> semi

  where
  todo = "/* TODO:" <+> pp cinstr <+> "*/"


cDoJump :: CurBlock => Block -> [E] -> CStmt
cDoJump b es =
  vcat [ vcat (zipWith assignP (blockArgs b) es)
       , if l /= blockName ?curBlock
           then vcat (map cDestroy (blockArgs ?curBlock))
           else empty
       , "goto" <+> cBlockLabel l <.> semi
       ]
  where
  l = blockName b
  assignP ba e = (let ?curBlock = b in cArgUse ba)
                <+> "=" <+> cExpr e <.> semi

cJump :: (AllBlocks, CurBlock) => JumpPoint -> CStmt
cJump (JumpPoint l es) =
  case Map.lookup l ?allBlocks of
    Just b  -> cDoJump b es
    Nothing -> panic "cJump" [ "Missing block: " ++ show (pp l) ]

cDestroy :: CurBlock => BA -> CStmt
cDestroy x = cArgUse x <+> "= {};"


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
cArgUse (BA x _) = cBlockLabel (blockName ?curBlock) <.> "_a" <.> int x

cBytes :: ByteString -> CExpr
cBytes bs = call "DDL::Array<DDL::UInt<8>>"
                        [ int (BS8.length bs), text (show bs) ]


--------------------------------------------------------------------------------



