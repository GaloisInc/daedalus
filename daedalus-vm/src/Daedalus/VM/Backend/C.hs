{-# Language OverloadedStrings, BlockArguments #-}
{-# Language ImplicitParams, ConstraintKinds #-}
module Daedalus.VM.Backend.C where

{-
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
-}
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint as P

import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.Rec(topoOrder,forgetRecs)

import Daedalus.VM
import qualified Daedalus.Core as Src
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types
import Daedalus.VM.Backend.C.UserDefined

-- XXX: Avoid using the same name for types and values.

{- assumptions on all DDL types:
  * default constructors: for uninitialized block parameters
  * assignment: for passing block parameters
-}


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
    , "void parser(" <.> cInst "DDL::Parser" [ parserTy ] <+> "&p) {"
    , nest 2 parserDef
    , "}"
    ]

  where
  orderedModules = forgetRecs (topoOrder modDeps (pModules prog))
  modDeps m      = (mName m, Set.fromList (mImports m))

  allTypes       = concatMap mTypes orderedModules
  allFuns        = concatMap mFuns orderedModules
  allBlocks      = Map.unions (pBoot prog : map vmfBlocks allFuns)
  doBlock b      = let ?allFuns   = allFuns
                       ?allBlocks = allBlocks
                   in cBlock b

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


type AllFuns    = (?allFuns   :: Map Src.FName VMFun)
type AllBlocks  = (?allBlocks :: Map Label Block)
type CurBlock   = (?curBlock  :: Block)

cDeclareBlockParams :: Block -> CStmt
cDeclareBlockParams b
  | null ps     = empty
  | otherwise   = vcat (header : ps)
  where
  header = "// Parameters for" <+> pp (blockName b)
  ps     = [ cArgDecl b ba <.> semi | ba <- blockArgs b ]

cArgDecl :: Block -> BA -> CExpr
cArgDecl bl ba@(BA x vmt _) = cType vmt <+> cArgUse bl ba


cBlock :: AllBlocks => Block -> CStmt
cBlock b = cBlockLabel (blockName b) <.> ": {" $$ nest 2 body $$ "}"
  where
  body = let ?curBlock = b
         in vcat (map cBlockStmt (blockInstrs b)) $$ cTermStmt (blockTerm b)




--------------------------------------------------------------------------------

cBlockStmt :: CurBlock => Instr -> CStmt
cBlockStmt cInstr =
  case cInstr of
    SetInput e      -> cStmt (cCall "p.setInput" [ cExpr e])
    Say x           -> cStmt (cCall "p.say"      [ cString x ])
    Output e        -> cStmt (cCall "p.output"   [ cExpr e ])
    Notify e        -> cStmt (cCall "p.notify"   [ cExpr e ])
    NoteFail        -> cStmt (cCall "p.noteFail" [])
    GetInput x      -> cVarDecl x (cCall "p.getInput" [])
    Spawn x l       -> todo
    CallPrim x p es ->
      case p of
        StructCon ut -> cVarDecl x todo
        NewBuilder t -> cVarDecl x todo
        Op1 op1      -> cVarDecl x (cOp1 op1 es)
        Op2 op2      -> cOp2 x op2 es
        Op3 op3      -> cVarDecl x todo
        OpN opN      -> cVarDecl x todo
  where
  todo = "/* todo (stmt)" <+> pp cInstr <+> "*/"

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
    Src.IsPrefix -> cVarDecl x (cCall (cExpr e2 <.> ".hasPrefix") [ cExpr e1 ])
    Src.Drop -> cVarDecl x (cCall ((cExpr e2) <.> ".iDrop") [ cExpr e1 ])
    Src.Take -> cVarDecl x (cCall ((cExpr e2) <.> ".iTake") [ cExpr e1 ])

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
    EBlockArg x   -> cArgUse ?curBlock x
    EVar x        -> cVarUse x
    EUnit         -> cCall "Unit" []
    EBool b       -> if b then "true" else "false"
    ENum n ty     -> cCall f [ integer n ]
      where
      f = case ty of
            Src.TUInt sz -> cInst "UInt" [ cSizeType sz ]
            Src.TSInt sz -> cInst "SInt" [ cSizeType sz ]

            _ -> panic "cExpr" [ "Unexpected type for numeric constant"
                               , show (pp ty) ]

    EMapEmpty {} -> todo
    ENothing {}  -> todo

  where
  todo = "/* XXX cExpr:" <+> pp expr <+> "*/"


cTermStmt :: (AllBlocks, CurBlock) => CInstr -> CStmt
cTermStmt ccInstr =
  case ccInstr of
    Jump jp -> cJump jp
    JumpIf {} -> todo
{-
    JumpIf e th el -> "if" <+> parens (cExpr e) <+> "{"
                      $$ nest 2 (cJump th)
                      $$ "} else {"
                      $$ nest 2 (cJump el)
                      $$ "}"
-}
    TailCall f c es -> todo

    Yield -> todo
    ReturnNo -> todo
    ReturnYes e -> todo
    Call f c l1 l2 args -> todo
    ReturnPure e -> "return" <+> cExpr e <.> semi

  where
  todo = "/* TODO:" <+> pp ccInstr <+> "*/"


cDoJump :: CurBlock => Block -> [E] -> CStmt
cDoJump b es =
  vcat [ vcat (zipWith assignP (blockArgs b) es)
       , "goto" <+> cBlockLabel l <.> semi
       ]
  where
  l = blockName b
  assignP ba e = cStmt (cArgUse b ba <+> "=" <+> cExpr e)

cJump :: (AllBlocks, CurBlock) => JumpPoint -> CStmt
cJump (JumpPoint l es) =
  case Map.lookup l ?allBlocks of
    Just b  -> cDoJump b es
    Nothing -> panic "cJump" [ "Missing block: " ++ show (pp l) ]

cVarDecl :: BV -> CExpr -> CStmt
cVarDecl v@(BV x t) e = cType t <+> cVarUse v <+> "=" <+> e P.<> semi


