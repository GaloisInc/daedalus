{-# Language OverloadedStrings, BlockArguments #-}
{-# Language ImplicitParams, ConstraintKinds #-}
module Daedalus.VM.Backend.C where

{-
import Data.ByteString(ByteString)
-}
import qualified Data.ByteString as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe(maybeToList,mapMaybe)

import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.Rec(topoOrder,forgetRecs)

import Daedalus.VM
import qualified Daedalus.Core as Src
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types
import Daedalus.VM.Backend.C.UserDefined
import Daedalus.VM.Backend.C.Call


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
    , vcat' (map cTypeGroup allTypes)
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
  doBlock b      = let ?allFuns = Map.fromList [ (vmfName f, f) | f <- allFuns ]
                       ?allBlocks = allBlocks
                   in cBasicBlock b

  parserTy       = cSemType (pType prog)
  parserDef      = vcat [ params
                        , cDeclareRetVars allFuns
                        , cDeclareCallClosures (Map.elems allBlocks)
                        , " "
                        , "// --- States ---"
                        , " "
                        , cGoto (cBlockLabel (pEntry prog))
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
type Copies     = (?copies    :: Map BV E)

cDeclareBlockParams :: Block -> CStmt
cDeclareBlockParams b
  | null ps     = empty
  | otherwise   = vcat (header : ps)
  where
  header = "\n// Parameters for" <+> pp (blockName b)
  ps = [ cDeclareVar (cType (getType ba)) (cArgUse b ba) | ba <- blockArgs b ]

{- A function always returns 0 or 1 things, so it should be sufficient
to just have 1 varaible per type.
Alternatively, we could generate separate variables for each functions.
-}
cDeclareRetVars :: [VMFun] -> CStmt
cDeclareRetVars funs =
  case stmts of
    [] -> empty
    _  -> vcat (header : stmts)
  where
  header  = "\n// Varaibles used to return values from functions"
  stmts   = map decl $ Set.toList $ Set.fromList
                                  $ map (Src.fnameType . vmfName) funs
  decl t  = cDeclareVar (cSemType t) (cRetVar (TSem t))


cDeclareCallClosures :: [Block] -> CStmt
cDeclareCallClosures bs =
  case mapMaybe doClo bs of
    [] -> empty
    ps -> vcat' (header : ps)
  where
  header = "\n// Types for preserving variables across calls"
  doClo b =
    case blockType b of
      NormalBlock   -> Nothing
      ReturnBlock n ->
        Just $
        cReturnClass "DDL::Closure" (blockName b)
                                    (map getType (drop n (blockArgs b)))
      ThreadBlock ->
        Just $
        cReturnClass "DDL::ThreadClosure" (blockName b)
                                    (map getType (drop 1 (blockArgs b)))




cBasicBlock :: (AllFuns,AllBlocks) => Block -> CStmt
cBasicBlock b = "//" <+> text (show (blockType b))
             $$ cBlockLabel (blockName b) <.> ": {" $$ nest 2 body $$ "}"
  where
  body = let ?curBlock = b
             ?copies   = Map.fromList [ (x,v) | Let x v <- blockInstrs b ]
         in getArgs
         $$ vcat (map cBlockStmt (blockInstrs b))
         $$ vcat (cTermStmt (blockTerm b))

  getArgs = case blockType b of
              NormalBlock -> empty
              ReturnBlock n ->
                let (ras,cas) = splitAt n (blockArgs b)
                    ty = cPtrT (cReturnClassName (blockName b))
                in
                cBlock $
                  [ cAssign (cArgUse b v) (cRetVar (getType v)) | v <- ras ] ++
                  [ cDeclareInitVar ty "clo" (parens ty <.> cCall "p.pop" [])
                  ] ++
                  [ cAssign (cArgUse b v) e
                  | (v,n) <- cas `zip` [ 0 .. ]
                  , let e = "clo->" <.> cField n
                  ] ++
                  [ "clo->free();" ]
              ThreadBlock ->
                let x : xs = blockArgs b
                    ty = cPtrT (cReturnClassName (blockName b))
                in
                cBlock
                  $ cDeclareInitVar ty "clo" (parens ty <.> cCall "p.pop" [])
                  : cAssign (cArgUse b x) "clo->notified"
                  : [ cAssign (cArgUse b v) e
                    | (v,n) <- xs `zip` [ 0 .. ]
                    , let e = "clo->" <.> cField n
                    ]
                 ++ [ "clo->free();" ]

              ThreadBlock -> "/* todo */"




--------------------------------------------------------------------------------

cVarDecl :: BV -> CExpr -> CStmt
cVarDecl v e = cStmt (cType (getType v) <+> cVarUse v <+> "=" <+> e)

cVMVar :: (Copies, CurBlock) => VMVar -> CExpr
cVMVar vmvar =
  case vmvar of
    ArgVar x   -> cArgUse ?curBlock x
    LocalVar x -> case Map.lookup x ?copies of
                    Just e  -> cExpr e
                    Nothing -> cVarUse x

cBlockStmt :: (Copies,CurBlock) => Instr -> CStmt
cBlockStmt cInstr =
  case cInstr of
    SetInput e      -> cStmt (cCall "p.setInput" [ cExpr e])
    Say x           -> cStmt (cCall "p.say"      [ cString x ])
    Output e        -> cStmt (cCall "p.output"   [ cExpr e ])
    Notify e        -> cStmt (cCall "p.notify"   [ cExpr e ])
    NoteFail        -> cStmt (cCall "p.noteFail" [])
    GetInput x      -> cVarDecl x (cCall "p.getInput" [])
    Spawn x l       -> todo
    Free xs         -> vcat (cFree xs)

    Let _ e
      | Just v <- eIsVar e -> cStmt (cCall (cVMVar v <.> ".copy") [])
      | otherwise          -> empty

    CallPrim x p es ->
      case p of
        StructCon ut ->
          let v = cVarUse x
          in vcat [ cDeclareVar (cType (getType x)) v
                  , cStmt (cCall (v <.> ".init")
                          [ cExpr e | e <- es, getType e /= TSem Src.TUnit ])
                  ]

        NewBuilder t -> cVarDecl x todo
        Integer n    -> cStmt (cCall "DDL::Integer" [ cString (show n) ])
        ByteArray bs -> cVarDecl x
                              (cCall "DDL::Array<DDL::UInt<8>>"
                                ( int (BS.length bs)
                                : [ cCall "DDL::UInt<8>" [ text (show w) ]
                                  | w <- BS.unpack bs
                                  ]
                                ))
        Op1 op1      -> cVarDecl x (cOp1 op1 es)
        Op2 op2      -> cOp2 x op2 es
        Op3 op3      -> cVarDecl x todo
        OpN opN      -> cVarDecl x todo
  where
  todo = "/* todo (stmt)" <+> pp cInstr <+> "*/"


cFree :: (CurBlock, Copies) => Set VMVar -> [CStmt]
cFree xs = [ cStmt (cCall (cVMVar y <.> ".free") [])
           | x <- Set.toList xs
           , y <- freeVar x
           ]
  where
  freeVar x =
    case x of
      LocalVar y | Just e <- Map.lookup y ?copies -> maybeToList (eIsVar e)
      _                                           -> [x]




cOp1 :: (Copies, CurBlock) => Src.Op1 -> [E] -> CExpr
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
    Src.ArrayLen -> cCall "DDL::Integer" [ cCall (e <.> ".size") [] ]
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
  [e] = map cExpr es


cOp2 :: (Copies,CurBlock) => BV -> Src.Op2 -> [E] -> CDecl
cOp2 x op2 ~[e1,e2] =
  case op2 of
    Src.IsPrefix -> cVarDecl x (cCall (cExpr e2 <.> ".hasPrefix") [ cExpr e1 ])
    Src.Drop -> cVarDecl x (cCall ((cExpr e2) <.> ".iDropI") [ cExpr e1 ])
    Src.Take -> cVarDecl x (cCall ((cExpr e2) <.> ".iTakeI") [ cExpr e1 ])

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



--------------------------------------------------------------------------------


cExpr :: (CurBlock,Copies) => E -> CExpr
cExpr expr =
  case expr of
    EBlockArg x   -> cArgUse ?curBlock x
    EVar x        -> case Map.lookup x ?copies of
                       Just e  -> cExpr e
                       Nothing -> cVarUse x
    EUnit         -> cCall "DDL::Unit" []
    EBool b       -> if b then "true" else "false"
    ENum n ty     -> cCall f [ integer n ]
      where
      f = case ty of
            Src.TUInt sz -> cInst "DDL::UInt" [ cSizeType sz ]
            Src.TSInt sz -> cInst "DDL::SInt" [ cSizeType sz ]

            _ -> panic "cExpr" [ "Unexpected type for numeric constant"
                               , show (pp ty) ]

    EMapEmpty {} -> todo
    ENothing {}  -> todo

  where
  todo = "/* XXX cExpr:" <+> pp expr <+> "*/"




--------------------------------------------------------------------------------

cTermStmt :: (AllFuns, AllBlocks, CurBlock, Copies) => CInstr -> [CStmt]
cTermStmt ccInstr =
  case ccInstr of
    Jump jp -> cJump jp

    JumpIf e choice ->
      [ cIf (cExpr e) (doChoice (jumpYes choice)) (doChoice (jumpNo  choice)) ]
      where
      doChoice ch = cFree (freeFirst ch) ++ cJump (jumpTarget ch)

    Yield ->
      [ cIf (cCall "p.hasSuspended" [])
          [ cGoto ("*" <.> cCall "p.yield" []) ]
          [ "return;" ]
      ]

    ReturnNo -> [ cGoto ("*" <.> cCall "p.returnNo" []) ]

    ReturnYes e ->
      [ cAssign (cRetVar (getType e)) (cExpr e)
      , cGoto ("*" <.> cCall "p.returnYes" [])
      ]

    ReturnPure e ->
      [ cAssign (cRetVar (getType e)) (cExpr e)
      , cGoto ("*" <.> cCall "p.returnPure" [])
      ]

    -- "no
    Call f _ no yes es ->
        doPush no
      : doPush yes
      : cJump (JumpPoint (lkpFun f) es)

    CallPure f l es -> doPush l : cJump (JumpPoint (lkpFun f) es)

    TailCall f _ es -> cJump (JumpPoint (lkpFun f) es)

  where
  todo = "/* TODO:" <+> pp ccInstr <+> "*/"
  lkpFun f = case Map.lookup f ?allFuns of
               Just fun -> vmfEntry fun
               Nothing  -> panic "cTermStmt" [ "Unknown function", show (pp f) ]

  doPush l =
    let clo = cCall (cReturnClassName (jLabel l))
                    ("&&" <.> cBlockLabel (jLabel l) : map cExpr (jArgs l))
    in cStmt (cCall "p.push" ["new" <+> clo])


cDoJump :: (Copies,CurBlock) => Block -> [E] -> [CStmt]
cDoJump b es =
  zipWith assignP (blockArgs b) es ++ [ cGoto (cBlockLabel l) ]
  where
  l            = blockName b
  assignP ba e = cAssign (cArgUse b ba) (cExpr e)

cJump :: (AllBlocks, CurBlock, Copies) => JumpPoint -> [CStmt]
cJump (JumpPoint l es) =
  case Map.lookup l ?allBlocks of
    Just b  -> cDoJump b es
    Nothing -> panic "cJump" [ "Missing block: " ++ show (pp l) ]



