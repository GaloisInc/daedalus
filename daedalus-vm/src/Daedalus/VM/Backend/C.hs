{-# Language OverloadedStrings, BlockArguments #-}
{-# Language ImplicitParams, ConstraintKinds #-}
{-# Language ParallelListComp #-}
module Daedalus.VM.Backend.C where

import qualified Data.ByteString as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Word(Word32,Word64)
import           Data.Int(Int32,Int64)
import           Data.Maybe(maybeToList,fromMaybe)
import           Data.List(partition,sortBy)
import           Data.Function(on)
import           Control.Applicative((<|>))

import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.Rec(topoOrder,forgetRecs)

import Daedalus.VM
import qualified Daedalus.Core as Src
import Daedalus.VM.RefCountSane
import Daedalus.VM.TypeRep
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types
import Daedalus.VM.Backend.C.UserDefined
import Daedalus.VM.Backend.C.Bitdata(bdCase,bdCaseDflt)
import Daedalus.VM.Backend.C.Call


{- assumptions on all DDL types:
  * default constructors: for uninitialized block parameters
  * assignment: for passing block parameters
-}

-- XXX: separate output and parser state(input/threads)

-- | Currently returns the content for @(.h,.cpp)@ files.
cProgram :: String -> Program -> (Doc,Doc)
cProgram fileNameRoot prog =
  case checkProgram prog of
    Nothing  -> (hpp,cpp)
    Just err -> panic "cProgram" err
  where
  module_marker = text fileNameRoot <.> "_H"


  hpp = vcat $
          [ "#ifndef" <+> module_marker
          , "#define" <+> module_marker
          , " "
          , includes
          , " "
          , let (ds,defs) = unzip (map (cTypeGroup allTypesMap) allTypes)
            in vcat' (ds ++ defs)
          , " "
          , "// --- Parsing Functions ---"
          ] ++
          [ delcareEntryResults (noCapRoots ++ capRoots) ] ++
          primSigs ++
          noCapRootSigs ++
          capRootSigs ++
          [ ""
          , "#endif"
          ]

  cpp = vcat $ [ "#include" <+> doubleQuotes (text fileNameRoot <.> ".h")
               , " "
               ] ++
               map (cFunSig Static) noPrims ++
               [ entTypeDef, capPrserSig ] ++
               noCapRootDefs ++
               capRootDefs ++
               noPrimDefs ++
               [ capParserDef ]



  -- primitives
  (prims,noPrims) = flip partition noCapFun \fun ->
                    case vmfDef fun of
                      VMExtern {} -> True
                      VMDef {}    -> False

  primSigs = case prims of
               [] -> []
               _  -> " "
                   : "// --- External Primitives ---"
                   : map (cFunSig Extern) prims

  -- Non-capturing parsers
  noPrimDefs =
    let ?allFuns = allFunMap
        ?allTypes = allTypesMap
    in concatMap cFun noCapFun


  -- Non-capturing roots
  noCapRoots      = [ f | f <- noCapFun, vmfIsEntry f ]
  (noCapRootSigs,noCapRootDefs) =
     let ?allFuns  = allFunMap
         ?allTypes = allTypesMap
     in unzip (map cNonCaptureRoot noCapRoots)

  -- Capturing roots
  capRoots                   = [ f | f <- capFuns, vmfIsEntry f ]
  (capEnts,capBlocks)        = unzip (zipWith cCaptureEntryDef [0..] capRoots)
  (cEntCode,cEntFuns,cEntTs) = unzip3 capEnts
  (capRootSigs,capRootDefs)  = unzip cEntFuns
  (capPrserSig,capParserDef,entTypeDef) =
    let ?allFuns = allFunMap
        ?allBlocks = allBlocks
        ?captures = Capture
        ?allTypes = allTypesMap
    in defineCaptureParser cEntTs cEntCode capFuns





  orderedModules = forgetRecs (topoOrder modDeps (pModules prog))
  modDeps m      = (mName m, Set.fromList (mImports m))

  allTypes       = concatMap mTypes orderedModules
  allTypesMap    = Map.fromList [ (Src.tName d, d) | d <- forgetRecs allTypes ]
  allFuns        = concatMap mFuns orderedModules
  allFunMap      = Map.fromList [ (vmfName f, f) | f <- allFuns ]

  (capFuns,noCapFun) = partition ((Capture ==) . vmfCaptures) allFuns

  allBlocks      = Map.unions
                 $ capBlocks ++
                   [ vmfBlocks d | f <- capFuns, VMDef d <- [vmfDef f] ]



includes :: Doc
includes =
  vcat [ "#include <ddl/parser.h>"
       , "#include <ddl/size.h>"
       , "#include <ddl/input.h>"
       , "#include <ddl/unit.h>"
       , "#include <ddl/bool.h>"
       , "#include <ddl/number.h>"
       , "#include <ddl/float.h>"
       , "#include <ddl/bitdata.h>"
       , "#include <ddl/integer.h>"
       , "#include <ddl/cast.h>"
       , "#include <ddl/maybe.h>"
       , "#include <ddl/array.h>"
       , "#include <ddl/map.h>"
       , "#include <ddl/owned.h>"
       , "#include <ddl/utils.h>"
       , "#include <optional>"
       ]


type AllFuns    = (?allFuns   :: Map Src.FName VMFun)
type AllTypes   = (?allTypes  :: Map Src.TName Src.TDecl)
type AllBlocks  = (?allBlocks :: Map Label Block)
type CurBlock   = (?curBlock  :: Block)
type Copies     = (?copies    :: Map BV E)
type CaptureFun = (?captures  :: Captures)




--------------------------------------------------------------------------------
-- Parsers that need to capture the stack

cCaptureParserSig :: Doc
cCaptureParserSig =
  "static void" <+>
  "parser(EntryArgs entry, DDL::ParseError &err, void* out)"


defineCaptureParser :: (AllFuns,AllTypes,AllBlocks,CaptureFun) =>
  [CDecl] -> [CExpr -> CStmt] -> [VMFun] -> (CDecl, CDecl, CDecl)
defineCaptureParser entTs ents capFuns
  | null ents   = (empty,empty,empty)
  | otherwise   = (cStmt cCaptureParserSig, def, cDeclareEntryArgs entTs)
  where
  def = cCaptureParserSig <+> "{" $$ nest 2 (vcat body) $$ "}"
  body =
    [ "DDL::ParserState p;"
    , "clang_bug_workaround: void *clang_bug = &&clang_bug_workaround;"
    , vcat (map cDeclareBlockParams (Map.elems ?allBlocks))
    , cDeclareRetVars capFuns
    , cDeclareClosures (Map.elems ?allBlocks)
    , " "
    , "// --- Entry points ---"
    , " "
    , cSwitch "entry.tag" ([ e "entry" | e <- ents ] ++ [ cDefault "return;" ])
    , " "
    , vcat' (map cBasicBlock (Map.elems ?allBlocks))
    ]



cDeclareBlockParams :: Block -> CStmt
cDeclareBlockParams b
  | null ps     = empty
  | otherwise   = vcat (header : ps)
  where
  header = "\n// Parameters for" <+> pp (blockName b)
  ps = [ cDeclareVar (cType (getType ba)) (cArgUse b ba) | ba <- blockArgs b ]

{- A function always returns 0 or 1 things, so it should be sufficient
to just have 1 varaible per type.
Alternatively, we could generate separate variables for each function.
-}
cDeclareRetVars :: [VMFun] -> CStmt
cDeclareRetVars funs = vcat (header : retInp : stmts)
  where
  header  = "\n// Varaibles used to return values from functions"
  stmts   = map decl $ Set.toList $ Set.fromList
                                  $ map (Src.fnameType . vmfName) funs
  decl t  = cDeclareVar (cSemType t) (cRetVar (TSem t))

  retInp  = cDeclareVar (cSemType Src.TStream) cRetInput


cDeclareClosures :: [Block] -> CStmt
cDeclareClosures bs =
    vcat' (header : map declareThr (Set.toList threadClos) ++
          map declareRet (Set.toList retClos))
  where
  header = "\n// Types for preserving variables across calls"

  (retClos,threadClos) = foldr addClo (Set.empty,Set.empty) bs

  addClo b (rets,threads) =
    let t  = blockType b
        n  = extraArgs t
        as = map getType (drop n (blockArgs b))
    in case t of
         NormalBlock    -> (rets,threads)
         ReturnBlock how ->
           case how of
             RetYes Capture -> (Set.insert as rets,threads)
             RetNo  Capture -> (Set.insert as rets,threads)
             _              -> (rets,threads)
         ThreadBlock    -> (rets, Set.insert as threads)

  declareRet ts = cClosureClass "DDL::Closure" (cReturnClassName ts) ts
  declareThr ts = cClosureClass "DDL::ThreadClosure"  (cThreadClassName ts) ts



--------------------------------------------------------------------------------
-- Entry Points

standardEntryArgs :: CType -> [Doc]
standardEntryArgs ty =
  [ "DDL::ParseError &error"
  , cInst "std::vector" [ ty ] <+> "&results"
  ]

cCaptureEntryFun :: Int -> Src.FName -> [VMT] -> (CDecl,CDecl)
cCaptureEntryFun n f as =
  (cStmt sig, sig <+> "{" $$ nest 2 body $$ "}")
  where
  nm   = cFEntryName f
  ty   = cSemType (Src.fnameType f)
  sig  = "void" <+> nm <+>
                  cArgBlock (standardEntryArgs ty ++ map (uncurry (<+>)) args)
  args = [ (cType t, "a" <.> int i) | (t,i) <- zip as [0..] ]
  ent  = vcat [ "{ .tag =" <+> int n
              , ", .args = { ." <.> capEntryName n <+> "= {"
                        <+> hcat (punctuate comma argVs) <+> "}}"
              , "}"
              ]
  argVs = [ "." <.> capArgName i <+> "=" <+> a | (i,(_,a)) <- zip [0..] args ]
  body = cStmt (cCall "parser" [ ent, "error", "&results" ])


cDeclareEntryArgs :: [CDecl] -> CDecl
cDeclareEntryArgs as = cStmt ("struct EntryArgs {" $$ nest 2 (vcat def) $$ "}")
  where
  def = [ cDeclareVar "int" "tag"
        , cStmt ("union {" $$ vcat as $$ "} args")
        ]

capArgName :: Int -> CIdent
capArgName i = "arg" <.> int i

capEntryName :: Int -> CIdent
capEntryName i = "entry" <.> int i

cCaptureEntryDef ::
  Int -> VMFun -> ((CExpr -> CStmt, (CDecl, CDecl), CDecl), Map Label Block)
cCaptureEntryDef n fun =
  ( (call, cCaptureEntryFun n name argTs, entTyDecl)
  , Map.fromList [ (blockName b, b) | b <- blocks ]
  )
  where
  call e = cCaseBlock (int n)
         $ doPush (Label nameText 0)
         : doPush (Label nameText 1)
         : zipWith (getArg e) [0..] args ++
           [ cGoto (cBlockLabel entryBlockName)
           ]

  doPush l =
    let clo = cCall (cReturnClassName []) ["&&" <.> cBlockLabel l]
    in cStmt (cCall "p.push" ["new" <+> clo])

  getArg ent a ba =
    cAssign (cArgUse entryBlock ba)
            (ent <.> ".args." <.> capEntryName n <.> "." <.> capArgName a)

  entTyDecl = cStmt $ vcat [ "struct {"
                           , nest 2 (vcat as)
                           , "}" <+> capEntryName n
                           ]
    where
    as = [ cDeclareVar (cType t) (capArgName i) | (i,t) <- zip [ 0 .. ] argTs ]


  name = vmfName fun
  nameText = case Src.fnameText name of
               Just txt -> "__" <> txt
               Nothing  -> panic "cCaptureEntryDef" [ "No name" ]

  ty   = TSem (Src.fnameType name)
  def  = case vmfDef fun of
           VMDef d     -> d
           VMExtern {} -> panic "cCaptureEntryDef" [ "Extern" ]

  entryBlockName = vmfEntry def
  entryBlock     = vmfBlocks def Map.! entryBlockName
  args           = blockArgs entryBlock
  argTs          = map getType args

  blocks =
    [ Block
        { blockName     = Label nameText 0
        , blockType     = ReturnBlock (RetNo Capture)
        , blockArgs     = []
        , blockLocalNum = 0
        , blockInstrs   = []
        , blockTerm     = Yield
        }

    , let v = BA 0 ty Owned
          i = BA 1 (TSem Src.TStream) Owned
      in Block
          { blockName     = Label nameText 1
          , blockType     = ReturnBlock (RetYes Capture)
          , blockArgs     = [v,i]
          , blockLocalNum = 0
          , blockInstrs   = [ Free (Set.singleton (ArgVar i))
                            , Output (EBlockArg v)
                            ]
          , blockTerm     = Yield
          }
    ]







-- Entry point for a non-capturing parser: siganture,defintiion
cNonCaptureRoot :: (AllTypes,AllFuns) => VMFun -> (CDecl,CDecl)
cNonCaptureRoot fun = (cStmt sig, sig <+> "{" $$ nest 2 (vcat body) $$ "}")
  where
  sig = "void" <+> cname <+>
                   cArgBlock (standardEntryArgs ty ++ map (uncurry (<+>)) pargs)

  cname = cFEntryName name
  name  = vmfName fun

  normalArgs = [ cType (getType x)
               | x <- case vmfDef fun of
                        VMDef d -> blockArgs (vmfBlocks d Map.! vmfEntry d)
                        VMExtern as -> as
               ]

  pargs = [ (t,"a" <> int n) | (t,n) <- zip normalArgs [ 1 .. ] ]

  ty   = cSemType (Src.fnameType name)

  call = cCall (cFName name)
               ([ "p", "&out_result", "&out_input" ] ++ map snd pargs)

  body = [ cDeclareVar "DDL::ParserState" "p"
         , cDeclareVar ty "out_result"
         , cDeclareVar "DDL::Input" "out_input"
         , cIf call
              [ cStmt (cCallMethod "results" "push_back" [ "out_result" ])
              , cStmt (cCallMethod "out_input" "free" [])
              ]
              [ cAssign "error" (cCallMethod "p" "getParseError" [])]
         ]



-- Declare the result of an entry
delcareEntryResults :: [VMFun] -> CDecl
delcareEntryResults es = cNamespace "DDL" [ cNamespace "ResultOf" (map ty es) ]
  where
  ty e = let n = vmfName e
         in cUsingT (cFEntryName n) (cSemType (Src.fnameType n))



--------------------------------------------------------------------------------
-- Non-capturing parsers

data FunLinkage = Static | Extern

cFunSig :: FunLinkage -> VMFun -> CDecl
cFunSig linkage fun = linkageStr <+> cDeclareFun res (cFName (vmfName fun)) args
  where
  linkageStr =
    case linkage of
      Static -> "static"
      Extern -> "extern"
  res  = if vmfPure fun then retTy else "bool"
  args = if vmfPure fun then normalArgs else retArgs ++ normalArgs
  normalArgs = [ cType (getType x)
               | x <- case vmfDef fun of
                        VMDef d -> blockArgs (vmfBlocks d Map.! vmfEntry d)
                        VMExtern as -> as
               ]

  retTy   = cSemType (Src.fnameType (vmfName fun))
  retArgs = [ "DDL::ParserState&", cPtrT retTy, cPtrT (cSemType Src.TStream) ]



cFun :: (AllTypes,AllFuns) => VMFun -> [CDecl]
cFun fun =
  case vmfDef fun of
    VMExtern {} -> []
    VMDef d
      | null args ->
        [ "static inline" $$ thisDef
        , cMemoValFun (vmfName fun)
        ]
      | otherwise -> [ thisDef ]
      where
      thisName = if null args then cFNameInit else cFName
      thisDef  = cDefineFun res (thisName (vmfName fun)) args body

      res
        | vmfPure fun    = retTy
        | otherwise      = "bool"

      args
        | vmfPure fun = normalArgs
        | otherwise = retArgs ++ normalArgs



      entryBlock = vmfBlocks d Map.! vmfEntry d

      argName n = "a" <.> int n

      normalArgs = [ cType (getType x) <+> argName n
                   | (x,n) <- blockArgs entryBlock `zip` [ 1.. ]
                   ]

      retTy   = cSemType (Src.fnameType (vmfName fun))
      retArgs = [ "DDL::ParserState& p"
                , cPtrT retTy <+> cRetVarFun
                , cPtrT (cSemType Src.TStream) <+> cRetInputFun
                ]

      body   = let ?allBlocks = vmfBlocks d
                   ?captures  = NoCapture
               in map cDeclareBlockParams (Map.elems (vmfBlocks d))
                 ++ [ cAssign (cArgUse entryBlock x) (argName n)
                    | (x,n) <- blockArgs entryBlock `zip` [ 1.. ]
                    ]
                 ++ cGoto (cBlockLabel (vmfEntry d))
                  : [ cBasicBlock b | b <- Map.elems (vmfBlocks d) ]


cMemoValFun :: FName -> CDecl
cMemoValFun f = cDefineFun retTy (cFName f) [] body
  where
  retTy  = cSemType (Src.fnameType f)
  var    = "result"
  memoTy = cInst "std::optional" [ cInst "DDL::Owned" [retTy]]

  body  = [ cDeclareVar ("static" <+> memoTy) var
          , cIf' ("!" <+> cCallMethod var "has_value" []) [
              cAssign var (cCallCon "DDL::Owned" [cCall (cFNameInit f) []])
            ]
          , cReturn (var <.> "->get()")
          ]



--------------------------------------------------------------------------------


cBasicBlock :: (AllTypes, AllFuns,AllBlocks,CaptureFun) => Block -> CStmt
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

              -- This block is used as the address to return to after calling a function.
              ReturnBlock how ->
                case how of
                  RetPure -> empty
                  RetNo NoCapture -> empty
                  RetYes NoCapture -> empty

                  -- Return with capture: manual stack manipulation
                  bty ->
                    let rn = extraArgs (ReturnBlock bty)
                        (ras,cas) = splitAt rn (blockArgs b)
                        ty = cPtrT (cReturnClassName (map getType cas))
                        regN i v = case how of
                                     RetYes Capture
                                        | i == (0::Int) -> cRetVar (getType v)
                                        | i == 1        -> cRetInput
                                     _ -> panic "getArgs.regN" ["invalid arg"]
                        resultN i v =
                          cAssign (cArgUse b v) (regN i v)
                    in
                    cBlock $
                      zipWith resultN [ 0.. ] ras ++
                      [ cDeclareInitVar ty "clo" (parens ty <.> cCall "p.pop" [])
                      ] ++
                      [ cStmt (cCall ("clo->get" <.> cField n) [ cArgUse b v ])
                      | (v,n) <- cas `zip` [ 0 .. ]
                      ] ++
                      [ cStmt (cCall "clo->free" []) ]



              ThreadBlock
                | Capture <- ?captures ->
                  let x : xs = blockArgs b
                      ty = cPtrT (cThreadClassName (map getType xs))
                  in
                  cBlock
                    $ cDeclareInitVar ty "clo" (parens ty <.> cCall "p.pop" [])
                    : cAssign (cArgUse b x)(cCall "DDL::Bool" ["clo->notified"])
                    : [ cStmt (cCall ("clo->get" <.> cField n) [ cArgUse b v ])
                      | (v,n) <- xs `zip` [ 0 .. ]
                      ]
                   ++ [ cStmt (cCall "clo->free" []) ]

                | otherwise -> panic "getArgs" ["Thread block in no-capture?"]





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
    Say x           -> cStmt (cCall "p.say"      [ cString x ])
    PushDebug x     -> cStmt (cCall "p.pushDebug" [ cString (Text.unpack x) ])
    PopDebug        -> cStmt (cCall "p.popDebug"  [])
    Output e        -> let t = cPtrT (cInst "std::vector" [ cType (getType e) ])
                           o = parens (parens(t) <.> "out")
                       in cStmt (cCall (o <.> "->push_back") [ cExpr e ])
    Notify e        -> cStmt (cCall "p.notify"   [ cExpr e ])
    NoteFail e      -> cStmt (cCall "p.noteFail" [ cExpr e ])
    Spawn x l       -> cVarDecl x (cCall "p.spawn" [clo])
      where clo = "new" <+> cCall (cThreadClassName (map getType (jArgs l)))
                    ("&&" <.> cBlockLabel (jLabel l) : map cExpr (jArgs l))

    Free xs         -> vcat (cFree xs)

    Let _ e
      | Just v <- eIsVar e -> cStmt (cCall (cVMVar v <.> ".copy") [])
      | otherwise          -> empty

    CallPrim x p es ->
      case p of
        StructCon _ut ->
          let v = cVarUse x
          in vcat [ cDeclareVar (cType (getType x)) v
                  , cStmt (cCallMethod v structCon
                          [ cExpr e | e <- es, getType e /= TSem Src.TUnit ])
                  ]

        NewBuilder _ -> cDeclareVar (cType (getType x)) (cVarUse x)
        Integer n    -> cVarDecl x (cCall "DDL::Integer" [ cString (show n) ])
        ByteArray bs -> cVarDecl x
                              (cCallCon "DDL::Array<DDL::UInt<8>>"
                                [ cCall "DDL::UInt<8>"
                                              [ text (show w) <> "UL" ]
                                  | w <- BS.unpack bs
                                ])
        Op1 op1      -> cOp1 x op1 es
        Op2 op2      -> cOp2 x op2 es
        Op3 op3      -> cOp3 x op3 es
        OpN opN      -> cOpN x opN es


cFree :: (CurBlock, Copies) => Set VMVar -> [CStmt]
cFree xs = [ cStmt (cCall (cVMVar y <.> ".free") [])
           | x <- Set.toList xs
           , y <- freeVar' x
           ]
  where
  freeVar' x =
    case x of
      LocalVar y | Just e <- Map.lookup y ?copies -> maybeToList (eIsVar e)
      _                                           -> [x]




cOp1 :: (Copies, CurBlock) => BV -> Src.Op1 -> [E] -> CStmt
cOp1 x op1 ~[e'] =
  case op1 of
    Src.CoerceTo tgtT
      | srcT == tgtT  -> cVarDecl x e
      | otherwise     -> cVarDecl x (cCall fun [e])
      where
      srcT = case getType e' of
               TSem t -> t
               _ -> bad "Expected a semantic type"

      bad :: String -> a
      bad msg = panic "cOp1" [ "Bad coercions"
                             , "from " ++ show (pp srcT)
                             , "to " ++ show (pp tgtT)
                             , msg
                             ]

      sz t = case t of
              Src.TSize i -> integer i
              _           -> bad "Size not an integer"

      fun =
        case srcT of

          Src.TUInt from ->
            case tgtT of
              Src.TInteger  -> cInst "DDL::uint_to_integer" [sz from]
              Src.TUInt to  -> cInst "DDL::uint_to_uint" [sz from, sz to]
              Src.TSInt to  -> cInst "DDL::uint_to_sint" [sz from, sz to]
              Src.TFloat    -> cInst "DDL::uint_to_float" [sz from]
              Src.TDouble   -> cInst "DDL::uint_to_double" [sz from]

              Src.TUser ut
                | let nm = Src.utName ut
                , Src.tnameBD nm -> cTNameUse GenPublic nm <.> "::fromBits"

              _             -> bad "Unexpected target type"

          Src.TSInt from ->
            case tgtT of
              Src.TInteger  -> cInst "DDL::sint_to_integer" [sz from]
              Src.TUInt to  -> cInst "DDL::sint_to_uint" [sz from, sz to]
              Src.TSInt to  -> cInst "DDL::sint_to_sint" [sz from, sz to]
              Src.TFloat    -> cInst "DDL::sint_to_float" [sz from]
              Src.TDouble   -> cInst "DDL::sint_to_double" [sz from]
              _             -> bad "Unexpected target type"

          Src.TInteger ->
            case tgtT of
              Src.TInteger  -> cInst "DDL::refl_cast" [cSemType tgtT]
              Src.TUInt to  -> cInst "DDL::integer_to_uint" [sz to]
              Src.TSInt to  -> cInst "DDL::integer_to_sint" [sz to]
              Src.TFloat    -> "DDL::integer_to_float"
              Src.TDouble   -> "DDL::integer_to_double"
              _             -> bad "Unexpected target type"

          Src.TFloat  ->
            case tgtT of
              Src.TInteger -> "DDL::float_to_integer"
              Src.TUInt to -> cInst "DDL::float_to_uint" [sz to]
              Src.TSInt to -> cInst "DDL::float_to_sint" [sz to]
              Src.TDouble  -> "DDL::float_to_double"
              _            -> bad "Unexpected target type"

          Src.TDouble ->
            case tgtT of
              Src.TInteger -> "DDL::double"
              Src.TUInt to -> cInst "DDL:double_to_uint" [sz to]
              Src.TSInt to -> cInst "DDL::double_to_sint" [sz to]
              Src.TFloat   -> "DDL::double_to_float"
              _            -> bad "Unexpected target type"




          Src.TUser ut
            | let nm = Src.utName ut
            , Src.tnameBD nm ->
              case tgtT of
                Src.TUInt {} -> "DDL::bitdata_to_uint"
                _ -> bad "Unexpected target type"

          _ | srcT == tgtT -> cInst "DDL::refl_cast" [cSemType tgtT]
            | otherwise    -> bad "Unexpected source type"

    Src.IsEmptyStream ->
      cVarDecl x $ cCallMethod e "length" [] <+> "==" <+> "0"

    Src.Head ->
      cVarDecl x $ cCall "DDL::UInt<8>" [ cCallMethod e "iHead" [] ]

    Src.StreamOffset ->
      cVarDecl x $ sizeTo64 (cCallMethod e "getOffset" [])

    Src.StreamLen ->
      cVarDecl x $ sizeTo64 (cCallMethod e "length" [])

    Src.BytesOfStream ->
      cVarDecl x $ cCallMethod e "getByteArray" []

    Src.OneOf bs ->
      let v     = cVarUse x
          true  = cAssign v (cCall "DDL::Bool" [ "true" ]) $$ cBreak
          false = cAssign v (cCall "DDL::Bool" [ "false" ])
      in
      vcat
        [ cDeclareVar (cType (getType x)) v
        , cSwitch (cCallMethod e "rep" [])
            $ [ cCase (int (fromEnum b)) true | b <- BS.unpack bs ]
           ++ [ cDefault false ]
        ]

    Src.Neg ->
      cVarDecl x $ "-" <> e

    Src.BitNot ->
      cVarDecl x $ "~" <> e

    Src.Not ->
      cVarDecl x $ "!" <> e

    Src.ArrayLen ->
      cVarDecl x $ sizeTo64 (cCallMethod e "size" [])

    Src.Concat ->
      cVarDecl x $ cCall (cType (getType x)) [ e ]

    Src.FinishBuilder ->
      cVarDecl x (cCall (cType (getType x)) [ e ])

    Src.NewIterator  ->
      cVarDecl x $ cCall (cType (getType x)) [ e ]

    Src.IteratorDone ->
      cVarDecl x $ cCallMethod e "done" []

    Src.IteratorKey  ->
      cVarDecl x $ cCallMethod e "key" []

    Src.IteratorVal ->
      cVarDecl x $ cCallMethod e "value" []

    Src.IteratorNext ->
      cVarDecl x $ cCallMethod e "next" []

    Src.EJust ->
      cVarDecl x $ cCall (cType (getType x)) [e]

    Src.FromJust ->
      cVarDecl x $ cCallMethod e "getValue" []

    Src.SelStruct _t l ->
      cVarDecl x $ cCallMethod e (selName GenOwn l) []

    Src.InUnion _ut l ->
      vcat [ cDeclareVar (cType (getType x)) (cVarUse x)
           , cStmt $ cCallMethod (cVarUse x) (unionCon l)
                                      [ e | getType e' /= TSem Src.TUnit ]
           ]

    Src.FromUnion _t l ->
      cVarDecl x $ cCallMethod e (selName GenOwn l) []

    Src.WordToFloat     -> cVarDecl x $ cCall "DDL::Float::fromBits" [
                                        cCallMethod e "rep" [] ]
    Src.WordToDouble    -> cVarDecl x $ cCall "DDL::Double::fromBits" [
                                        cCallMethod e "rep" [] ]
    Src.IsNaN           -> cVarDecl x $ cCallMethod e "isNaN" []
    Src.IsInfinite      -> cVarDecl x $ cCallMethod e "isInfinite" []
    Src.IsDenormalized  -> cVarDecl x $ cCallMethod e "isDenormalized" []
    Src.IsNegativeZero  -> cVarDecl x $ cCallMethod e "isNegativeZero" []

  where
  e = cExpr e'

sizeTo64 :: CExpr -> CExpr
sizeTo64 e = cCallCon "DDL::UInt<64>" [ cCallMethod e "rep" [] ]


cOp2 :: (Copies,CurBlock) => BV -> Src.Op2 -> [E] -> CDecl
cOp2 x op2 ~[e1',e2'] =
  case op2 of
    Src.IsPrefix -> cVarDecl x (cCallMethod e2 "hasPrefix" [ e1 ])
    Src.Drop     -> cVarDecl x (cCallMethod e2 "iDrop"    [ n ])
      where n = cCall "DDL::Size::from" [cCallMethod e1 "rep" []]
    Src.Take     -> cVarDecl x (cCallMethod e2 "iTake"    [ n ])
      where n = cCall "DDL::Size::from" [cCallMethod e1 "rep" []]

    Src.Eq    -> cVarDecl x $ cCallCon "DDL::Bool" [e1 <+> "==" <+> e2]
    Src.NotEq -> cVarDecl x $ cCallCon "DDL::Bool" [e1 <+> "!=" <+> e2]
    Src.Leq   -> cVarDecl x $ cCallCon "DDL::Bool" [e1 <+> "<=" <+> e2]
    Src.Lt    -> cVarDecl x $ cCallCon "DDL::Bool" [e1 <+> "<"  <+> e2]

    Src.Add   -> cVarDecl x (e1 <+> "+" <+> e2)
    Src.Sub   -> cVarDecl x (e1 <+> "-" <+> e2)
    Src.Mul   -> cVarDecl x (e1 <+> "*" <+> e2)
    Src.Div   -> cVarDecl x (e1 <+> "/" <+> e2)
    Src.Mod   -> cVarDecl x (e1 <+> "%" <+> e2)

    Src.BitAnd  -> cVarDecl x (e1 <+> "&" <+> e2)
    Src.BitOr   -> cVarDecl x (e1 <+> "|" <+> e2)
    Src.BitXor  -> cVarDecl x (e1 <+> "^" <+> e2)
    Src.Cat     -> cVarDecl x (cCall (cType (getType x)) [ e1, e2 ])
    Src.LCat    -> cVarDecl x (cCall "DDL::lcat" [ e1, e2 ])
    Src.LShift  -> cVarDecl x (e1 <+> "<<" <+> e2)
    Src.RShift  -> cVarDecl x (e1 <+> ">>" <+> e2)

    Src.ArrayIndex  -> cVarDecl x (cArraySelect e1 i)
      where i = cCall "DDL::Size::from" [cCallMethod e2 "rep" []]

    Src.Emit        -> cVarDecl x (cCall (cType (getType x)) [ e1, e2 ])
    Src.EmitArray   -> cVarDecl x (cCall (cType (getType x)) [ e1, e2 ])
    Src.EmitBuilder -> cVarDecl x (cCall (cType (getType x)) [ e1, e2 ])

    Src.ArrayStream -> cVarDecl x (cCall (cType (getType x)) [e1,e2])

    Src.MapLookup -> cVarDecl x (cCallMethod e1 "lookup" [e2])
    Src.MapMember ->
      cVarDecl x (cCallCon "DDL::Bool" [ cCallMethod e1 "contains" [e2] ])


  where
  e1   = cExpr e1'
  e2   = cExpr e2'


cOp3 :: (Copies,CurBlock) => BV -> Src.Op3 -> [E] -> CDecl
cOp3 x op es =
  case op of
    Src.RangeUp   -> range "rangeUp"
    Src.RangeDown -> range "rangeDown"
    Src.MapInsert -> cVarDecl x (cCallMethod e1 "insert" [ e2, e3 ])
  where
  range m = cVarDecl x (cCall (ty <.> "::" <.> m) [ e1, e2, e3 ])
  [e1,e2,e3] = map cExpr es
  ty = cType (getType x)



cOpN :: (Copies,CurBlock) => BV -> Src.OpN -> [E] -> CDecl
cOpN x op es =
  case op of
    Src.ArrayL t -> cVarDecl x (cCallCon con (map cExpr es))
      where con = cSemType (Src.TArray t)

    Src.CallF _  -> panic "cOpN" ["CallF"]




--------------------------------------------------------------------------------


cExpr :: (CurBlock,Copies) => E -> CExpr
cExpr expr =
  case expr of
    EBlockArg x   -> cArgUse ?curBlock x
    EVar x        -> case Map.lookup x ?copies of
                       Just e  -> cExpr e
                       Nothing -> cVarUse x
    EUnit         -> cCall "DDL::Unit" []
    EBool b       -> cCall "DDL::Bool" [if b then "true" else "false"]
    EFloat f t    -> cCall con [pp f]
        where con = case t of
                      Src.TFloat  -> "DDL::Float"
                      Src.TDouble -> "DDL::Double"
                      _           -> panic "cExpr" [ "Bad float" ]
    ENum n ty     -> cCallCon f [ arg ]
      where
      lit pref sz =
        pref <.>
        case sz of
          Src.TSize s
            | s <= 8    -> "INT8_C"
            | s <= 16   -> "INT16_C"
            | s <= 32   -> "INT32_C"
            | s <= 64   -> "INT64_C"
            | otherwise -> panic "cExpr" ["Literal > 64 bits"]
          Src.TSizeParam {} -> panic "cExpr" [ "Type variable in literal" ]
      mkArg cty = cCall cty [integer n]

      (f,arg) =
        case ty of
          Src.TUInt sz -> ( cInst "DDL::UInt"   [ cSizeType sz ]
                          , mkArg (lit "U" sz) )
          Src.TSInt sz -> ( cInst "DDL::SInt"   [ cSizeType sz ]
                          , mkArg (lit ""  sz) )
          Src.TFloat   -> ("DDL::Float", integer n)
          Src.TDouble  -> ("DDL::Double", integer n)

          _ -> panic "cExpr" [ "Unexpected type for numeric constant"
                             , show (pp ty) ]


    EMapEmpty k v -> cCallCon (cInst "DDL::Map" [ cSemType k, cSemType v ]) []
    ENothing t  -> parens (cCall (cInst "DDL::Maybe" [cSemType t]) [])




--------------------------------------------------------------------------------

cTermStmt :: (AllTypes, AllFuns, AllBlocks, CurBlock, Copies, CaptureFun) =>
  CInstr -> [CStmt]
cTermStmt ccInstr =
  case ccInstr of
    Jump jp -> cJump jp

    JumpIf e (JumpCase opts) -> cDoCase e opts

    Yield ->
      [ cIf (cCall "p.hasSuspended" [])
          [ cGoto ("*" <.> cCall "p.yield" []) ]
          [ cAssign "err.offset" "p.finalYield()", "return;" ]
      ]

    ReturnNo ->
      case ?captures of
        Capture   -> [ cGoto ("*" <.> cCall "p.returnNo" []) ]
        NoCapture -> [ "return false;" ]
        Unknown   -> panic "cTermStmt" ["Unknown"]

    ReturnYes e i ->
      case ?captures of
        Capture ->
          [ cAssign (cRetVar (getType e)) (cExpr e)
          , cAssign cRetInput (cExpr i)
          , cGoto ("*" <.> cCall "p.returnYes" [])
          ]
        NoCapture ->
          [ cAssign ("*" <.> cRetVarFun)   (cExpr e)
          , cAssign ("*" <.> cRetInputFun) (cExpr i)
          , "return true;"
          ]
        Unknown   -> panic "cTermStmt" ["Unknown"]

    ReturnPure e ->
      case ?captures of
        Capture ->
          [ cAssign (cRetVar (getType e)) (cExpr e)
          , cGoto ("*" <.> cCall "p.returnPure" [])
          ]
        NoCapture ->
          [ cStmt ("return" <+> cExpr e) ]
        Unknown   -> panic "cTermStmt" ["Unknown"]

    Call f captures no yes es ->
      case captures of
        Unknown   -> panic "cTermStmt" ["Unknown"]

        Capture ->
            doPush no
          : doPush yes
          : case vmfDef (lkpFun f) of
              VMDef d -> cJump (JumpPoint (vmfEntry d) es)
              VMExtern {} -> panic "Capture call to extern" []

        NoCapture ->
          let JumpPoint lYes esYes = yes
              JumpPoint lNo esNo = no
              bYes = ?allBlocks Map.! lYes
              bNo  = ?allBlocks Map.! lNo
              a : i : _ = blockArgs bYes
              call = cCall (cFName f)
                   $ "p"
                   : ("&" <.> cArgUse bYes a)
                   : ("&" <.> cArgUse bYes i)
                   : map cExpr es
          in [ cIf call
                  (freeClo no  ++ cDoJump bYes esYes)
                  (freeClo yes ++ cDoJump bNo  esNo)
            ]

          where freeClo (JumpPoint _ vs) =
                   [ cStmt (cCallMethod (cExpr e) "free" [])
                   | e <- vs, typeRepOf e == HasRefs ]

    -- XXX: this one does not need to be terminal
    CallPure f (JumpPoint lab les) es ->
      case Map.lookup lab ?allBlocks of
        Just b -> zipWith assignP (blockArgs b) (doCall : map cExpr les) ++
                  [ cGoto (cBlockLabel (blockName b)) ]
          where assignP ba = cAssign (cArgUse b ba)
                doCall = cCall (cFName f) (map cExpr es)
        Nothing -> panic "CallPure" [ "Missing block: " ++ show (pp lab) ]


    TailCall f captures es ->
      let fun       = lkpFun f
          retT      = TSem (Src.fnameType f)
          doCall as = cCall (cFName f) (as ++ map cExpr es)
      in
      case (captures, ?captures) of
        (Capture,Capture) ->
          case vmfDef fun of
            VMDef d  ->  cJump (JumpPoint (vmfEntry d) es)
            VMExtern _ -> panic "Tail call" ["Capturing primitive?", showPP f]
        (Capture,NoCapture) -> panic "cBasicBlock" [ "Capture from no-capture" ]

        -- this is not a tail call anymore
        (NoCapture,Capture)
          | vmfPure fun ->
            [ cAssign (cRetVar retT) (doCall [])
            , cGoto ("*" <.> cCall "p.returnPure" [])
            ]

          | otherwise ->
            [ cIf (doCall [ "p", "&" <.> cRetVar retT, "&" <.> cRetInput ])
                  [cGoto ("*" <.> cCall "p.returnYes" [])]
                  [cGoto ("*" <.> cCall "p.returnNo" [])]
            ]

        (NoCapture,NoCapture) ->
            [ cStmt $ "return" <+> doCall args ]

          where
          args
            | vmfPure fun = []
            | otherwise   = [ "p", cRetVarFun, cRetInputFun ]

        (Unknown,_) ->  panic "cTermStmt" ["Unknown"]
        (_,Unknown) ->  panic "cTermStmt" ["Unknown"]

  where
  lkpFun f = case Map.lookup f ?allFuns of
               Just fun -> fun
               Nothing  -> panic "cTermStmt" [ "Unknown function", show (pp f) ]

  doPush l =
    let clo = cCall (cReturnClassName (map getType (jArgs l)))
                    ("&&" <.> cBlockLabel (jLabel l) : map cExpr (jArgs l))
    in cStmt (cCall "p.push" ["new" <+> clo])


cDoJump :: (Copies,CurBlock,CaptureFun) => Block -> [E] -> [CStmt]
cDoJump b es =
  zipWith assignP as es ++ [ cGoto (cBlockLabel l) ]
  where
  as           = drop (extraArgs (blockType b)) (blockArgs b)
  l            = blockName b
  assignP ba e = cAssign (cArgUse b ba) (cExpr e)


cJump :: (AllBlocks, CurBlock, Copies,CaptureFun) => JumpPoint -> [CStmt]
cJump (JumpPoint l es) =
  case Map.lookup l ?allBlocks of
    Just b  -> cDoJump b es
    Nothing -> panic "cJump" [ "Missing block: " ++ show (pp l) ]

cDoCase :: (AllTypes, AllFuns, AllBlocks, CurBlock, Copies, CaptureFun) =>
           E -> Map Pattern JumpWithFree -> [CStmt]
cDoCase e opts =
  case getType e of
    TSem Src.TBool ->
      check
      do ifTrue  <- lkpOrDflt (PBool True)
         ifFalse <- lkpOrDflt (PBool False)
         pure [ cIf (cCallMethod (cExpr e) "getValue" [])
                    (doChoice ifTrue) (doChoice ifFalse) ]

    TSem (Src.TMaybe _) ->
      check
      do ifNothing <- lkpOrDflt PNothing
         ifJust    <- lkpOrDflt PJust
         pure [ cIf (cCallMethod (cExpr e) "isJust" [])
                    (doChoice ifJust) (doChoice ifNothing) ]

    TSem (Src.TInteger) -> mkBigInt e opts

    TSem (Src.TUInt _)  -> mkSwitch "rep" numPat
    TSem (Src.TSInt _)  -> mkSwitch "rep" numPat
    TSem (Src.TUser ut)
      | Src.tnameBD (Src.utName ut) -> mkBDSwitch ut
      | otherwise -> mkSwitch "getTag" conPat

    ty -> panic "JumpIf" [ "`case` on unexpected type", show (pp ty) ]


  where
  dflt        = Map.lookup PAny opts
  lkpOrDflt p = Map.lookup p opts <|> dflt
  doChoice ch = cFree (freeFirst ch) ++ cJump (jumpTarget ch)

  check = fromMaybe
            (panic "JumpIf" ["Invalid case", "Type: " ++ show (pp (getType e))])

  numPat p = case p of
               PNum n -> integer n
               _ -> panic "numPat" [ "Unexpected", show (pp p) ]

  conPat ~(PCon l) =
    case getType e of
      TSem (Src.TUser ut) -> cSumTagV (Src.utName ut) l
      _ -> panic "cDoCase" [ "conPat on non-user type" ]

  mkBDSwitch ut =
    case Map.lookup (Src.utName ut) ?allTypes of
      Just d ->
        case Src.tDef d of
          Src.TBitdata univ (Src.BDUnion fs) ->
            [
            let alts = [ (t, vcat (doChoice ch))
                       | (PCon l,ch) <- Map.toList opts
                       , Just t <- [lookup l fs]
                       ]
            in case dflt of
                 Nothing -> bdCase False ?allTypes univ (cExpr e) alts
                 Just x  -> bdCaseDflt False ?allTypes univ (cExpr e) alts
                                                         (vcat (doChoice x))
            ]
          _ -> panic "mkBDSwitch" [ "Invalid switch", showPP ut ]
      _ -> panic "mkBDSwitch" [ "Missing type", showPP ut ]

  mkSwitch getNum pToCase =
    let addDflt cs = case dflt of
                       Nothing -> cs
                       Just x  -> cs ++ [cDefault $ vcat (doChoice x)]
    in [ cSwitch (cCallMethod (cExpr e) getNum []) $
           addDflt
            [ cCase (pToCase pat) (vcat (doChoice ch))
                            | (pat, ch) <- Map.toList opts, pat /= PAny ]
       ]

  mkBigInt = compileBigInteITE

data Tree a = Tip | One a | Node a (Tree a) (Tree a)
  deriving Show


foldTree :: b -> (a -> b) -> (a -> b -> b -> b) -> Tree a -> b
foldTree tip one node = go
  where
  go t =
    case t of
      Tip -> tip
      One a -> one a
      Node a l r -> node a (go l) (go r)

listToTree :: [a] -> Tree a
listToTree xs0 = go (length xs0) xs0
  where
  go sz xs =
    let n = div sz 2
    in case splitAt n xs of
         (_,[])    -> Tip
         ([],[r])  -> One r
         (ls,r:rs) -> Node r (go n ls) (go (sz - n - 1) rs)



compileBigInteITE ::
  (AllFuns, AllBlocks, CurBlock, Copies, CaptureFun) =>
  E -> Map Pattern JumpWithFree -> [CStmt]

compileBigInteITE e alts = foldTree dflt mkOne mkIf opts
  where
  ce   = cExpr e

  dflt = case Map.lookup PAny alts of
         Just ch -> doChoice ch
         Nothing -> panic "compileBigInteITE" [ "Mssing default case" ]

  opts = listToTree
       $ sortBy (compare `on` fst)
           [ (n, doChoice ch) | (PNum n, ch) <- Map.toList
                                                  (Map.delete PAny  alts) ]

  doChoice ch = cFree (freeFirst ch) ++ cJump (jumpTarget ch)

  mkOne (i,ch) =
    prep i ++
    [ cIf (checkVarName <+> "==" <+> "0") ch dflt ]

  mkIf (i,ch) ifSmaller ifLarger =
    prep i ++
    [ cIf (checkVarName <+> "==" <+> "0")
        ch
        [ cIf (checkVarName <+> "<" <+> "0") ifSmaller ifLarger ]
    ]

  checkVarName = "c" -- XXX
  ivarName = "i" -- XXX

  prep i =
    pref ++
    [ cDeclareInitVar "int" checkVarName
                      (cCall "DDL::compare" [ ce, fromMaybe ivarName mbK ]) 
    ] ++
    suff
    where
    inRng lower =
      toInteger lower <= i && i <= toInteger (maxBound `asTypeOf` lower)


    mbK
      | inRng (minBound :: Word32) = Just (cCall "UINT32_C" [integer i])
      | inRng (minBound :: Int32)  = Just (cCall "INT32_C" [integer i])
      | inRng (minBound :: Word64) = Just (cCall "UINT64_C" [integer i])
      | inRng (minBound :: Int64)  = Just (cCall "INT64_C" [integer i])
      | otherwise                  = Nothing

    (pref,suff) =
       case mbK of
         Just _  -> ([],[])
         Nothing ->
           ( [ cDeclareConVar "DDL::Integer" ivarName
                                 [cString (show i)] ]
           , [ cStmt (cCallMethod ivarName "free" []) ]
           )


