 {-# LANGUAGE OverloadedStrings, TupleSections, GADTs, DataKinds #-}

module Daedalus.ParserGen.Generate where

import Language.C
import Control.Monad.State as SM

import Daedalus.ParserGen.AST
import Daedalus.ParserGen.Aut as Aut
import Daedalus.ParserGen.Action as Action

import qualified Daedalus.Type.AST as DAST

import Daedalus.Interp

import Data.Text (unpack)
import Data.Array as Arr
import Text.PrettyPrint

import Debug.Trace

-- State for the generator
data CAutGenData = CAutGenData {
  names :: [Name],
  actionId :: Integer,
  expressionId :: Integer,
  declarations :: [CExtDecl]
}

emptyAutGenData :: CAutGenData
emptyAutGenData = CAutGenData {
  names = [],
  actionId = 0,
  expressionId = 0,
  declarations = []
}

-- The generator monad
type CAutGenM = SM.State CAutGenData

-- Return a new Name instance for use
nextName :: CAutGenM Name
nextName = do
  st <- get
  let n = head $ names st
  put $ st { names = tail $ names st }
  return n

nextActionId :: CAutGenM Integer
nextActionId = do
  st <- get
  let v = actionId st
  put $ st { actionId = v + 1 }
  return v

nextExpressionId :: CAutGenM Integer
nextExpressionId = do
  st <- get
  let v = expressionId st
  put $ st { expressionId = v + 1 }
  return v

addDeclaration :: CExtDecl -> CAutGenM ()
addDeclaration decl = modify' (\s -> s { declarations = declarations s ++ [decl] })

addDeclarations :: [CExtDecl] -> CAutGenM ()
addDeclarations decls = modify' (\s -> s { declarations = declarations s ++ decls })

-- Get a new named identifier
makeIdent :: String -> CAutGenM Ident
makeIdent s = mkIdent nopos s <$> nextName

makeEnumConstantExpr :: String -> CAutGenM CExpr
makeEnumConstantExpr s = CVar <$> makeIdent s <*> pure undefNode

-- Get a type specification for a typedef-ed type.
-- NOTE: Almost every struct in the C runtime is typedef-ed. Hence this function!
makeTypeDefType :: String -> CAutGenM CTypeSpec
makeTypeDefType s = do
  ident <- makeIdent s
  return $ CTypeDef ident undefNode

-- Create a named variable thingie that is suitable to use as part of a declaration
-- and return that along with the identifier we just created
makeVarDeclr :: String -> CAutGenM (Ident, CDeclr)
makeVarDeclr s = do
  ident <- makeIdent s
  let decl = CDeclr (Just ident) [] Nothing [] undefNode
  return (ident, decl)

makeArrayVarDeclr :: String -> Int -> CAutGenM (Ident, CDeclr)
makeArrayVarDeclr s len = do
  ident <- makeIdent s
  let arraySizeExpr = CArrSize False $ makeIntConstExpr len
  let arrayDerivedDeclr = CArrDeclr [] arraySizeExpr undefNode
  let decl = CDeclr (Just ident) [arrayDerivedDeclr] Nothing [] undefNode
  return (ident, decl)

-- Make an expression out of a constant integer
makeIntConstExpr :: Int -> CExpr
makeIntConstExpr v = CConst $ CIntConst (cInteger $ toInteger v) undefNode

makeStringConstExpr :: String -> CExpr
makeStringConstExpr s = CConst $ CStrConst (cString s) undefNode

-- Make an expression out of a Name
makeNameConstExpr :: Maybe DAST.Name -> CExpr
makeNameConstExpr ms =
  case ms of
    Just s  -> CConst $ CStrConst (cString $ name2String s) undefNode
    Nothing -> makeIntConstExpr 0
  where
    name2String n = do
      let x = DAST.nameScopedIdent n
      case x of
        DAST.Unknown ident -> unpack ident
        DAST.Local   ident -> unpack ident
        DAST.ModScope _ ident -> unpack ident

makeAddrOfExpr :: CExpr -> CExpr
makeAddrOfExpr e = CUnary CAdrOp e undefNode

makeAddrOfIdent :: Ident -> CExpr
makeAddrOfIdent ident = CUnary CAdrOp (CVar ident undefNode) undefNode

makeFieldInitializer :: String -> CExpr -> CAutGenM ([CDesignator], CInit)
makeFieldInitializer name expr = do
  initIdent <- makeIdent name
  return ([CMemberDesig initIdent undefNode], CInitExpr expr undefNode)

makeStructInitializer :: [(String, CExpr)] -> CAutGenM CInit
makeStructInitializer nameExprList = do
    inits <- mapM (uncurry makeFieldInitializer) nameExprList
    return $ CInitList inits undefNode

makeNestedStructInitializer :: [(String, CInit)] -> CAutGenM CInit
makeNestedStructInitializer nameInitList = do
  inits <- mapM mapInitializers nameInitList
  return $ CInitList inits undefNode
  where
    mapInitializers (name, initializer) = do
      ident <- makeIdent name
      return ([CMemberDesig ident undefNode], initializer)

makeArrayInitializer :: [CInit] -> CAutGenM CInit
makeArrayInitializer ilist = do
  let inits = map ([], ) ilist
  return $ CInitList inits undefNode

-- Make a tagged union initializer. Essentially something that looks like this
-- { .tag = <tagEnum>,  <fieldName> = <fieldInit> }
makeTaggedUnionInitializer :: String -> String -> CInit -> CAutGenM CInit
makeTaggedUnionInitializer tagEnum fieldName fieldInit = do
  tagExpr <- makeEnumConstantExpr tagEnum
  let tagInit = CInitExpr tagExpr undefNode
  makeNestedStructInitializer [("tag", tagInit), (fieldName, fieldInit)]


generateIO :: ArrayAut -> IO CTranslUnit
generateIO aut = do
  let (v, _) = runState (generateNFA aut) emptyAutGenData
  return v

generateTextIO :: ArrayAut -> IO String
generateTextIO aut = do
  tunit <- generateIO aut
  let t = render $ pretty tunit
  return t

-- The top-level function to generate a C language version of the parser-gen created automata.
-- NOTE: For simplicity and code reuse reasons, this takes an ArrayAut as input instead of
-- the typeclass Aut.
-- TODO: Reconsider this
generateNFA :: ArrayAut -> CAutGenM CTranslUnit
generateNFA aut = do
  generateNFADeclarations aut
  decls <- gets declarations
  return $ CTranslUnit decls undefNode

generateNFADeclarations :: ArrayAut -> CAutGenM ()
generateNFADeclarations aut = sequence_ [generateNFAAutDeclaration aut]

generateNFAAutDeclaration :: ArrayAut -> CAutGenM ()
generateNFAAutDeclaration aut = do
  -- NOTE: Type name MUST match the code for the Automata structure in aut.h
  -- and the definition in aut.h must be a typedef-ed struct
  autType <- CTypeSpec <$> makeTypeDefType "Aut"
  (_, autVarDeclr)  <- makeVarDeclr "aut"
  tableIdent <- generateNFAAutTable aut
  let tableExpr = CVar tableIdent undefNode
  fieldsInit <-
    makeStructInitializer
      [ ("initial", initStateExpr)
      , ("table", tableExpr)
      , ("accepting", finalStateExpr)
      ]
  let decl = CDeclExt $ CDecl [autType] [(Just autVarDeclr, Just fieldsInit, Nothing)] undefNode
  addDeclaration decl
  where
    initStateExpr = makeIntConstExpr $ initialState aut
    finalStateExpr = makeIntConstExpr $ acceptings $ mapAut aut


generateNFAAutTable :: ArrayAut -> CAutGenM Ident
generateNFAAutTable aut = do
  initializers <- mapM generateInitializer fullIndexedTransitions
  initializer <- makeArrayInitializer initializers
  (ident, declr) <- makeArrayVarDeclr "trs" (length fullIndexedTransitions)
  declType <- CTypeSpec <$> makeTypeDefType "Choice"
  let decl = CDeclExt $ CDecl [declType] [(Just declr, Just initializer, Nothing)] undefNode
  addDeclaration decl
  return ident
  where
    fullIndexedTransitions = do
      -- NOTE: We are making use of the fact that the ArrayAut's transitionArray
      -- has an entry for every index
      -- TODO: Fix this to not rely on the internals of this implementation
      Arr.assocs $ transitionArray aut
    generateInitializer (_, Nothing) = do
      tagExpr <- makeEnumConstantExpr "EMPTYCHOICE"
      makeStructInitializer [("tag", tagExpr)]
    generateInitializer (st, Just choice) = do
      -- Use state as the tag for the identifier
      (tag, cnt, ident) <- generateActionStatePairs st choice
      tagExpr <- makeEnumConstantExpr tag
      let lenExpr = makeIntConstExpr cnt
      let addrExpr = CVar ident undefNode
      makeStructInitializer [("tag", tagExpr), ("len", lenExpr), ("transitions", addrExpr)]

generateActionStatePairs :: Int -> Choice -> CAutGenM (String, Int, Ident)
generateActionStatePairs identTag choice = do
  let varName = transitionName
  initializers <- case choice of
    UniChoice (action, st) -> sequence [generateActionStatePair action st]
    SeqChoice actionStateList _ -> mapM (uncurry generateActionStatePair) actionStateList
    ParChoice actionStateList -> mapM (uncurry generateActionStatePair) actionStateList
  (cnt, ident) <- createDeclaration varName initializers
  return (choiceExpr choice, cnt, ident)
  where
    createDeclaration :: String -> [CInit] -> CAutGenM (Int, Ident)
    createDeclaration s initializers = do
      (ident, declr) <- makeArrayVarDeclr s (length initializers)
      declType <- CTypeSpec <$> makeTypeDefType "ActionStatePair"
      initList <- makeArrayInitializer initializers
      let decl = CDecl [declType] [(Just declr, Just initList, Nothing)] undefNode
      addDeclaration $ CDeclExt decl
      return (length initializers, ident)
    transitionName = "tr" ++ show identTag
    choiceExpr (UniChoice _) = "UNICHOICE"
    choiceExpr (SeqChoice _ _) = "SEQCHOICE"
    choiceExpr (ParChoice _) = "PARCHOICE"

generateActionStatePair :: Action -> Action.State -> CAutGenM CInit
generateActionStatePair action st = do
  ident <- generateAction action
  let actionExpr = makeAddrOfIdent ident
  let stateExpr = makeIntConstExpr st
  makeStructInitializer [("pAction", actionExpr), ("state", stateExpr)]

generateAction :: Action -> CAutGenM Ident
generateAction act = do
  (actVarIdent, actVarDeclr)  <- makeActionVar
  actType <- CTypeSpec <$> makeTypeDefType "Action"
  actInit <- generateActionData act
  let decl = CDeclExt $ CDecl [actType] [(Just actVarDeclr, Just actInit, Nothing)] undefNode
  addDeclaration decl
  return actVarIdent
  where
    makeActionVar = do
      n <- nextActionId
      let name = "a" ++ (show n)
      makeVarDeclr name

generateActionData :: Action -> CAutGenM CInit
generateActionData (IAct ip) = do
  dataInit <- generateInputActionData ip
  makeTaggedUnionInitializer "ACT_InputAction" "inputAction" dataInit
generateActionData (CAct cp) =  do
  dataInit <- generateControlActionData cp
  makeTaggedUnionInitializer "ACT_ControlAction" "controlAction" dataInit
generateActionData (SAct sp) =  do
  dataInit <- generateSemanticActionData sp
  makeTaggedUnionInitializer "ACT_SemanticAction" "semanticAction" dataInit
generateActionData (BAct sp) =  do
  dataInit <- generateBranchActionData sp
  makeTaggedUnionInitializer "ACT_BranchAction" "branchAction" dataInit
generateActionData EpsA       = generateEpsAction

-- Generate an C initializer for an InputAction
generateInputActionData :: InputAction -> CAutGenM CInit
generateInputActionData IEnd = do
  tagExpr <- makeEnumConstantExpr "ACT_IEnd"
  makeStructInitializer [("tag", tagExpr)]
generateInputActionData (IMatchBytes _withsem e) = do
  -- First create the initializer for the IMatchBytesData datastructure
  let withSemExpr = makeIntConstExpr (if _withsem == DAST.YesSem then 1 else 0)
  matchExpr <- generateVExpr e
  dataInitializer <- makeStructInitializer [("withsem", withSemExpr), ("expr", matchExpr)]
  -- Now the initializer for the outer tagged union structure
  makeTaggedUnionInitializer "ACT_IMatchBytes" "iMatchBytesData" dataInitializer
generateInputActionData (ClssAct _withsem e) = do
  -- NOTE: We are only handling a single character read at this point and
  -- are doing some really specific hackery for the purpose
  -- TODO: Fix this!
  let v = case DAST.texprValue e of
            DAST.TCSetSingle e' -> evalNoFunCall e' [] []
            _ -> error $ "Unimplemented expression in ClssAct evaluation : " ++ show e
  case v of
    VUInt _b iv -> do
      -- First create the initializer for the ReadCharData datastructure
      let valExpr = makeIntConstExpr $ fromIntegral iv
      dataInitializer <- makeStructInitializer [("chr", valExpr)]
      -- Now the initializer for the outer tagged union structure
      makeTaggedUnionInitializer "ACT_Temp_ReadChar" "readCharData" dataInitializer
    _ -> return $ error $ "Unimplemented ClssAct matching : " ++ show v
generateInputActionData x = return $ error $ "Unimplemented action: " ++ show x

generateSemanticActionData :: SemanticAction -> CAutGenM CInit
generateSemanticActionData EnvFresh = do
  tagExpr <- makeEnumConstantExpr "ACT_EnvFresh"
  makeStructInitializer [("tag", tagExpr)]
generateSemanticActionData (EnvStore nm) = do
  -- First create the initializer for the ReadCharData datastructure
  dataInitializer <- makeStructInitializer [("name", makeNameConstExpr nm)]
  -- Now the initializer for the outer tagged union structure
  makeTaggedUnionInitializer "ACT_EnvStore" "envStoreData" dataInitializer
generateSemanticActionData (ReturnBind e) = do
  -- First create the initializer for the ReturnBindData datastructure
  expr <- generateVExpr e
  dataInitializer <- makeStructInitializer [("expr", expr)]
  -- Now the initializer for the outer tagged union structure
  makeTaggedUnionInitializer "ACT_ReturnBind" "returnBindData" dataInitializer
generateSemanticActionData (ManyFreshList s) = do
  let withSemExpr = makeIntConstExpr (if s == DAST.YesSem then 1 else 0)
  dataInitializer <- makeStructInitializer [("withsem", withSemExpr)]
  makeTaggedUnionInitializer "ACT_ManyFreshList" "manyFreshListData" dataInitializer
generateSemanticActionData (ManyAppend s) = do
  let withSemExpr = makeIntConstExpr (if s == DAST.YesSem then 1 else 0)
  dataInitializer <- makeStructInitializer [("withsem", withSemExpr)]
  makeTaggedUnionInitializer "ACT_ManyAppend" "manyAppendData" dataInitializer
generateSemanticActionData ManyReturn = do
  tagExpr <- makeEnumConstantExpr "ACT_ManyReturn"
  makeStructInitializer [("tag", tagExpr)]
generateSemanticActionData DropOneOut = do
  tagExpr <- makeEnumConstantExpr "ACT_DropOneOut"
  makeStructInitializer [("tag", tagExpr)]
generateSemanticActionData x = return $ error $ "Unimplemented action: " ++ show x

generateControlActionData :: ControlAction -> CAutGenM CInit
generateControlActionData (Push _name _le q) = do
  -- First create the initializer for the PushData datastructure
  let stateExpr = makeIntConstExpr q
  let nameExpr = makeNameConstExpr $ Just _name
  dataInitializer <- makeStructInitializer [("name", nameExpr), ("state", stateExpr)]
  -- Now the initializer for the outer tagged union structure
  makeTaggedUnionInitializer "ACT_Push" "pushData" dataInitializer
generateControlActionData Pop = do
  tagExpr <- makeEnumConstantExpr "ACT_Pop"
  makeStructInitializer [("tag", tagExpr)]
generateControlActionData (ActivateFrame _ln) = do
  -- First create the initializer for the ActivateFrameData datastructure
  -- TODO: Fix this - Only NULL for now. How to represent a list of names or is something else appropriate here?
  let nameListExpr = makeIntConstExpr 0
  dataInitializer <- makeStructInitializer [("name", nameListExpr)]
  -- Now the initializer for the outer tagged union structure
  makeTaggedUnionInitializer "ACT_ActivateFrame" "activateFrameData" dataInitializer
generateControlActionData DeactivateReady = do
  tagExpr <- makeEnumConstantExpr "ACT_DeactivateReady"
  makeStructInitializer [("tag", tagExpr)]
generateControlActionData (BoundSetup bound) = do
  dataInitializer <- generateBoundSetupData bound
  makeTaggedUnionInitializer "ACT_BoundSetup" "boundSetupData" dataInitializer
  where
    generateBoundSetupData (DAST.Exactly e) = do
      expr <- generateVExpr e
      dataInitializer <- makeStructInitializer [("expr", expr)]
      makeTaggedUnionInitializer "ACT_Exactly" "exactlyData" dataInitializer
    generateBoundSetupData (DAST.Between maybeE1 maybeE2) = do
      expr1 <- exprOrNull maybeE1
      expr2 <- exprOrNull maybeE2
      dataInitializer <- makeStructInitializer [("left", expr1), ("right", expr2)]
      makeTaggedUnionInitializer "ACT_Between" "betweenData" dataInitializer
    exprOrNull Nothing = return $ makeIntConstExpr 0
    exprOrNull (Just e) = generateVExpr e
generateControlActionData BoundCheckSuccess = do
  tagExpr <- makeEnumConstantExpr "ACT_BoundCheckSuccess"
  makeStructInitializer [("tag", tagExpr)]
generateControlActionData BoundIsMore = do
  tagExpr <- makeEnumConstantExpr "ACT_BoundIsMore"
  makeStructInitializer [("tag", tagExpr)]
generateControlActionData BoundIncr = do
  tagExpr <- makeEnumConstantExpr "ACT_BoundIncr"
  makeStructInitializer [("tag", tagExpr)]
generateControlActionData x = return $ error $ "Unimplemented action: " ++ show x

generateBranchActionData :: BranchAction -> CAutGenM CInit
generateBranchActionData (CutBiasAlt q) = do
  -- First create the initializer for the PushData datastructure
  let stateExpr = makeIntConstExpr q
  dataInitializer <- makeStructInitializer [("state", stateExpr)]
  -- Now the initializer for the outer tagged union structure
  makeTaggedUnionInitializer "ACT_CutBiasAlt" "cutBiasAltData" dataInitializer
generateBranchActionData x = return $ error $ "Unimplemented action: " ++ show x


generateVExpr :: NVExpr -> CAutGenM CExpr
generateVExpr e = do
  (exprVarIdent, exprVarDeclr)  <- makeExprVar
  exprType <- CTypeSpec <$> makeTypeDefType "VExpr"
  exprInit <- exprInitializer
  let decl = CDeclExt $ CDecl [exprType] [(Just exprVarDeclr, Just exprInit, Nothing)] undefNode
  addDeclaration decl
  return $ makeAddrOfIdent exprVarIdent
  where
    makeExprVar = do
      n <- nextExpressionId
      let name = "e" ++ (show n)
      makeVarDeclr name
    exprInitializer = do
      designatorInitList <- generateVExprData e
      makeStructInitializer designatorInitList

generateVExprData :: NVExpr -> CAutGenM [(String, CExpr)]
generateVExprData e = do
  traceShow e $ case DAST.texprValue e of
    DAST.TCVar v -> do
      tagExpr <- makeEnumConstantExpr "E_VAR";
      return $ [("tag", tagExpr), ("name", nameExpr v)]
    DAST.TCIn _ e1 _ ->
      -- Right now we only generate things corresponding to the inner expression
      generateVExprData e1
    _ -> do
      tagExpr <- makeEnumConstantExpr "E_INT";
      return $ [("tag", tagExpr), ("name", makeIntConstExpr 0)]
  where
    nameExpr nm = makeNameConstExpr $ Just $ DAST.tcName nm


generateEpsAction :: CAutGenM CInit
generateEpsAction = do
  tagExpr <- makeEnumConstantExpr "ACT_EpsA"
  makeStructInitializer [("tag", tagExpr)]


--   let varDecl = CDeclr (Just autVar) [] Nothing [] undefNode

--   ff <- newIdent "ff"
--   let ffinit = CInitExpr (CConst $ CStrConst (cString "Hello") undefNode) undefNode
--   let vinit = CInitList [([CMemberDesig ff undefNode], ffinit)] undefNode
--   return $ CDeclExt $ CDecl [t] [(Just v, Just vinit, Nothing)] undefNode
--   where
