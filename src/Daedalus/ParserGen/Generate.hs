 {-# LANGUAGE OverloadedStrings, TupleSections, GADTs, DataKinds #-}

module Daedalus.ParserGen.Generate where

import Language.C
import Control.Monad.State as SM

import Daedalus.ParserGen.Aut as Aut
import Daedalus.ParserGen.Action as Action

import qualified Daedalus.Type.AST as DAST

import Daedalus.Interp

import Data.Text ()
import Text.PrettyPrint

-- State for the generator
data CAutGenData = CAutGenData {
  names :: [Name],
  actionId :: Integer,
  transitionId :: Integer,
  declarations :: [CExtDecl]
}

emptyAutGenData :: CAutGenData
emptyAutGenData = CAutGenData {
  names = [],
  actionId = 1,
  transitionId = 1,
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

nextTransitionId :: CAutGenM Integer
nextTransitionId = do
  st <- get  
  let v = transitionId st
  put $ st { transitionId = v + 1 }
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

-- Make an expression out of a constant integer
makeNameConstExpr :: Maybe DAST.Name -> CExpr
makeNameConstExpr ms = 
  case ms of
    Just s  -> CConst $ CStrConst (cString $ show $ name2Text s) undefNode
    Nothing -> makeIntConstExpr 0

makeAddrOfExpr :: Ident -> CExpr
makeAddrOfExpr ident = CUnary CAdrOp (CVar ident undefNode) undefNode

makeStructInitializer :: [(String, CExpr)] -> CAutGenM CInit
makeStructInitializer nameExprList = do
    inits <- mapM (uncurry structFieldInitializer) nameExprList
    return $ CInitList inits undefNode
    where
      structFieldInitializer name expr = do
        initIdent <- makeIdent name
        return ([CMemberDesig initIdent undefNode], CInitExpr expr undefNode)

makeArrayInitializer :: [CInit] -> CAutGenM CInit
makeArrayInitializer ilist = do
  let inits = map (\e -> ([], e)) ilist
  return $ CInitList inits undefNode


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
  decls <- declarations <$> get
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
  let tableExpr = makeAddrOfExpr tableIdent
  fieldsInit <- 
    makeStructInitializer 
      [("initial", initStateExpr)
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
  let stateChoiceList = allTransitions aut
  initializers <- mapM (generateInitializer . snd) stateChoiceList
  initializer <- makeArrayInitializer initializers
  (ident, declr) <- makeArrayVarDeclr "trs" (length stateChoiceList)
  declType <- CTypeSpec <$> makeTypeDefType "Choice"
  let decl = CDeclExt $ CDecl [declType] [(Just declr, Just initializer, Nothing)] undefNode
  addDeclaration decl
  return ident
  where
    generateInitializer choice = do
      (tag, cnt, ident) <- generateActionStatePairs choice
      tagExpr <- makeEnumConstantExpr tag
      let lenExpr = makeIntConstExpr cnt
      let addrExpr = makeAddrOfExpr ident
      makeStructInitializer [("tag", tagExpr), ("len", lenExpr), ("transitions", addrExpr)]

generateActionStatePairs :: Choice -> CAutGenM (String, Int, Ident)
generateActionStatePairs choice = do
  varName <- transitionName
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
    transitionName = nextTransitionId >>= (\i -> return $ "tr" ++ show i)
    choiceExpr (UniChoice _) = "UNICHOICE"
    choiceExpr (SeqChoice _ _) = "SEQCHOICE"
    choiceExpr (ParChoice _) = "PARCHOICE"

generateActionStatePair :: Action -> Action.State -> CAutGenM CInit
generateActionStatePair action st = do
  ident <- generateAction action
  let actionExpr = makeAddrOfExpr ident
  let stateExpr = makeIntConstExpr st
  makeStructInitializer [("pAction", actionExpr), ("state", stateExpr)]

generateAction :: Action -> CAutGenM Ident
generateAction act = do
  (actVarIdent, actVarDeclr)  <- makeActionVar
  actType <- CTypeSpec <$> makeTypeDefType "Action"
  actInit <- actionInitializer
  let decl = CDeclExt $ CDecl [actType] [(Just actVarDeclr, Just actInit, Nothing)] undefNode 
  addDeclaration decl
  return actVarIdent
  where
    makeActionVar = do
      n <- nextActionId
      let name = "a" ++ (show n)
      makeVarDeclr name
    actionInitializer = do
      designatorInitList <- generateActionData act
      makeStructInitializer designatorInitList

generateActionData :: Action -> CAutGenM [(String, CExpr)]
generateActionData (IAct ip) = generateInputActionData ip
generateActionData (CAct cp) = generateControlAction cp
generateActionData (SAct sp) = generateSemanticActionData sp
generateActionData (BAct bp) = return []
generateActionData EpsA      = generateEpsAction

generateInputActionData :: InputAction -> CAutGenM [(String, CExpr)]
generateInputActionData IEnd = do
  tagExpr <- makeEnumConstantExpr "ACT_END"
  return [("tag", tagExpr)]
generateInputActionData (IMatchBytes withsem e) = do
  matchExpr <- generateVExpr e
  tagExpr <- makeEnumConstantExpr "ACT_MatchBytes"
  return [("tag", tagExpr), ("expr", matchExpr)]
generateInputActionData (ClssAct withsem e) = do
  -- TODO: Generalize this
  let v = case DAST.texprValue e of 
            DAST.TCSetSingle e' -> evalNoFunCall e' [] []
            _ -> error $ "Unimplemented expression in ClssAct evaluation : " ++ show e
  case v of
    VUInt b iv -> do
      tagExpr <- makeEnumConstantExpr "ACT_ReadChar"
      let valExpr = makeIntConstExpr $ fromIntegral iv
      return [("tag", tagExpr), ("chr", valExpr)]
    _ -> return $ error $ "Unimplemented ClssAct matching : " ++ show v
generateInputActionData x = return $ error $ "Unimplemented action: " ++ show x

generateSemanticActionData :: SemanticAction -> CAutGenM [(String, CExpr)]
generateSemanticActionData EnvFresh = do
  tagExpr <- makeEnumConstantExpr "ACT_EnvFresh"
  return [("tag", tagExpr)]
generateSemanticActionData (EnvStore nm) = do
  tagExpr <- makeEnumConstantExpr "ACT_EnvStore"
  return [("tag", tagExpr), ("name", makeNameConstExpr nm)]
generateSemanticActionData (ReturnBind e) = do
  tagExpr <- makeEnumConstantExpr "ACT_ReturnBind"
  expr <- generateVExpr e
  return [("tag", tagExpr), ("expr", expr)]
generateSemanticActionData x = return $ error $ "Unimplemented action: " ++ show x

generateControlAction :: ControlAction -> CAutGenM [(String, CExpr)]
generateControlAction (Push name le q) = do
  tagExpr <- makeEnumConstantExpr "ACT_Push"
  -- TODO: Fix this
  let valExpr = makeIntConstExpr q
  return [("tag", tagExpr), ("name", valExpr)]
generateControlAction (Pop q) = do
  tagExpr <- makeEnumConstantExpr "ACT_Pop"
  let valExpr = makeIntConstExpr q
  return [("tag", tagExpr), ("name", valExpr)]
generateControlAction (ActivateFrame ln) = do
  tagExpr <- makeEnumConstantExpr "ACT_ActivateFrame"
  -- TODO: Fix this
  let valExpr = makeIntConstExpr 0
  return [("tag", tagExpr), ("namelist", valExpr)]
generateControlAction DeactivateReady = do
  tagExpr <- makeEnumConstantExpr "ACT_DeactivateReady"
  return [("tag", tagExpr)]
generateControlAction x = return $ error $ "Unimplemented action: " ++ show x

generateVExpr :: NVExpr -> CAutGenM CExpr
generateVExpr e = do
  ident <- makeIdent "UNKNOWN"
  return $ CVar ident undefNode

generateEpsAction :: CAutGenM [(String, CExpr)]
generateEpsAction = do
  tagExpr <- makeEnumConstantExpr "ACT_EpsA"
  return [("tag", tagExpr)]


--   let varDecl = CDeclr (Just autVar) [] Nothing [] undefNode 
  
--   ff <- newIdent "ff"
--   let ffinit = CInitExpr (CConst $ CStrConst (cString "Hello") undefNode) undefNode
--   let vinit = CInitList [([CMemberDesig ff undefNode], ffinit)] undefNode
--   return $ CDeclExt $ CDecl [t] [(Just v, Just vinit, Nothing)] undefNode 
--   where
