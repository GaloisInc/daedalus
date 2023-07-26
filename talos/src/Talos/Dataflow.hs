{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Talos.Dataflow where

import Daedalus.Core
import Daedalus.GUID
import Daedalus.PP

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Lens as Map
import Data.List (intercalate)
import Talos.StatefulProcessor
import Text.Printf (printf)
import Control.Monad (foldM)

-- | 'Constrainedness' captures a rough approximation of the range of values
-- a variable can take on.
data Constrainedness = 
      Unconstrained       -- ^ Any value.
    | LightlyConstrained  -- ^ Most values.
    | HeavilyConstrained  -- ^ A small, finite number of values.
    deriving (Eq, Show)

joinCon :: Constrainedness -> Constrainedness -> Constrainedness
joinCon Unconstrained _ = Unconstrained
joinCon _ Unconstrained = Unconstrained
joinCon LightlyConstrained _ = LightlyConstrained
joinCon _ LightlyConstrained = LightlyConstrained
joinCon _ _ = HeavilyConstrained

meetCon :: Constrainedness -> Constrainedness -> Constrainedness
meetCon Unconstrained c = c
meetCon c Unconstrained = c
meetCon LightlyConstrained c = c
meetCon c LightlyConstrained = c
meetCon _ _ = HeavilyConstrained

-- | The abstract state tracked for each variable.
data VariableAbstractState = VariableAbstractState
    { constrainedness :: Constrainedness

    -- | Whether data in this variable flows from the stream.
    , fromStream :: Bool
    }
    deriving (Eq, Show)

bottomVariableAbstractState :: VariableAbstractState
bottomVariableAbstractState = VariableAbstractState
  { constrainedness = HeavilyConstrained
  , fromStream = False
  }

joinVST :: VariableAbstractState -> VariableAbstractState -> VariableAbstractState
joinVST v1 v2 = VariableAbstractState
  { constrainedness = joinCon (constrainedness v1) (constrainedness v2)
  , fromStream = (fromStream v1) || (fromStream v2)
  }

-- | Meet of constrainedness, lifted to variable abstract states.  fromStream is
-- still joined.
meetVST :: VariableAbstractState -> VariableAbstractState -> VariableAbstractState
meetVST v1 v2 = VariableAbstractState
  { constrainedness = meetCon (constrainedness v1) (constrainedness v2)
  , fromStream = (fromStream v1) || (fromStream v2)
  }

type FunctionID = GUID
type VariableID = GUID

type VMap = Map FunctionID (Map VariableID VariableAbstractState)

computeDataflow :: Module -> VMap
computeDataflow _ = Map.Empty

-------------------------------------------------------------------------------

data DataflowState = DataflowState
    { fname :: Maybe FName
    , vmap  :: VMap
    }
    deriving (Eq, Show)

emptyDataflowState :: DataflowState
emptyDataflowState = DataflowState
  { fname = Nothing
  , vmap  = Map.empty
  }

type ErrorMsg = String
-- type DSP a = State DataflowState (Either ErrorMsg a)
type DSP a = StatefulProcessor DataflowState ErrorMsg a

runDSP :: DSP a -> DataflowState -> (Either ErrorMsg a, DataflowState)
runDSP dsp st = runState dsp st

getFun :: DSP FunctionID
getFun = do
    st <- get
    case fname st of
        Just n -> pure $ fnameId n
        Nothing -> throwError "Error: no function in context"

update :: (DataflowState -> DataflowState) -> DSP ()
update f = do
    st <- get
    put $ f st

-- | Add or update the abstract state for a given variable in the
-- current function context.
updateVar :: VariableID -> VariableAbstractState -> DSP ()
updateVar v vst = do
    st <- get
    f <- getFun
    let fm    = Map.findWithDefault Map.empty f $ vmap st
        fm'   = Map.insert v vst fm
        vmap' = Map.insert f fm' $ vmap st
    update $ \st -> st { vmap = vmap' }

-- | Look up the abstract state of a variable using the current
-- function context.  Fails if the function/variable is not found.
lookupVar :: VariableID -> DSP VariableAbstractState
lookupVar v = do
    st <- get
    f <- getFun
    let fm = Map.findWithDefault Map.empty f $ vmap st
    case Map.lookup v fm of
        Just x -> pure x
        Nothing -> throwError $ printf "Error: undeclared variable: %s" (show v)

-- lookupFun :: FunctionID -> Map VariableID VariableAbstractState
-- lookupFun f = do
--     st <- get
--     pure $ Map.findWithDefault Map.empty f $ vmap st

applyAnalysis :: (a -> DSP VariableAbstractState) -> Fun a -> DSP VariableAbstractState
applyAnalysis analysis Fun{fDef=Def x} = analysis x
applyAnalysis _ _ = throwError "Error: can't analyze external function"

-------------------------------------------------------------------------------

-- | Compute dataflow state for each variable in Expr.  Assumes variables have
-- unique GUIDs.
dataflowFFun :: Fun Expr -> DSP ()
dataflowFFun fexpr = do
    _ <- applyAnalysis dataflowExpr fexpr
    pure ()

dataflowExpr :: Expr -> DSP VariableAbstractState

dataflowExpr (Var n) = lookupVar $ nameId n

dataflowExpr (PureLet name e1 e2) = do
    vst <- dataflowExpr e1
    updateVar (nameId name) vst
    dataflowExpr e2

-- TODO(cns): add field-level precision.
dataflowExpr (Struct _ fields) = do
    vsts <- sequence $ map (dataflowExpr . snd) fields
    pure $ foldl joinVST bottomVariableAbstractState vsts

dataflowExpr (ECase Case {casePats}) = do
    vsts <- sequence $ map (dataflowExpr . snd) casePats
    pure $ foldl joinVST bottomVariableAbstractState vsts

dataflowExpr (ELoop (FoldMorphism name e1 col e2)) = do
    -- Process the default expression assigned to the collector variable.
    eVst <- dataflowExpr e1
    updateVar (nameId name) eVst

    -- Process the loop collection.
    lVst <- dataflowExpr (lcCol col)
    updateVar (nameId (lcElName col)) lVst

    -- Process the loop body.
    bVst <- dataflowExpr e2

    -- Merge the VST of the loop collection with the collector variable.  Loops
    -- implicitly bind the value returned by the loop body to this variable, or
    -- the default value if the collection is empty.
    let mergedVst = joinVST eVst bVst
    updateVar (nameId name) mergedVst

    pure mergedVst

dataflowExpr (ELoop (MapMorphism col e)) = do
    -- Process the loop collection.
    lVst <- dataflowExpr (lcCol col)
    updateVar (nameId (lcElName col)) lVst

    -- Process the loop body.
    dataflowExpr e

-- Ap0 --

-- | Unitary applications are literal values and therefore heavily constrained.
dataflowExpr (Ap0 _) = pure bottomVariableAbstractState

-- Ap1 -- 

-- | Coercing an expression to a type affects how constrained it is, depending
-- on the type.
dataflowExpr (Ap1 (CoerceTo t) e) = do
    vst <- dataflowExpr e
    pure vst { constrainedness = meetCon (constrainedness vst) (constrainednessOfType t)}

dataflowExpr (Ap1 IsEmptyStream e) = do
    -- Process e in order to add any local variables to the dataflow map.
    _ <- dataflowExpr e
    pure bottomVariableAbstractState

-- | Stream operations are (conservatively) unconstrained on the input stream
-- and constrained on streams derived from stream literals.
dataflowExpr (Ap1 op e) |
     op == Head
  || op == StreamOffset
  || op == BytesOfStream
  = dataflowExpr e

dataflowExpr (Ap1 Not e) = do
    vst <- dataflowExpr e
    case constrainedness vst of
        Unconstrained      -> pure vst { constrainedness = HeavilyConstrained }
        LightlyConstrained -> pure vst { constrainedness = HeavilyConstrained }
        HeavilyConstrained -> pure vst { constrainedness = LightlyConstrained }

dataflowExpr (Ap1 (OneOf _) e) = do
    vst <- dataflowExpr e
    pure vst { constrainedness = HeavilyConstrained }

dataflowExpr (Ap1 op e) |
     op == IsNaN
  || op == IsInfinite
  || op == IsDenormalized
  || op == IsNegativeZero
  = do
    vst <- dataflowExpr e
    pure vst { constrainedness = HeavilyConstrained }

-- | Remaining cases are derived from their child expression.
dataflowExpr (Ap1 _ e) = dataflowExpr e

-- Ap2 -- 

dataflowExpr (Ap2 op _ e2) |
     op == Drop
  || op == Take
  || op == MapLookup
  = dataflowExpr e2

dataflowExpr (Ap2 op e1 e2) |
     op == IsPrefix
  || op == Eq
  -- Collections made up of both constrained and unconstrained elements
  -- are considered constrained.
  || op == Cat
  || op == LCat
  || op == Emit
  || op == EmitArray
  || op == EmitBuilder
  -- Presence in a map induces a constraint unless both the key and the map
  -- come from the file.
  || op == MapMember
   = do
    vst1 <- dataflowExpr e1
    vst2 <- dataflowExpr e2
    pure $ meetVST vst1 vst2

-- | Remaining cases are derived from their child expressions.
dataflowExpr (Ap2 _ e1 e2) = do
    vst1 <- dataflowExpr e1
    vst2 <- dataflowExpr e2
    pure $ joinVST vst1 vst2

-- Ap3

-- Collections made up of both constrained and unconstrained elements
-- are considered constrained.
dataflowExpr (Ap3 MapInsert e1 e2 e3) = do
    _    <- dataflowExpr e1
    vst2 <- dataflowExpr e2
    vst3 <- dataflowExpr e3
    pure $ meetVST vst2 vst3

dataflowExpr (Ap3 _ _ _ e3) = dataflowExpr e3

-- ApN

dataflowExpr (ApN (ArrayL _) es) = do
    vsts <- sequence $ map dataflowExpr es
    pure $ foldl meetVST bottomVariableAbstractState vsts

-- dataflowExpr (ApN (CallF fn) es) = do
    -- TODO(cns): Look up fn, meet params and actual args.

-- Default
dataflowExpr e = throwError $ show $ pp e

constrainednessOfType :: Type -> Constrainedness
constrainednessOfType TBool = HeavilyConstrained
constrainednessOfType TUnit = HeavilyConstrained
constrainednessOfType (TArray t) = constrainednessOfType t
constrainednessOfType (TMaybe t) = constrainednessOfType t
constrainednessOfType _ = LightlyConstrained

-------------------------------------------------------------------------------

dataflowGFun :: Fun Grammar -> DSP () 
dataflowGFun fgram = do
    _ <- applyAnalysis dataflowGrammar fgram
    pure ()

dataflowGrammar :: Grammar -> DSP VariableAbstractState
dataflowGrammar _ =
    pure VariableAbstractState 
        { constrainedness = Unconstrained
        , fromStream = False
        }

-------------------------------------------------------------------------------

summarizeDataflow :: Module -> IO ()
summarizeDataflow md = do
    putStrLn "GFuns:"
    putStrLn $ intercalate ", " $ map (show . pp . fnameText . fName) $ mGFuns md
    putStrLn "FFuns:"
    putStrLn $ intercalate ", " $ map (show . pp . fnameText . fName) $ mFFuns md
    putStrLn "BFuns:"
    putStrLn $ intercalate ", " $ map (show . pp . fnameText . fName) $ mBFuns md
