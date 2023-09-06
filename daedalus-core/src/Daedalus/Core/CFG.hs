{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Construct a control flow graph and an associated annotated Module

module Daedalus.Core.CFG
  ( cfg
  , NodeId
  , CFGModule(..)
  , CFG
  , CFGFun(..)
  , CFGSimpleNode(..)
  , CFGNode(..)
  , cfgFunToDot
  ) where

import           Data.Map      (Map)
import qualified Data.Map      as Map

import           Daedalus.Core
import           Daedalus.GUID (GUID, HasGUID, getNextGUID)
import           GHC.Generics  (Generic)
import           MonadLib      (WriterT, put, runWriterT)
import Daedalus.PP 

-- Entry
cfg :: HasGUID m => Module -> m (Module, CFGModule)
cfg m = do
  (gfuns', m_cfgfuns) <- unzip <$> traverse cfgGFun (mGFuns m)
  let cfgfuns = Map.fromList [ (cfgfunName f, f) | Just f <- m_cfgfuns ]
  pure (m { mGFuns = gfuns'}, CFGModule cfgfuns)

-- ----------------------------------------------------------------------------------------
-- CFG datatype

type NodeId = GUID

data CFGModule = CFGModule
  { cfgFuns  :: Map FName CFGFun
  } deriving (Generic)

type CFG = Map NodeId CFGNode  

data CFGFun = CFGFun
  { cfgfunEntry :: !NodeId
  , cfgfunExit  :: !NodeId
  , cfgfunName  :: !FName
  , cfgfunCFG   :: !CFG
  -- , cfgfunGrammars :: Map NodeId Grammar
  } deriving (Generic)

-- We ignore Annot
data CFGSimpleNode =
  CPure Expr
  | CGetStream
  | CSetStream Expr
  | CMatch Sem Match
  | CCall FName [Expr]

-- Basically Grammar without Do/Let
data CFGNode =
  CSimple (Maybe Name) CFGSimpleNode NodeId
  | CFail
  | COr Bool NodeId NodeId
  | CCase (Case NodeId)
  | CLoop (Maybe Name) (LoopClass' Expr NodeId) NodeId

-- ----------------------------------------------------------------------------------------
-- Workers

freshNodeId :: HasGUID m => m NodeId
freshNodeId = getNextGUID

cfgGFun :: HasGUID m => Fun Grammar -> m (Fun Grammar, Maybe CFGFun)
cfgGFun fu =
  case fDef fu of
    Def b    -> do
      inN   <- freshNodeId
      exitN <- freshNodeId
      (g', nodes) <- runWriterT (cfgG Nothing inN exitN b)
      let fu' = fu { fDef = Def g'
                   , fAnnot = NodeID inN : fAnnot fu
                   }
          cfgfun = CFGFun
            { cfgfunEntry = inN
            , cfgfunExit = exitN
            , cfgfunName  = fName fu
            , cfgfunCFG   = nodes
            }
      pure (fu', Just cfgfun)
    External -> pure (fu, Nothing)

cfgG :: HasGUID m => Maybe Name -> NodeId -> NodeId -> Grammar -> WriterT CFG m Grammar
cfgG m_x inN exitN g =
  case g of
    Pure e -> simple (CPure e)
    GetStream -> simple CGetStream
    SetStream e ->  simple (CSetStream e)
    Match s m    -> simple (CMatch s m)
    Fail {} -> do
      emitNode CFail
      pure (annot g)
      
    Do_ lhs rhs   -> goDo Do_ Nothing lhs rhs
    Do  n lhs rhs -> goDo (Do n) (Just n) lhs rhs
    Let n e rhs   -> cfgG m_x inN exitN (Do n (Pure e) rhs) -- FIXME
    OrBiased lhs rhs   -> goOr OrBiased True lhs rhs
    OrUnbiased lhs rhs -> goOr OrUnbiased False lhs rhs
    Call fn es -> simple (CCall fn es)
    Annot a g' -> Annot a <$> cfgG m_x inN exitN g'
    GCase cs   -> do
      cs' <- traverse (goOne exitN) cs
      emitNode (CCase (fst <$> cs'))
      pure (annot (GCase (snd <$> cs')))
    Loop lc -> do
      lc' <- traverse (goOne inN) lc -- loop back here
      emitNode (CLoop m_x (fst <$> lc') exitN)
      pure (annot (Loop (snd <$> lc')))
  where
    goOne exitN' g' = do
      lN <- freshNodeId
      (,) lN <$> cfgG m_x lN exitN' g'
    
    goDo mk m_y lhs rhs = do
      rhsN <- freshNodeId
      lhs' <- cfgG m_y inN rhsN lhs
      rhs' <- cfgG m_x rhsN exitN rhs
      pure (mk lhs' rhs') -- We do not annotate Do/Do_ nodes

    goOr mk biased lhs rhs = do
      lN <- freshNodeId
      rN <- freshNodeId
      lhs' <- cfgG m_x lN exitN lhs
      rhs' <- cfgG m_x rN exitN rhs
      emitNode (COr biased lN rN)
      pure (annot (mk lhs' rhs'))
      
    simple n = do
      emitNode (CSimple m_x n exitN)
      pure (annot g)
    
    annot = Annot (NodeID inN)
    emitNode node = tell (Map.singleton inN node)

    tell = put -- to be consistent with mtl

-- ----------------------------------------------------------------------------------------
-- Pretty printing

cfgFunToDot :: CFGFun -> Doc
cfgFunToDot f =
  ("digraph " <> pp (cfgfunName f) <> " " <> lbrace)
  $+$ nest 2 (vcat (prelude ++ nodes))
  $+$ rbrace
  where
    prelude = [ "rankdir=LR;"
              , "init -> " <> pp (cfgfunEntry f) <> semi
              , "init [style = invis];"
              , pp (cfgfunExit f) <> " [style = invis];"
              ]
    nodes   = concat [ mkNode k n |  (k, n) <- Map.toList (cfgfunCFG f) ]
    mkNode nid n =
      let (lbl, edges) =
            case n of
              CSimple m_x sn nxtN -> (mkSimple m_x sn, [(nid, nxtN, Nothing)])
              CFail -> ("Fail", [])
              COr b l r -> ("Or" <> if b then " (biased) " else "", [(nid, l, Nothing), (nid, r, Nothing)])
              CCase (Case n' pats) -> ("Case " <> pp n', [ (nid, l, Just (pp pat)) | (pat, l) <- pats ])
              CLoop m_x lc nxtN -> ( maybe empty (\x -> pp x <> " = ") m_x <> "Loop"
                                   , [ (nid, loopClassBody lc, Just "loop"), (nid, nxtN, Just "exit") ])
          edges' = [ pp l1 <> " -> " <> pp l2 <> maybe empty (brackets . (<>) "label = ". doubleQuotes) m_lbl <> semi
                   | (l1, l2, m_lbl) <- edges ]
      in ( pp nid <> " " <> brackets ("label = " <> doubleQuotes lbl) <> semi ) : edges'

    mkSimple m_x sn =
      let pfx = maybe empty (\x -> pp x <> " = ") m_x
      in pfx <> case sn of
        CPure e -> pp e
        CGetStream -> "GetStream"
        CSetStream e -> "SetStream " <> pp e
        CMatch s m  -> ppMatch s m
        CCall fn es  -> pp fn <> hsep (map pp es)
      
        
  
