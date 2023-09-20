{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- This module exports two closely related APIs: a pass to annotate a
-- (un-annotated) module with GUIDs, one for each node; and a pass to
-- turn those annotations into a CFG.

-- Construct a control flow graph and an associated annotated Module

module Daedalus.Core.CFG
  ( addNodeIDs
  , pattern WithNodeID
  , cfg
  , NodeID
  , CFGModule(..)
  , CFG
  , CFGFun(..)
  , CFGSimpleNode(..)
  , CFGNode(..)
  , cfgFunToDot
  ) where

import           Data.Functor   (($>))
import           Data.List      (partition)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           GHC.Generics   (Generic)
import           MonadLib       (WriterT, put, runWriterT)

import           Daedalus.Core
import           Daedalus.GUID  (GUID, HasGUID, getNextGUID)
import           Daedalus.Panic (panic)
import           Daedalus.PP

{-# COMPLETE WithNodeID #-}
pattern WithNodeID :: NodeID -> [Annot] -> Grammar -> Grammar
pattern WithNodeID n anns g <- (getNodeIDPat -> (n, anns, g))

getNodeIDPat :: Grammar -> (NodeID, [Annot], Grammar)
getNodeIDPat (skipGetAnnot -> (anns', g))
  | ([NodeID n], anns) <- partition isNodeID anns' = (n, anns, g)
  | otherwise = panic "Missing NodeID annotation" []
  where
    isNodeID (NodeID {}) = True
    isNodeID _           = False
    
-- ----------------------------------------------------------------------------------------
-- Decorating grammar nodes

addNodeIDs :: HasGUID m => Module -> m Module
addNodeIDs mo = do
  gfs <- traverse (traverse addNodeIDsG) (mGFuns mo)
  pure mo { mGFuns = gfs }

addNodeIDsG :: HasGUID m => Grammar -> m Grammar
addNodeIDsG (Annot a g) = Annot a <$> addNodeIDsG g
addNodeIDsG g = do
  nid <- freshNodeID
  Annot (NodeID nid) <$> gebChildrenG addNodeIDsG pure pure g

-- ----------------------------------------------------------------------------------------
-- Construct CFG

-- Entry
cfg :: HasGUID m => Module -> m CFGModule
cfg m = do
  m_cfgfuns <- traverse cfgGFun (mGFuns m)
  let cfgfuns = Map.fromList [ (cfgfunName f, f) | Just f <- m_cfgfuns ]
  pure (CFGModule cfgfuns)

-- ----------------------------------------------------------------------------------------
-- CFG datatype

type NodeID = GUID

data CFGModule = CFGModule
  { cfgFuns  :: Map FName CFGFun
  } deriving (Generic)

type CFG = Map NodeID CFGNode  

data CFGFun = CFGFun
  { cfgfunEntry :: !NodeID
  , cfgfunExit  :: !NodeID
  , cfgfunName  :: !FName
  , cfgfunCFG   :: !CFG
  -- , cfgfunGrammars :: Map NodeID Grammar
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
  CSimple (Maybe Name) CFGSimpleNode NodeID
  | CFail
  | COr Bool NodeID NodeID
  | CCase (Case NodeID)
  | CLoop (Maybe Name) (LoopClass' Expr NodeID) NodeID

-- ----------------------------------------------------------------------------------------
-- Workers

freshNodeID :: HasGUID m => m NodeID
freshNodeID = getNextGUID

cfgGFun :: HasGUID m => Fun Grammar -> m (Maybe CFGFun)
cfgGFun fu =
  case fDef fu of
    Def b -> do
      exitN <- freshNodeID
      (inN, nodes) <- runWriterT (cfgG Nothing exitN b)
      let cfgfun = CFGFun
            { cfgfunEntry = inN
            , cfgfunExit  = exitN
            , cfgfunName  = fName fu
            , cfgfunCFG   = nodes
            }
      pure (Just cfgfun)
    External -> pure Nothing

cfgG :: HasGUID m => Maybe Name -> NodeID -> Grammar -> WriterT CFG m NodeID
cfgG m_x exitN (WithNodeID inN _anns g) =
  case g of
    Pure e -> simple (CPure e)
    GetStream    -> simple CGetStream
    SetStream e  -> simple (CSetStream e)
    Match s m    -> simple (CMatch s m)
    Fail {}      -> emitNode CFail
      
    Do_ lhs rhs   -> goDo Nothing lhs rhs
    Do  n lhs rhs -> goDo (Just n) lhs rhs
    Let n e rhs   -> do
      rhsN <- cfgG m_x exitN rhs
      emitNode (CSimple (Just n) (CPure e) rhsN)

    OrBiased lhs rhs   -> goOr True lhs rhs
    OrUnbiased lhs rhs -> goOr False lhs rhs
    Call fn es -> simple (CCall fn es)
    Annot _a _g' -> panic "Unexpected Annot" []    
    GCase cs   -> do
      cs' <- traverse (cfgG m_x exitN) cs
      emitNode (CCase cs')
    Loop lc -> do
      lc' <- traverse (cfgG m_x inN) lc -- loop back here
      emitNode (CLoop m_x lc' exitN)
  where
    goDo m_y lhs rhs = do
      rhsN <- cfgG m_x exitN rhs
      cfgG m_y rhsN lhs

    goOr biased lhs rhs = do
      lN <- cfgG m_x exitN lhs
      rN <- cfgG m_x exitN rhs
      emitNode (COr biased lN rN) $> lN -- Ignore id of Do, use the id of the first non-do in the AST.
      
    simple n = emitNode (CSimple m_x n exitN)
    emitNode node = tell (Map.singleton inN node) $> inN

    tell = put -- to be consistent with mtl

-- ----------------------------------------------------------------------------------------
-- Pretty printing

cfgFunToDot :: CFGFun -> Doc
cfgFunToDot f =
  ("digraph " <> pp (cfgfunName f) <> " " <> lbrace)
  $+$ nest 2 (vcat (prelude ++ nodes))
  $+$ rbrace
  where
    prelude = [ "init -> " <> pp (cfgfunEntry f) <> semi
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
      
        
 
 
