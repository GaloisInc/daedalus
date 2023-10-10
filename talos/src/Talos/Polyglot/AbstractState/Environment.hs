{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.AbstractState.Environment where

import           Data.Map              (Map)
import qualified Data.Map              as Map

import Daedalus.Core
import Daedalus.Core.CFG
import Daedalus.PP

import           Talos.Polyglot.AbstractState.ThreadSet (ThreadSet)
import qualified Talos.Polyglot.AbstractState.ThreadSet as ThreadSet

import Talos.Polyglot.Util
import Talos.Polyglot.PolyglotReader

-- | Maps variables to the set of stream read nodes that flow to this variable,
-- one set per path.
type Env = Map Name Summary
type Summary = ThreadSet NodeID

ppEnv :: Env -> Doc
ppEnv env = vcat rows
  where
    rows = map doRow (Map.toList env)
    doRow (name, summary) = pp name <+> text "->" <+> pp summary

empty :: Env
empty = Map.empty

join :: Env -> Env -> Env
join = mapUnion ThreadSet.join

-- | Merge the given summary with the environment summary for the given variable
-- name.
extend :: Env -> Name -> Summary -> Env
extend env var summary = Map.alter f var env
  where
    f Nothing         = Just summary
    f (Just summary') = Just (ThreadSet.join summary summary')

-- | Find the stream reads that are used in this expression.  TODO: env should
-- be in a reader; we're not adding to it, just using it to look up open var
-- names.
summarizeExpr :: Env -> Expr -> PolyglotReader Summary

-- This is the important case.  The rest just implement the traversal :(
summarizeExpr env (Var name) = return $ Map.findWithDefault ThreadSet.empty name env

summarizeExpr env (PureLet _ left right) = do
  leftSummary <- summarizeExpr env left
  rightSummary <- summarizeExpr env right
  return $ ThreadSet.join leftSummary rightSummary

summarizeExpr env (Struct _ fields) = do
  summaries <- sequence $ map ((summarizeExpr env) . snd) fields
  return $ foldl ThreadSet.join ThreadSet.empty summaries

summarizeExpr env (ECase Case{..}) = do
  let varSummary = Map.findWithDefault ThreadSet.empty caseVar env
  summaries <- sequence $ map ((summarizeExpr env) . snd) casePats
  return $ foldl ThreadSet.join ThreadSet.empty (varSummary:summaries)

summarizeExpr env (ELoop loop) = summarizeExpr env (morphismBody loop)

summarizeExpr _ (Ap0 _) = return ThreadSet.empty

summarizeExpr env (Ap1 _ e1) = summarizeExpr env e1

summarizeExpr env (Ap2 _ e1 e2) = do
  summaries <- sequence $ map (summarizeExpr env) [e1, e2]
  return $ foldl ThreadSet.join ThreadSet.empty summaries

summarizeExpr env (Ap3 _ e1 e2 e3) = do
  summaries <- sequence $ map (summarizeExpr env) [e1, e2, e3]
  return $ foldl ThreadSet.join ThreadSet.empty summaries

summarizeExpr env (ApN (CallF name) exprs) = do
  ffun <- getFFun name
  case fDef ffun of
    External -> return ThreadSet.empty
    Def body -> do
      fSum <- summarizeExpr env body
      summaries <- sequence $ map (summarizeExpr env) exprs
      return $ foldl ThreadSet.join ThreadSet.empty (fSum:summaries)

summarizeExpr env (ApN op exprs) = do
  summaries <- sequence $ map (summarizeExpr env) exprs
  return $ foldl ThreadSet.join ThreadSet.empty summaries
