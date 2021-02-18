{-# LANGUAGE GADTs, DataKinds, RankNTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-} -- for dealing with TCDecl and existential k

-- We walk through the each decl figuring out if it has things we
-- can't easily handle and thus need to send to the solver.  At the
-- moment this is any Guard.

module Talos.Analysis ( summarise
                      , Summary
                      ) where

import Data.Map (Map)

import Daedalus.GUID
import Daedalus.Type.AST

import Talos.Analysis.Monad
import Talos.Analysis.Domain (TCSynthAnnot)
import Talos.Analysis.PathSet

-- import Debug.Trace



--------------------------------------------------------------------------------
-- Dep analysis

-- Top level function
summarise :: Map TCTyName TCTyDecl ->
             [Name] -> [TCDecl TCSynthAnnot] -> GUID -> (Summaries, GUID)
summarise declTys _roots decls nguid  = (summaries s', nextGUID s')

  where
    s' = calcFixpoint s0 
    s0 = initState declTys decls nguid 
