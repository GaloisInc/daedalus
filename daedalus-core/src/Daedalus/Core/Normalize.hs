module Daedalus.Core.Normalize where

import qualified Data.Set as Set

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.Type

normM :: Module -> Module
normM m = m { mGFuns = map normGFun (mGFuns m) }

normGFun :: Fun Grammar -> Fun Grammar
normGFun fu =
  case fDef fu of
    Def g -> fu { fDef = Def (normG g) }
    _     -> fu

-- XXX: rename things rather than disabling the optimization
-- XXX: The annotation manipulation makes sense for source ranges,
-- but not so sure for other things..
normG :: Grammar -> Grammar
normG gram =
  case mapChildrenG normG gram of
    Do_ (Annotated as (Do_ x y)) z ->
      gBinAnnotate Do_ (gAnnotate as x) (normG (Do_ (gAnnotate as y )z))

    Do_ (Annotated as (Do a x y)) z
      | not (a `Set.member` freeVars z) ->
      gBinAnnotate (Do a) (gAnnotate as x) (normG (Do_ (gAnnotate as y) z))

    Do_ (Annotated as (Let a x y)) z
      | not (a `Set.member` freeVars z) ->
      Let a x (normG (Do_ (gAnnotate as y) z))

    Do_ (Annotated _as (Pure (Ap0 Unit))) x -> normG x
    Do_ x (Annotated _as (Pure (Ap0 Unit)))
      | typeOf x == TUnit -> x

    Do a (Annotated as (Do_  x y)) z ->
      gBinAnnotate Do_ (gAnnotate as x) (normG (Do a (gAnnotate as y) z))

    Do a (Annotated as (Do b x y)) z
      | not (b `Set.member` freeVars z) ->
        gBinAnnotate (Do b) (gAnnotate as x) (normG (Do a (gAnnotate as y) z))

    Do a (Annotated as (Let b x y)) z
      | not (b `Set.member` freeVars z) ->
        Let b x (normG (Do a (gAnnotate as y) z))

    Do a x (Annotated _as (Pure (Var a'))) | a == a' -> x

    Do a (Annotated as (Pure e)) x -> gAnnotate as (Let a e x)

    norm            -> norm

