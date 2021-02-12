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


normG :: Grammar -> Grammar
normG gram =
  case mapChildrenG normG gram of
    Do_ (Do_ x y) z -> Do_ x (normG (Do_ y z))
    Do_ (Do a x y) z
      | not (a `Set.member` freeVars z) -> Do a x (normG (Do_ y z))

    Do_ (Let a x y) z
      | not (a `Set.member` freeVars z) -> Let a x (normG (Do_ y z))

    Do_ x (Pure (Ap0 Unit))
      | typeOf x == TUnit -> x

    Do a (Do_  x y) z -> Do_ x (normG (Do a y z))
    Do a (Do b x y) z
      | not (b `Set.member` freeVars z) -> Do b x (normG (Do a y z))

    Do a (Let b x y) z
      | not (b `Set.member` freeVars z) -> Let b x (normG (Do a y z))

    Do a x (Pure (Var a')) | a == a' -> x

    Do a (Pure e) x -> Let a e x

    norm            -> norm

