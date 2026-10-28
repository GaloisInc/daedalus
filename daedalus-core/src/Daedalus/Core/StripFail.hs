
module Daedalus.Core.StripFail where

import qualified Data.Set as Set


-- Removes fails as much as possible (i.e., removes branch nodes which dominate fails, etc.)
-- c.f. normM

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Panic (panic)

stripFailM :: Module -> Module
stripFailM m = m { mGFuns = map stripFailGFun (mGFuns m) }

stripFailGFun :: Fun Grammar -> Fun Grammar
stripFailGFun fu =
  case fDef fu of
    Def g -> fu { fDef = Def (stripFailG g) }
    _     -> fu

stripFailG :: Grammar -> Grammar
stripFailG gram =
  case mapChildrenG stripFailG gram of
    Do_ f@(Fail {}) _ -> f
    Do_ _ f@(Fail {}) -> f
    
    Do _ f@(Fail {}) _ -> f
    Do _ _ f@(Fail {}) -> f

    Let x _ f@(Fail {})
      | not (x `Set.member` freeVars f) -> f
      
    OrBiased f@(Fail {}) (Fail {}) -> f
    OrBiased (Fail {}) g -> g
    OrBiased g (Fail {}) -> g

    OrUnbiased f@(Fail {}) (Fail {}) -> f
    OrUnbiased (Fail {}) g -> g
    OrUnbiased g (Fail {}) -> g

    Annot _ f@(Fail {}) -> f

    GCase (Case e cs) ->
      case cs of
        [] -> panic "stripFailG" ["empty case alternatives"]
        _ | any isNonFailPAny cs -> GCase (Case e cs)
        _ ->
          case (filter (isFail . snd) cs, filter (not . isFail . snd) cs) of
            ([], _)            -> GCase (Case e cs)
            ((_, f) : _, [])   -> f
            ((_, f) : _, cs')  -> GCase (Case e (cs' ++ [(PAny, f)]))
        
    nonFail -> nonFail

  where
    isFail (Fail {}) = True
    isFail _         = False

    isNonFailPAny (PAny, g) | not (isFail g) = True
    isNonFailPAny _ = False

