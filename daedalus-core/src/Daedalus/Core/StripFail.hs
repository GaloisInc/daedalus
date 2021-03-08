
module Daedalus.Core.StripFail where

-- Removes fails as much as possible (i.e., removes branch nodes which dominate fails, etc.)
-- c.f. normM

import Daedalus.Core

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

    Let _ _ f@(Fail {}) -> f
    OrBiased f@(Fail {}) (Fail {}) -> f
    OrBiased (Fail {}) g -> g
    OrBiased g (Fail {}) -> g

    OrUnbiased f@(Fail {}) (Fail {}) -> f
    OrUnbiased (Fail {}) g -> g
    OrUnbiased g (Fail {}) -> g

    Annot _ f@(Fail {}) -> f
    GCase (Case e cs) ->
      case filter (not . isFail . snd) cs of
        []  -> snd (head cs) -- propagate failure (should be a Fail)
        cs' -> GCase (Case e cs')

    nonFail -> nonFail

  where
    isFail (Fail {}) = True
    isFail _         = False


