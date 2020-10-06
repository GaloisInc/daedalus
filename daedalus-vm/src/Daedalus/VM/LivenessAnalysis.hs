module Daedalus.VM.LivenessAnalysis where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(mapAccumL)

import Daedalus.VM
import Daedalus.VM.FreeVars


-- Insert `Free` instructions after the last use of a varilbe
class InsertFree a where
  insertFree :: a -> a

instance InsertFree Block where
  insertFree b = b { blockInstrs = newIs }
    where
    (live,iss) = mapAccumL updI (freeVarSet (blockTerm b))
                                (reverse (blockInstrs b))
    baSet      = Set.fromList [ ArgVar v | v <- blockArgs b ]
    freeIs     = [ Free vs | let vs = Set.difference baSet live
                           , not (Set.null vs) ]
    newIs      = freeIs ++ concat (reverse iss)

updI :: Set VMVar -> Instr -> (Set VMVar, [Instr])
updI live i = (newLive, i : freeIs)
  where
  used     = freeVarSet i
  newLive  = Set.union used live `Set.difference`
                                              (LocalVar `Set.map` defineSet i)
  freeIs   = [ Free vs | let vs = Set.difference used live
                       , not (Set.null vs) ]

instance InsertFree VMFun where
  insertFree fun = fun { vmfBlocks = insertFree <$> vmfBlocks fun }

instance InsertFree Module where
  insertFree m = m { mFuns = insertFree <$> mFuns m }

instance InsertFree Program where
  insertFree p = p { pModules = insertFree <$> pModules p }

