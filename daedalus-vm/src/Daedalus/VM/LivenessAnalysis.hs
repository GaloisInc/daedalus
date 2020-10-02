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
    (live,iss) = mapAccumL updI (freeVars (blockTerm b))
                                (reverse (blockInstrs b))
    freeIs     = [ Free v | v' <- blockArgs b
                          , let v = ArgVar v'
                          , not (v `Set.member` live) ]
    newIs      = freeIs ++ concat (reverse iss)

updI :: Set VMVar -> Instr -> (Set VMVar, [Instr])
updI live i = (newLive, i : freeIs)
  where
  used     = freeVars i
  newLive  = Set.union used live `Set.difference` (LocalVar `Set.map` defines i)
  freeIs   = [ Free v | v <- Set.toList used, not (v `Set.member` live) ]

instance InsertFree VMFun where
  insertFree fun = fun { vmfBlocks = insertFree <$> vmfBlocks fun }

instance InsertFree Module where
  insertFree m = m { mFuns = insertFree <$> mFuns m }

instance InsertFree Program where
  insertFree p = p { pModules = insertFree <$> pModules p }

