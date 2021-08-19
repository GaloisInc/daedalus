
{- | In this module we normalize the "names" (i.e., unique ids) for bound
type variables.  This helps when generating Haskell code, for example,
as polymorphic functions end up with more stable names, which should
reduce recompilation.

IMPORTANT: we assume that there are no nested quantifiers, which should
be true at the moment, but of we add a construct that requires that,
we need to revisit the code in this module -}
module Daedalus.Type.NormalizeTypeVars where

import Data.Map(Map)
import qualified Data.Map as Map

import Daedalus.Type.AST
import Daedalus.Type.Subst
import Daedalus.Type.Traverse


renameBound :: [TVar] -> ([TVar], Map TVar Type)
renameBound xs = (as, Map.fromList [ (x,TVar a) | (x,a) <- zip xs as ])
  where
  as      = zipWith ren xs [0..]
  ren x n = x { tvarId = n }

normTCModule :: TCModule a -> TCModule a
normTCModule m = m
  { tcModuleTypes = fmap (fmap normTCTyDecl) (tcModuleTypes m)
  , tcModuleDecls = fmap (fmap normTCDecl)   (tcModuleDecls m)
  }

normTCTyDecl :: TCTyDecl -> TCTyDecl
normTCTyDecl tc = tc { tctyParams = as, tctyDef = apSubstT su (tctyDef tc) }
  where
  (as,su) = renameBound (tctyParams tc)

normTCDecl :: TCDecl a -> TCDecl a
normTCDecl d = mapTypes (apSubstT su) d { tcDeclTyParams = as }
  where
  (as,su) = renameBound (tcDeclTyParams d)
