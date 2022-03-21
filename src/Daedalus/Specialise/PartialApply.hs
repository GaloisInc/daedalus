{-# LANGUAGE TupleSections, DataKinds, GADTs #-}

module Daedalus.Specialise.PartialApply (partialApply) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Daedalus.Panic
import Daedalus.Pass
import Daedalus.PP

import Daedalus.Type.AST
import Daedalus.Type.Subst
import Daedalus.Type.Traverse
import Daedalus.Type.RefreshGUID

{- | Generate a sepcialized version of a declaration.
PRE: newPs and tcFrees (args) are disjoint from bound vars in the decl.

NOTE: an instantiated declaration may end up with more paramteres than
the original.  Consider, for example:

@
    f P = ...
    g   = f (Q xs ys)

~~>
    f xs ys = ... [ Q xs ys / P ]
    g       = f xs ys
@
-}
-- FIXME: we don't rename here as the GUID guarantees that we have
-- unique names (ignoring the scopedIdent).  We _will_ need to refresh
-- the decl.
partialApply :: 
  Name            {- ^ New name for instantiate declaration -}   ->
  [Type]          {- ^ Concrete types to use for the instance. -} ->
  [TCName Value]  {- ^ Additional paramteres for the instances. See NOTE -} ->
  [Maybe (Arg a)] {- ^ Concrete values to use for instances, possible only partially applied -} ->
  TCDecl a        {- ^ Uninstantiated declaration -} ->
  PassM (TCDecl a)
partialApply tnm' targs newPs args
  TCDecl { tcDeclTyParams = ttys,
           tcDeclParams   = tparams,
           tcDeclDef      = tdef,
           tcDeclCtxt     =  tctxt,
           tcDeclAnnot    = tannot
         }
  =
  refreshDecl $ TCDecl {
    tcDeclName     = tnm',
    tcDeclTyParams = [],
    tcDeclCtrs     = [],
    tcDeclImplicit = [],
    tcDeclParams   = newPs' ++ tparams',
    tcDeclDef      = tdef',
    tcDeclCtxt     = tctxt,
    tcDeclAnnot    = tannot
  }

  where
    tdef' = case mapTypes (apSubstT substT) tdef of
              ExternDecl t -> ExternDecl t
              Defined d    -> Defined (apSubst subst d)

    -- Infinitely many args to support partially applied
    -- instantiations.
    argsExtended = args ++ repeat Nothing

    -- FIXME: we should ensure that the new tys don't overlap.
    tparams' = [ mapTypes (apSubstT substT) p | (p, Nothing) <- zip tparams argsExtended ]
    newPs'   = map ValParam newPs

    substT :: Map TVar Type
    substT = Map.fromList (zip ttys targs)

    -- Make a substitution for the new args
    subst = foldl (flip mkOne) emptySubst $ catMaybes
            $ zipWith (\x y -> (x, ) <$> y) tparams args -- don't need extended here, as we drop Nothings
    mkOne (ValParam p,     ValArg e)     = addSubst p (texprValue e)
    mkOne (GrammarParam p, GrammarArg e) = addSubst p (texprValue e)
    mkOne (p, a) = panic ("Mismatched argument kinds: " ++ show (pp p) ++ " " ++ show (pp a))  []

    -- We need to rename the free variables in the arguments s.t. they
    -- aren't captured by binders in tdef.  This might mean renaming
    -- variables from newPs as well.
    -- bounds = tcBounds tdef `Set.union` (Set.fromList (map paramToName tparams))
    -- frees  = tcFree args
    -- renamers = Set.intersection bounds frees

    -- We don't use a Subst directly as we want to rename the new
    -- params as well.  We could reuse Unifier, but that may change to
    -- be term valued in future (which could then arguably be a
    -- Subst).
    -- argsRename :: MapF TCName TCName
    -- argsRename = foldl (\m (Some k) -> MapF.insert k (makeFreshFor renamers k) m)
    --                    MapF.empty renamers

    -- FIXME: breaks Subst abstraction
    -- argsSubst = fmapF TCVar argsRename

    -- argsNoBound = map (fmap (apSubstArg argsSubst)) args
    -- newPsNoBound =
    --   map (\x -> fromMaybe x (MapF.lookup x argsRename)) newPs

-- paramToName :: Param -> Some TCName
-- paramToName (ValParam     p) = Some p
-- paramToName (GrammarParam p) = Some p
-- paramToName (ClassParam   p) = Some p
