{-# Language BlockArguments, OverloadedStrings #-}
{-# Language NamedFieldPuns, RecordWildCards #-}
module Daedalus.Type.Generalize(generalize,DeclInfo(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph.SCC(stronglyConnComp)
import Data.Graph(SCC(..))
import Data.List(foldl')
import Data.Maybe(mapMaybe)
import Control.Monad(forM,forM_,unless)

import Daedalus.PP

import Daedalus.Type.AST
import Daedalus.Type.Monad
import Daedalus.Type.Traverse
import Daedalus.Type.Subst
import Daedalus.Type.Constraints

-- | This is result of generalization.
data DeclInfo = DeclInfo
  { tcDecls     :: Rec (TCDecl SourceRange)
    -- ^ A generalized recursive group.

  , tcTypeDefs  :: Map TCTyName TCTyDecl
    -- ^ Type declarations computed from the declarations.
  }



-- | Generalize the types of a type-checked group of definitions.
generalize :: Rec (TCDecl SourceRange) -> STypeM DeclInfo
generalize ds =
  do -- Simplify constraints.
     -- Includes adding definitions for "inferred" type defintitions.
     (lcs,tds,ds1) <- simpCtrs ds

     -- Check no left-over mono types
     cs <- forM lcs \lc ->
              case thingValue lc of
                IsNamed _ -> reportDetailedError lc
                  "Failed to infer expression type."
                  [ "Expressions examined with `case` or `is`" <+>
                                                    "need a concrete type."
                  , "Plese use a type annotation to specify it."
                  ]
                x         -> pure x

     -- Check that all types that needed definitions were defined
     todo <- getNeedsDef
     forM_ todo \l ->
         do let nm = thingValue l
            unless (nm `Map.member` tds) $
               do mb <- lookupTypeDef nm
                  case mb of
                    Nothing -> reportError l ("Name" <+> backticks (pp nm)
                                         <+> "does not refer to a named type.")
                    _ -> pure ()


     -- Since we don't have local definitions there should be no free
     -- type variables in the environment
     let freeInTys = completeFreeInTD tds
         as        = Set.toList
                   $ Set.unions ( freeTVS ds1
                                : freeTVS cs
                                : Map.elems freeInTys
                                )

     pure $ doGeneralize as cs (Set.toList <$> freeInTys)
            DeclInfo { tcDecls    = ds1
                     , tcTypeDefs = tds
                     }

--------------------------------------------------------------------------------



simpCtrs :: Rec (TCDecl SourceRange) ->
            STypeM ( [Located Constraint]
                   , Map TCTyName TCTyDecl
                   , Rec (TCDecl SourceRange)
                   )
simpCtrs ds =
  do lcs  <- simplifyConstraints
     ds1  <- traverse (traverseTypes zonkT) ds
     tds  <- getNewTypeDefs
     let (lcs1,tds1,ds2,hasRen) = renameAnonTC lcs tds ds1

     -- if some types were renamed, add back constraints and keep simplifying
     if hasRen
       then do forM_ lcs1 \lc -> addConstraint lc (thingValue lc)
               replaceNewTypeDefs tds1
               simpCtrs ds2
       else pure (lcs1,tds1,ds2)



{- | If the result of a declaration is an anonymous named type that
matches the declaration's name, then we make this type not-anonymous.
Note that as a result, we may have to revisit some of the existing
constraints (e.g. Has), which were not solved because the non-anonymous
type was not yet defined.

For example, if the result of `X` happans to be `X-Anon1`, then we just
replace all occurances of `X-Anon1` with `X`
-}
renameAnonTC ::
  [Located Constraint] ->
  Map TCTyName TCTyDecl ->
  Rec (TCDecl SourceRange) ->
  ( [Located Constraint]
  , Map TCTyName TCTyDecl
  , Rec (TCDecl SourceRange)
  , Bool
  )
renameAnonTC lcs tys ds
  | Map.null renSu = (lcs,tys,ds,False)
  | otherwise = ( [ mapTypes renTC lc | lc <- lcs ]
                , Map.fromList
                      [ (tctyName d,d) | d <- map renTD (Map.elems tys) ]
                , renD <$> ds
                , True
                )
  where
  -- renaming substitution
  renSu = Map.fromList (mapMaybe shouldRename (recToList ds))

  -- replace anonymous types with the entries from the renaming substitutin.
  renTC ty = case ty of
               TVar _ -> ty
               TCon x ts -> TCon y (map renTC ts)
                  where y = Map.findWithDefault x x renSu
               Type tf -> Type (renTC <$> tf)

  renD = mapTypes renTC

  renTD t = let nm = Map.findWithDefault (tctyName t) (tctyName t) renSu
            in t { tctyName = nm
                 , tctyDef = mapTypes renTC (tctyDef t)
                 }

  -- Check if the result type of declaration matches one of the
  -- anonymous types from the declaration's definition.
  shouldRename TCDecl { tcDeclDef, tcDeclName } =
    case typeOf tcDeclDef of
      Type (TGrammar ty) -> matches ty
      ty -> matches ty
    where
    matches ty =
      case ty of
        TCon nm@(TCTyAnon x _) _
          | tcDeclName == x -> Just (nm, TCTy x)
        _ -> Nothing


--------------------------------------------------------------------------------


{- | Free variables in a collection of (possibly recursive) type declarations.
Example:

    data T1 = MkT1 T2
    data T2 = MkT2 ?a T1

We'd like to generalize the free variable `?a`.  Note that it is not enough
to compute just the free variables in each type individually, as this would
give us:

    data T1   = MkT1 T2     -- WRONG:
    data T2 a = MkT2 a T1

To get the correct answer each type needs not only its parameters, but
also the paraemetrs for its dependency.  Mutually recursive types, like
in the above example, are processed together.
-}

completeFreeInTD :: Map TCTyName TCTyDecl -> Map TCTyName (Set TVar)
completeFreeInTD tds = foldl' addFree Map.empty
                     $ stronglyConnComp
                     $ map getDeps
                     $ Map.elems tds
  where
  freeInTys = freeTVS <$> tds

  getDeps d = let me   = tctyName d
                  deps = Set.toList (collectTypes freeTCons (tctyDef d))
              in ((me,deps),me,deps)

  lkp mp x = Map.findWithDefault Set.empty x mp
  addFree mp scc =
    let one (me,deps) = Set.unions (lkp freeInTys me : map (lkp mp) deps)
    in case scc of
         AcyclicSCC t -> Map.insert (fst t) (one t) mp
         CyclicSCC ts -> let vs      = Set.unions (map one ts)
                             ins m x = Map.insert (fst x) vs m
                          in foldl' ins mp ts



--------------------------------------------------------------------------------
-- Real generalization



-- | This abstracts the given variables and makes the recursive group of
-- types.  It happens after all the type declaring shenanigans.
doGeneralize :: [TVar] {- ^ Params for decl -} ->
                [Constraint] {- ^ Constraints on type (for decl) -} ->
                Map TCTyName [TVar] {- ^ Params for each type -} ->
                DeclInfo -> DeclInfo
doGeneralize as cs tparams decls
  | null as   = decls -- tparams are asssumed to be a subset of as
  | otherwise = DeclInfo
                  { tcTypeDefs = addTPsTy <$> tcTypeDefs decls
                  , tcDecls =
                      case tcDecls decls of
                        NonRec d  -> NonRec (addTPs False d)
                        MutRec ds -> MutRec (map (addTPs True) ds)
                  }
  where
  ts      = map TVar as
  tconMap = map TVar <$> tparams
  dMap    = Map.fromList [ (tcDeclName d, ts) | d <- recToList (tcDecls decls) ]

  addTPs :: Bool -> TCDecl SourceRange -> TCDecl SourceRange
  addTPs r TCDecl { .. } =
           TCDecl { tcDeclName     = tcDeclName
                  , tcDeclTyParams = as
                  , tcDeclCtrs     = map (fixUpTCons tconMap) cs
                  , tcDeclParams   = fixUpTCons tconMap tcDeclParams
                  , tcDeclDef      =
                      case fixUpTCons tconMap tcDeclDef of
                        Defined d | r -> Defined (fixUpRecCallSites dMap d)
                        res ->  res
                  , tcDeclCtxt     = tcDeclCtxt
                  , tcDeclAnnot    = tcDeclAnnot
                  }

  addTPsTy d = fixUpTCons tconMap d { tctyParams = tparams Map.! tctyName d }


fixUpTCons :: TraverseTypes a => Map TCTyName [Type] -> a -> a
fixUpTCons mp = if Map.null mp then id else mapTypes (fixUpTConsT mp)

fixUpTConsT :: Map TCTyName [Type] -> Type -> Type
fixUpTConsT mp ty =
  case ty of
    TVar {} -> ty
    TCon x [] | Just ts <- Map.lookup x mp -> TCon x ts
    TCon x ts -> TCon x (map (fixUpTConsT mp) ts)
    Type tf -> Type (fixUpTConsT mp <$> tf)

fixUpRecCallSites :: Map Name [Type] -> TC SourceRange k -> TC SourceRange k
fixUpRecCallSites ch expr =
  exprAt expr $
    case mapTCF (fixUpRecCallSites ch) (texprValue expr) of
      TCCall x [] es | Just ts <- Map.lookup (tcName x) ch -> TCCall x ts es
      e                                                    -> e
