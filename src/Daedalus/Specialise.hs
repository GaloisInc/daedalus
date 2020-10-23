{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, NamedFieldPuns #-}
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, DataKinds, TupleSections #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, PolyKinds #-}
{-# LANGUAGE RecordWildCards #-} -- for dealing with TCDecl and existential k

{- |
Specialise rules s.t. polymorphic rules and those which have
grammar arguments are removed.  This also has the effect of
removing the information about recursive grouping (FIXME: we could
preserve it).

Consider a declaration:

    f as x P = e      -- `as` are the type parameters to the function

for each call site, A, `f ts x (Q y)` we generate a new function:

    f_A x y = e [ts/as] [(Q y/P]

we also try to reuse instances, so if there are some other call sites,
for examle:
  B: f ts y (Q z)
  C: f ts y (Q y)

we are going to just reuse `f_A` like this:
  B: f_A y z
  C: f_A y y

Type arguments are just compared for equality, while other arguments
are unified.  See 'Specialise.Unfiy' for details.
-}


module Daedalus.Specialise (specialise, regroup) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Semigroup (First(..))
import MonadLib

import Data.Parameterized.Some

import Daedalus.Panic
import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.GUID



import Daedalus.AST(nameScopeAsModScope)
import Daedalus.Type.AST
import Daedalus.Type.Traverse

import Daedalus.Type.Free
import Daedalus.Specialise.Monad
import Daedalus.Specialise.Unify

-- -----------------------------------------------------------------------------
-- Top level driver


regroup :: [TCDecl SourceRange] -> Map ModuleName [Rec (TCDecl SourceRange)]
regroup = fmap reverse . foldr addR Map.empty . topoOrder
  where
  owner d = case nameScopedIdent (tcDeclName d) of
              ModScope m _ -> m
              _ -> panic "regroup" [ "Declaration is not ModScope" ]

  addR r mp =
    case r of
      NonRec d -> Map.insertWith (++) (owner d) [r] mp
      MutRec ds -> case map owner ds of
                     x : xs | all (== x) xs ->
                              Map.insertWith (++) x [r] mp
                     _ -> panic "recroup.addR" ["Oops"]


-- | This assumes that the declratations are in dependency order.
specialise :: HasGUID m => [Name] -> [Rec (TCDecl SourceRange)]
              -> m (Either String [TCDecl SourceRange])
specialise ruleRoots decls =
  runPApplyM ruleRoots (concat . reverse <$> mapM go (reverse decls))
  where
    -- First we find if we need to generate partial applications.  If we
    -- do so, we can discard the input tdecl (FIXME: I think?), the
    -- reasoning being that specialised decls are problematic otherwise.
    go (NonRec d) = do
      insts <- getPendingSpecs [tcDeclName d]
      seen  <- seenRule (tcDeclName d)
      ds    <- case Map.lookup (tcDeclName d) insts of
                 Just is        -> mapM (flip apInst d) is -- forget d
                 Nothing | seen -> pure [d]
                 Nothing        -> pure []
      mapM specialiseOne ds

    -- We treat each inst. req. independently; the unify stuff will
    -- make sure it doesn't explode.  We check for unsupported
    -- recursion by checking for new recursive reqs. after completing
    -- all decls in the ds.
    go (MutRec ds) = do
      insts <- getPendingSpecs (map tcDeclName ds)
      -- We know that any recursive element we have seen doesn't have
      -- grammar args, so is a reasonably place to start if we have no
      -- specialisations.  Note that we may have specialisations that
      -- are internal to the recursive group.
      (seen, unseen) <- partitionM (seenRule . tcDeclName) ds
      let roots = [ Map.singleton n [i] | (n, is) <- Map.toList insts, i <- is ]
      case (roots, seen) of
        ([], [])      -> pure []
        ([], _ )      -> goOneRoot (seen ++ unseen) mempty
        _             -> concat <$> mapM (goOneRoot ds) roots

    goOneRoot :: [TCDecl SourceRange] ->
                 Map Name [Instantiation] -> PApplyM [TCDecl SourceRange]
    goOneRoot ds todo = do
      rs <- goOne ds todo
      insts <- getPendingSpecs (map tcDeclName ds)
      if not (Map.null insts)
        then raise ("Incompatible recursion detected for " ++ show (ppError insts))
        else return rs

    ppError insts =
      punctuate ", " $ map (\(k, is) -> pp k <+> vcat (map pp is)) (Map.toList insts)

    -- The idea here is that we process each decl exactly once, so
    -- that if we see the decl again (after we are done), then
    -- something is wrong with the input.
    goOne :: [TCDecl SourceRange] ->
             Map Name [Instantiation] -> PApplyM [TCDecl SourceRange]
    goOne [] _    = pure []
    goOne ds@(d : rest) todo =
      case Map.minViewWithKey todo of
        -- Fall back to just processing the first decl.
        Nothing ->
            do d'      <- specialiseOne d
               newTodo <- getPendingSpecs (map tcDeclName rest)
               (d' :) <$> goOne rest newTodo

        Just ((n, [inst]), todoRest)
          | (ds', d' : ds'') <- break (\di -> tcDeclName di == n) ds ->
            do let newds = ds' ++ ds''
               -- let d' = apInst inst d
               d'' <- specialiseOne =<< apInst inst d'
               newTodo <- getPendingSpecs (map tcDeclName newds)
               (d'' :) <$> goOne newds (Map.unionWith (++) todoRest newTodo)

        Just ((n, _ : _ : _), _) ->
          raise ("Multiple instantiations requested for " ++ show (pp n))
        Just ((_, []), _) -> panic "Empty instantiation list" []
        _ -> panic "Impossible" []


-- This function traverses a term and replaces all problematic
-- function calls by speciialised versions
specialiseOne :: TCDecl SourceRange -> PApplyM (TCDecl SourceRange)
specialiseOne TCDecl {..}
  | not (null tcDeclTyParams) = panic "specialiseOne"
                                      ["Specializing a poly function"]
  | otherwise =
  case tcDeclDef of
    ExternDecl _ -> pure TCDecl { .. }
    Defined d -> do tdef <- go d
                    pure (TCDecl { tcDeclDef = Defined tdef, .. })
  where
    go :: forall k'. TC SourceRange k' -> PApplyM (TC SourceRange k')
    go (TC v) = TC <$> traverse go' v

    go' :: forall k'. TCF SourceRange k' -> PApplyM (TCF SourceRange k')
    go' texpr =
      case texpr of
        -- FIXME: maype specialise simple recursive case?
        TCCall n ts as -> do
          as' <- mapM (traverseArg go) as
          let m = fst (nameScopeAsModScope tcDeclName)
          specialiseCall m n ts as'
        x -> traverseTCF go x

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs = do
  (yess, nos) <- foldM go ([], []) xs
  pure (reverse yess, reverse nos)
  where
    go (yes, no) x = do r <- p x
                        pure $ if r then (x : yes, no) else (yes, x : no)



-- -----------------------------------------------------------------------------
-- Specialisation policy

-- This is the main policy function --- this determines how and when a
-- function call necessitates a specialised version.  Returns a new
-- call if required.
specialiseCall ::
  ModuleName        {- ^ Name of the module containing the call -} ->
  TCName k          {- ^ Call this function -} ->
  [Type]            {- ^ With these types -} ->
  [Arg SourceRange] {- ^ And these concrete arguments -} ->
  PApplyM (TCF SourceRange k)

-- No specialisation required if there are no type args, and no grammar args.
specialiseCall _ n [] args | all isValArg args = do
  addSeenRule (tcName n)
  pure (TCCall n [] args)
  where
    isValArg (ValArg _ ) = True
    isValArg _           = False

-- We have some type args and/or some grammar args.
specialiseCall m nm ts args = requestSpec m nm ts probArgs args
  where
    probArgs = map probArg args

    probArg (ValArg _) = Nothing
    probArg arg        = Just arg

-- -----------------------------------------------------------------------------

{- Request a specialisation

We want to specialise a call, this checks to see if it unifies with
an existing spec. request.
-}
requestSpec ::
  ModuleName            {- ^ Name of the module containing the call -} ->
  TCName k              {- ^ Specialize this -} ->
  [Type]                {- ^ Using these type arguments -} ->
  [Maybe (Arg SourceRange)]
                        {- ^ And these arguments: Nothing = leave as arg -}->
  [Arg SourceRange]     {- ^ All original arguments -} ->
  PApplyM (TCF SourceRange k)
requestSpec m tnm ts args origArgs = do
  rs <- lookupRequestedSpecs (tcName tnm)
  case rs of
    Just insts | Just (First call) <- foldMap findUnifier insts
                 -> pure call
    _ -> do nm' <- addSpecRequest m (tcName tnm) ts newPs args
            pure (mkCall nm' newPs)
  where
    newPs = map getValue (Set.toList (tcFree args))

    getValue :: Some TCName -> TCName Value
    getValue (Some tnm'@(TCName { tcNameCtx = AValue })) = tnm'
    getValue (Some tnm') =
        panic "requestSpec"
          [ "Saw a non-Value free variable: " ++ show (pp tnm')
          , "Requesting " ++ show (pp tnm)
          , "Args: " ++ show (hsep $ map ppA args)
          ]

    ppA Nothing = text "_"
    ppA (Just v) = pp v

    findUnifier inst
      | ts == instTys inst
      , Right unifier <- unify args (instArgs inst)
      = let params = map (apUnifier unifier) (instNewParams inst)
        in Just (First (mkCall (instNewName inst) params))

    findUnifier _ = Nothing

    remainingArgs = [ a | (a, Nothing) <- zip origArgs args ]

    -- We don't pass any type args, as the target should be monomorphic
    mkCall nm' params =
      TCCall (tnm { tcName = nm' }) []
             (remainingArgs ++ map (ValArg . syntheticTC . TCVar) params)

syntheticTC :: TCF SourceRange k -> TC SourceRange k
syntheticTC = annotExpr synthetic



