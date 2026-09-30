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

import Data.List (find, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Semigroup (First(..))
import MonadLib

import Data.Parameterized.Some

import Daedalus.Panic
import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.Pass



import Daedalus.AST(nameScopeAsModScope)
import Daedalus.Type.AST
import Daedalus.Type.Traverse

import Daedalus.Type.Free
import Daedalus.Specialise.Monad
import Daedalus.Specialise.Unify
import Data.Maybe (isNothing)

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


-- | Does this declaration need to be turned into monomorphic instances?
-- True if it has type parameters, or a class/grammar/higher-order value
-- parameter.
isTemplateDecl :: TCDecl a -> Bool
isTemplateDecl d =
  not (null (tcDeclTyParams d)) || any isHigherOrder (tcDeclParams d)
  where
  isHigherOrder p =
    case p of
      ValParam x     ->
        case tcType x of
          Type (TFun _ _) -> True
          _               -> False
      ClassParam _   -> True
      GrammarParam _ -> True

-- | This assumes that the declratations are in dependency order.
specialise :: [Name] -> [Rec (TCDecl SourceRange)]
              -> PassM (Either String [TCDecl SourceRange])
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

    -- Skip the whole group if it is dead code.
    go (MutRec ds) = do
      extInsts  <- getPendingSpecs (map tcDeclName ds)
      reachable <- or <$> mapM (seenRule . tcDeclName) ds
      if Map.null extInsts && not reachable
        then pure []
        else goOneRoot ds extInsts

    goOneRoot :: [TCDecl SourceRange] ->
                 Map Name [Instantiation] -> PApplyM [TCDecl SourceRange]
    goOneRoot ds extInsts =
      do normalOut <- mapM specialiseOne normal
         newInsts  <- getPendingSpecs (map tcDeclName ds)
         let todo = Map.unionWith (++) extInsts newInsts
         (normalOut ++) <$> goOne maxRecursiveInstantiations templates todo []
      where
      (normal, templates) = partition (not . isTemplateDecl) ds

    -- Keep processing newly requested instances until the recursive group
    -- reaches a fixed point.  There are malformed programs for which
    -- specialization produces an infinite sequence of distinct instances,
    -- so put a deterministic bound on the amount of work done for one root.
    goOne :: Int ->
             [TCDecl SourceRange] ->
             Map Name [Instantiation] ->
             [TCDecl SourceRange] -> PApplyM [TCDecl SourceRange]
    goOne fuel templates todo done =
      case popInstantiation todo of
        Nothing -> pure (reverse done)
        Just ((n, inst), todoRest)
          | fuel <= 0 ->
              raise $ unlines
                [ "Specialization of a recursive group did not terminate."
                , "The group contains: " ++
                    show (commaSep (map (pp . tcDeclName) templates))
                , "Still pending: " ++ show (pendingSummary todo)
                ]
          | Just d <- findDecl n templates -> do
              d' <- specialiseOne =<< apInst inst d
              newTodo <- getPendingSpecs (map tcDeclName templates)
              let todo' = Map.unionWith (++) todoRest newTodo
              goOne (fuel - 1) templates todo' (d' : done)
          | otherwise -> panic "Missing declaration in recursive group"
                               [show (pp n)]

    findDecl n = find ((n ==) . tcDeclName)

    popInstantiation todo =
      case Map.minViewWithKey todo of
        Nothing -> Nothing
        Just ((n, inst : insts), rest) ->
          let rest' | null insts = rest
                    | otherwise  = Map.insert n insts rest
          in Just ((n, inst), rest')
        Just ((_, []), _) -> panic "Empty instantiation list" []

    pendingSummary todo =
      commaSep
        [ pp n <+> parens (text (show (length is)) <+> "instances")
        | (n, is) <- Map.toList todo
        ]

    maxRecursiveInstantiations = 20


-- This function traverses a term and replaces all problematic
-- function calls by specialised versions
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
specialiseCall m n ts args
  | [] <- ts, all isNothing probArgs = do
      addSeenRule (tcName n)
      pure (TCCall n [] args)
  | otherwise = requestSpec m n ts probArgs args
  where
    probArgs = map probArg args

    -- If it is a partially applied function, we inline.
    probArg arg | Type (TFun _ _) <- typeOf arg = Just arg
    -- Any non-function typed value is left alone
    probArg (ValArg _) = Nothing
    -- Anything else is inlined.
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
             (map (ValArg . syntheticTC . TCVar) params ++ remainingArgs)

syntheticTC :: TCF SourceRange k -> TC SourceRange k
syntheticTC = annotExpr synthetic
