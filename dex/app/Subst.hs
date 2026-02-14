module Subst where

import Data.Map.Strict(Map)
import Data.Map.Strict qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set

import Name
import Type
import AST



--------------------------------------------------------------------------------

type Subst a = Map Name (Type a)

(@@) :: Subst a -> Subst a -> Subst a
su2 @@ su1 = Map.union (apSubstT su2 <$> su1) su2

mergeSubst :: Ord a => Subst a -> Subst a -> Maybe (Subst a)
mergeSubst x y
  | Map.null (Map.filter not (Map.intersectionWith (==) x y)) = pure (Map.union x y)
  | otherwise = Nothing

apSubstT :: Subst a -> Type a -> Type a
apSubstT su ty =
  case ty of
    TVar x       -> Map.findWithDefault ty (locThing x) su
    Type f es ns -> Type f (map (apSubstT su) es) ns

apForeignSubstBasicExporterType :: Subst b -> BasicExporterType a b -> BasicExporterType a b
apForeignSubstBasicExporterType su (a :-> b) = a :-> apSubstT su b

-- | Apply a substitution to the foreign types in an exporter
apForeignSubstExp :: Subst b -> Exporter a b -> Exporter a b
apForeignSubstExp su expr =
  case expr of
    ExportTop f cs ts fs ty ->
      ExportTop f cs (map (apSubstT su) ts) (map (apForeignSubstExp su) fs) 
                      (apForeignSubstBasicExporterType su <$> ty)
    ExportLocal f ty ->
      ExportLocal f (apForeignSubstBasicExporterType su <$> ty)


-- | Check if we can instantiate the variables in the first type
-- in a way that would make it equal to the second type.
matchType :: Ord a => Type a -> Type a -> Maybe (Subst a)
matchType t1 t2 =
  case (t1,t2) of
    (Type f xs ys, Type g as bs)
      | locThing f == locThing g && ys == bs -> matchMany xs as
    (TVar x, t) -> pure (Map.singleton (locThing x) t)
    _ -> Nothing

matchMany :: Ord a => [Type a] -> [Type a] -> Maybe (Subst a)
matchMany xs ys =
  case (xs, ys) of
    (x : xs', y : ys') ->
      do
        su1 <- matchType x y
        su2 <- matchMany xs' ys'
        mergeSubst su1 su2

    ([], []) -> pure mempty
    _ -> Nothing

-- | Find the most general unifier of two types.
-- If two variables are to be unified together, we prefer binding the one that's
-- in the given bind set.
unifyType :: Eq a => Set Name -> Set Name -> Type a -> Type a -> Maybe (Subst a)
unifyType preferB skolem t1 t2 =
  case (t1,t2) of
    (Type f xs ms, Type g ys ns)
      | locThing f == locThing g && ms == ns -> unifyTypes preferB skolem xs ys
      | otherwise -> Nothing
    (TVar x, _) -> bindVar preferB skolem x t2
    (_, TVar x) -> bindVar preferB skolem x t1

bindVar :: Set Name -> Set Name -> Loc Name -> Type a -> Maybe (Subst a)
bindVar preferB skolem x t =
  case t of
    TVar y
      | x' == y' -> pure mempty
      | y' `Set.member` preferB -> pure (Map.singleton y' (TVar x)) -- assumes preferB and skolem are disjoin
      | x' `Set.member` skolem ->
         if not (y' `Set.member` skolem) then pure (Map.singleton y' (TVar x)) else Nothing
      | otherwise -> pure (Map.singleton x' t)
      where y' = locThing y
    _
      | x' `Set.member` skolem -> Nothing
      | occurs t -> Nothing
      | otherwise -> pure (Map.singleton x' t)
  where
  x' = locThing x
  occurs ty =
    case ty of
      Type _ ts _ -> any occurs ts
      TVar y -> x' == locThing y

unifyTypes ::
  Eq a => Set Name -> Set Name -> [Type a] -> [Type a] -> Maybe (Subst a)
unifyTypes preferB skolem xs ys =
  case (xs,ys) of
    ([],[]) -> pure mempty
    (x : xs', y : ys') ->
      do su1 <- unifyType preferB skolem x y
         let subst = map (apSubstT su1)
         su2 <- unifyTypes preferB skolem (subst xs') (subst ys')
         pure (su2 @@ su1)
    _ -> error "[BUG] `unifyTypes` length mismatch."







