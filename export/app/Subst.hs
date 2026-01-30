module Subst where

import Data.Map.Strict(Map)
import Data.Map.Strict qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set

import Daedalus.Core qualified as Core
import Name
import Type
import AST

data Su = Su {
  suTypes :: Map Core.TParam Core.Type,
  suSizes :: Map Core.TParam Core.SizeType
}

emptySu :: Su
emptySu = Su { suTypes = Map.empty, suSizes = Map.empty }

bindType :: Core.TParam -> Core.Type -> Su -> Maybe Su
bindType x ty su =
  case Map.lookup x (suTypes su) of
    Nothing -> pure su { suTypes = Map.insert x ty (suTypes su) }
    Just t
      | t == ty -> pure su
      | otherwise -> Nothing
  
bindSize :: Core.TParam -> Core.SizeType -> Su -> Maybe Su
bindSize x sz su =
  case Map.lookup x (suSizes su) of
    Nothing -> pure su { suSizes = Map.insert x sz (suSizes su) }
    Just s
      | s == sz -> pure su
      | otherwise -> Nothing

--------------------------------------------------------------------------------
-- Substitutions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | Matching types: we only bind variables in the first argument
-- Variables in the 2nd argument are treated as constants.
matchType :: Core.Type -> Core.Type -> Su -> Maybe Su
matchType pat ty su =
  case (pat,ty) of
    (Core.TStream,   Core.TStream)    -> pure su
    (Core.TInteger,  Core.TInteger)   -> pure su
    (Core.TBool,     Core.TBool)      -> pure su
    (Core.TFloat,    Core.TFloat)     -> pure su
    (Core.TDouble,   Core.TDouble)    -> pure su
    (Core.TUnit,     Core.TUnit)      -> pure su

    (Core.TParam a, _) -> bindType a ty su
    
    (Core.TUInt szP, Core.TUInt szT)  -> matchSizeType szP szT su
    (Core.TSInt szP, Core.TSInt szT)  -> matchSizeType szP szT su
    
    (Core.TArray a,  Core.TArray b)   -> matchType a b su
    (Core.TMaybe a,  Core.TMaybe b)   -> matchType a b su
    (Core.TBuilder a, Core.TBuilder b) -> matchType a b su
    (Core.TIterator a, Core.TIterator b) -> matchType a b su
    (Core.TUser ut1, Core.TUser ut2) -> matchUserType ut1 ut2 su
    (Core.TMap a b,  Core.TMap c d) -> matchMany matchType [a,b] [c,d] su
    _ -> Nothing

matchMany :: (a -> a -> Su -> Maybe Su) -> [a] -> [a] -> Su -> Maybe Su
matchMany f xs ys su =
  case (xs,ys) of
    ([],[]) -> pure su
    (a : as, b : bs) ->
      do
        su1 <- f a b su
        matchMany f as bs su1
    _ -> error "[BUG]: `matchMany` length mismatch"

matchSizeType :: Core.SizeType -> Core.SizeType -> Su -> Maybe Su
matchSizeType pat sz su =
  case (pat,sz) of
    (Core.TSizeParam x, _) -> bindSize x sz su
    (Core.TSize a, Core.TSize b) | a == b -> pure su
    _ -> Nothing

matchUserType :: Core.UserType -> Core.UserType -> Su -> Maybe Su
matchUserType u1 u2 su
  | Core.utName u1 == Core.utName u2 =
    do su1 <- matchMany matchType (Core.utTyArgs u1) (Core.utTyArgs u2) su
       matchMany matchSizeType (Core.utNumArgs u1) (Core.utNumArgs u2) su1
  | otherwise = Nothing

apSubstCore :: Su -> Core.Type -> Core.Type
apSubstCore su ty =
  case ty of
    Core.TStream -> ty
    Core.TInteger -> ty
    Core.TBool -> ty
    Core.TFloat -> ty
    Core.TDouble -> ty
    Core.TUnit -> ty

    Core.TParam a -> Map.findWithDefault ty a (suTypes su)

    Core.TUInt szP -> Core.TUInt (apSubstSize su szP)
    Core.TSInt szP -> Core.TSInt (apSubstSize su szP)
  
    Core.TArray a -> Core.TArray (apSubstCore su a)
    Core.TMaybe a -> Core.TMaybe (apSubstCore su a)
    Core.TBuilder a -> Core.TBuilder (apSubstCore su a)
    Core.TIterator a -> Core.TIterator (apSubstCore su a)
    Core.TUser ut -> Core.TUser ut {
      Core.utTyArgs = map (apSubstCore su) (Core.utTyArgs ut),
      Core.utNumArgs = map (apSubstSize su) (Core.utNumArgs ut)
      }
    Core.TMap a b -> Core.TMap (apSubstCore su a) (apSubstCore su b)

apSubstSize :: Su -> Core.SizeType -> Core.SizeType
apSubstSize su ty =
  case ty of
    Core.TSizeParam x -> Map.findWithDefault ty x (suSizes su)
    Core.TSize {} -> ty







--------------------------------------------------------------------------------

type Subst = Map Name Type

(@@) :: Subst -> Subst -> Subst
su2 @@ su1 = Map.union (apSubst su2 <$> su1) su2
  
class ApSubst t where
  apSubst :: Subst -> t -> t

instance ApSubst Type where
  apSubst su ty =
    case ty of
      TVar x    -> Map.findWithDefault ty (nameName x) su
      Type f es -> Type f (map (apSubst su) es)

instance ApSubst Exporter where
  apSubst su ex =
    case ex of
      ExportTop f cs ts -> ExportTop f cs (map (apSubst su) ts)
      ExportApp f g -> ExportApp (apSubst su f) (apSubst su g)

-- | Find the most general unifier of two types.
-- If two variables are to be unified together, we prefer binding the one that's
-- in the given bind set.
unifyType :: Set Name -> Set Name -> Type -> Type -> Maybe Subst
unifyType preferB skolem t1 t2 =
  case (t1,t2) of
    (Type f xs, Type g ys)
      | nameName f == nameName g -> unifyForeignTypes preferB skolem xs ys
      | otherwise -> Nothing
    (TVar x, _) -> bindForeignVar preferB skolem x t2
    (_, TVar x) -> bindForeignVar preferB skolem x t1

bindForeignVar :: Set Name -> Set Name -> LName -> Type -> Maybe Subst
bindForeignVar preferB skolem x t =
  case t of
    TVar y
      | x' == y' -> pure mempty
      | y' `Set.member` preferB -> pure (Map.singleton y' (TVar x)) -- assumes preferB and skolem are disjoin
      | x' `Set.member` skolem ->
         if not (y' `Set.member` skolem) then pure (Map.singleton y' (TVar x)) else Nothing
      | otherwise -> pure (Map.singleton x' t)
      where y' = nameName y
    _
      | x' `Set.member` skolem -> Nothing
      | occurs t -> Nothing
      | otherwise -> pure (Map.singleton x' t)
  where
  x' = nameName x
  occurs ty =
    case ty of
      Type _ ts -> any occurs ts
      TVar y -> x' == nameName y

unifyForeignTypes ::
    Set Name -> Set Name -> [Type] -> [Type] -> Maybe Subst
unifyForeignTypes preferB skolem xs ys =
  case (xs,ys) of
    ([],[]) -> pure mempty
    (x : xs', y : ys') ->
      do su1 <- unifyType preferB skolem x y
         let subst = map (apSubst su1)
         su2 <- unifyForeignTypes preferB skolem (subst xs') (subst ys')
         pure (su2 @@ su1)
    _ -> error "[BUG] `unifyForeginTypes` length mismatch."







