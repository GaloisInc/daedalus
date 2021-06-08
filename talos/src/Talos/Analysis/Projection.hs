{-# LANGUAGE OverloadedStrings #-}

module Talos.Analysis.Projection where

import Daedalus.Core
import Talos.Analysis.EntangledVars
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Daedalus.Core.Free
import Data.Map (Map)
import Daedalus.Core.Type
import Data.Maybe (mapMaybe, fromMaybe)
import Daedalus.PP
import Daedalus.Panic

-- -----------------------------------------------------------------------------
-- Expression level analysis
--
-- Calculate the EVs used in an expression relative to a given
-- projection (field set) on the result.  We can always just return
-- freeVars for the argument, but the hope is that we can do a bit
-- better.


projectStruct :: (Type -> Maybe Expr) -> FieldSet -> [(Label, Expr)] -> [(Label, Expr)]
projectStruct dflt fset = mapMaybe go
  where
    go (l, e) = (,) l <$> case Map.lookup l (getFieldSet fset) of
      Nothing    -> dflt (typeOf e)
      Just fset' -> Just (projectE dflt fset' e)

-- This function is pretty simple at the moment --- it just projects
-- out of structs (and under e.g. let and case)
projectE :: (Type -> Maybe Expr) -> FieldSet -> Expr -> Expr
projectE deflt fset expr
  | fset == emptyFieldSet = expr
  | otherwise             = go expr
  where
    go e = case e of
      Var {}           -> e
      PureLet n e' e'' -> PureLet n e' (go e'')
      Struct ut flds -> Struct ut (projectStruct deflt fset flds)
      ECase cs       -> ECase (go <$> cs)
      Ap0 {}         -> e
      Ap1 {}         -> e
      Ap2 {}         -> e
      Ap3 {}         -> e
      ApN {}         -> e

entangledVarsApp :: Set Name -> Expr -> Maybe EntangledVars
entangledVarsApp env = go []
  where
    go acc (Var n)
      | n `Set.member` env = Nothing
      | otherwise          =
        Just $ singletonEntangledVars (ProgramVar n) (pathToFieldSet acc)
    go acc (Ap1 (SelStruct _ l) e) = go (l : acc) e
    go _   _ = Nothing

entangledVarsE :: Expr -> EntangledVars
entangledVarsE = go Set.empty
  where
    go env expr =
      let children = foldMapChildrenE (go env) expr
      in case expr of
        Var n | n `Set.member` env -> mempty
              | otherwise          -> singletonEntangledVars (ProgramVar n) emptyFieldSet
        PureLet n e e' -> go env e <> go (Set.insert n env) e'
        Struct {}      -> children
        ECase {}       -> children
        Ap0 {}         -> children
        Ap1 (SelStruct {}) _ -> fromMaybe children (entangledVarsApp env expr)
        Ap1 {}         -> children
        Ap2 {}         -> children
        Ap3 {}         -> children
        ApN {}         -> children

freeEntangledVars :: FieldSet -> Expr -> EntangledVars
freeEntangledVars fset e = entangledVarsE (projectE (const Nothing) fset e)

freeVarsToEntangledVars :: FreeVars a => a -> EntangledVars
freeVarsToEntangledVars = EntangledVars . Map.fromSet (const emptyFieldSet ) . Set.map ProgramVar . freeVars

--------------------------------------------------------------------------------
-- Type inhabitants

typeToInhabitant :: Map TName TDecl -> Type -> Expr
typeToInhabitant tdecls = go
  where
    go ty = case ty of
      TStream    -> arrayStream (byteArrayL "array") (byteArrayL mempty)
      TUInt {}   -> intL 0 ty
      TSInt {}   -> intL 0 ty
      TInteger   -> intL 0 ty
      TBool      -> boolL False
      TUnit      -> unit
      TArray (TUInt (TSize 8)) -> byteArrayL mempty
      TArray t   -> arrayL t []
      TMaybe t -> nothing t
      TMap tk tv -> mapEmpty tk tv
      TBuilder t -> newBuilder t
      TIterator t -> newIterator (go t)
      TUser ut     -> goUT ut
      TParam _     -> panic "Saw a type param" []

    goUT ut
      | Just decl <- Map.lookup (utName ut) tdecls =
          case tDef decl of
            TStruct fs -> Struct ut [ (l, go ty) | (l, ty) <- fs ]
            TUnion  ((l, ty) : _) -> inUnion ut l (go ty)
            TUnion  _ -> panic "Empty union" [showPP ut]
      | otherwise = panic "Unknown user type " [showPP ut]

-- -- fixme: this is the fallback case
-- class FreeEntangledVars a where
--   freeEntangledVars :: FieldSet -> a -> EntangledVars

-- instance FreeEntangledVars Expr where
--   freeEntangledVars _fset =
--     EntangledVars . Map.fromSet (const emptyFieldSet ) . Set.map ProgramVar . freeVars

-- instance FreeEntangledVars ByteSet where
--   freeEntangledVars _fset =
--     EntangledVars . Map.fromSet (const emptyFieldSet ) . Set.map ProgramVar . freeVars

-- instance FreeEntangledVars Grammar where
--   freeEntangledVars _fset =
--     EntangledVars . Map.fromSet (const emptyFieldSet ) . Set.map ProgramVar . freeVars
