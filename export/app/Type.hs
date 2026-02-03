module Type where

import Data.Set(Set)
import Data.Set qualified as Set
import Daedalus.PP
-- import Daedalus.Core qualified as Core
import Name

{-
freeTVarsCore :: Core.Type -> Set Core.TParam
freeTVarsCore ty =
  case ty of
    Core.TStream -> Set.empty
    Core.TUInt _ -> Set.empty
    Core.TSInt _ -> Set.empty
    Core.TInteger -> Set.empty
    Core.TBool -> Set.empty
    Core.TFloat -> Set.empty
    Core.TDouble -> Set.empty
    Core.TUnit -> Set.empty
    Core.TArray a -> freeTVarsCore a
    Core.TMaybe a -> freeTVarsCore a
    Core.TMap a b -> freeTVarsCore a `Set.union` freeTVarsCore b
    Core.TBuilder a -> freeTVarsCore a
    Core.TIterator a -> freeTVarsCore a
    Core.TUser ut -> Set.unions (map freeTVarsCore (Core.utTyArgs ut))
    Core.TParam x -> Set.singleton x
-}

data Type tc =
    Type (Loc tc) [Type tc] [Integer]
  | TVar (Loc Name)

freeTVars :: Type tc -> Set Name
freeTVars ft =
  case ft of
    Type _ fts _ -> Set.unions (map freeTVars fts)
    TVar x -> Set.singleton (locThing x)

data BasicExporterType a b = Type a :-> Type b

data ExporterType a b = Forall {
  etDDLTypeVars     :: [Name],
  etForeignTypeVars :: [Name],
  etExporterParams  :: [BasicExporterType a b],
  etType            :: BasicExporterType a b
}

instance PP tc => PP (Type tc) where
  pp ft = 
    case ft of
      TVar x -> "?" <.> pp x
      Type f xs ns -> pp f <.> args
        where
        args =
          case xs of
            [] -> mempty
            _  -> "<" <.> commaSep (map pp xs ++ map pp ns) <.> ">"

instance (PP a, PP b) => PP (BasicExporterType a b) where
  pp (x :-> y) = pp x <+> "=>" <+> pp y
