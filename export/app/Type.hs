module Type where

import Data.Set(Set)
import Data.Set qualified as Set
import Daedalus.PP
import Daedalus.Core qualified as Core
import Name

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

data ForeignType =
    ForeignType LName [ForeignType]
  | ForeignTVar LName

freeTVarsForeignType :: ForeignType -> Set Name
freeTVarsForeignType ft =
  case ft of
    ForeignType _ fts -> Set.unions (map freeTVarsForeignType fts)
    ForeignTVar x -> Set.singleton (nameName x)

data BasicExporterType = Core.Type :-> ForeignType

data ExporterType = Forall {
  etDDLTypeVars     :: [Core.TParam],
  etForeignTypeVars :: [Name],
  etExporterParams  :: [BasicExporterType],
  etType            :: BasicExporterType
}

instance PP ForeignType where
  pp ft = 
    case ft of
      ForeignTVar x -> "?" <.> pp x
      ForeignType f xs -> pp f <.> args
        where
        args =
          case xs of
            [] -> mempty
            _  -> "<" <.> commaSep (map pp xs) <.> ">"

instance PP BasicExporterType where
  pp (x :-> y) = pp x <+> "=>" <+> pp y
