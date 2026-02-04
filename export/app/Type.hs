module Type where

import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set
import AlexTools(SourceRange)
import Daedalus.PP
import Daedalus.Core qualified as Core
import Name

coreTypeToType :: SourceRange -> Map Core.TParam (Type DDLTCon) -> Map Core.TParam Integer -> Core.Type -> Type DDLTCon
coreTypeToType rng vargs szargs ty =
  let go = coreTypeToType rng vargs szargs
      goSz = coreSizeToInt szargs
      con x = Type (Loc { locThing = x, locRange = rng })
  in
  case ty of
    Core.TStream -> con TStream [] []
    Core.TUInt sz -> con TUInt [] [goSz sz]
    Core.TSInt sz -> con TSInt [] [goSz sz]
    Core.TInteger -> con TInteger [] []
    Core.TBool -> con TBool [] []
    Core.TFloat -> con TFloat [] []
    Core.TDouble -> con TDouble [] []
    Core.TUnit -> con TUnit [] []
    Core.TArray a -> con TArray [go a] []
    Core.TMaybe a -> con TMaybe [go a] []
    Core.TMap a b -> con TMap [go a, go b] []
    Core.TBuilder a -> con TBuilder [go a] []
    Core.TIterator a -> con TIterator [go a] []
    Core.TUser ut -> con (TUser (Core.utName ut))
                          (map go (Core.utTyArgs ut))
                          (map goSz (Core.utNumArgs ut))
    Core.TParam a ->
      case Map.lookup a vargs of
        Just t -> t
        Nothing -> error "[BUG] `coreTypeToType`"

coreSizeToInt :: Map Core.TParam Integer -> Core.SizeType -> Integer
coreSizeToInt args sz =
  case sz of
    Core.TSize n -> n
    Core.TSizeParam a ->
      case Map.lookup a args of
        Just n -> n
        Nothing -> error "[BUG] coreSizeToInt"

data Type tc =
    Type (Loc tc) [Type tc] [Integer]
  | TVar (Loc Name)

instance Eq tc => Eq (Type tc) where
  x == y =
    case (x,y) of
      (Type f xs ys, Type g as bs) -> locThing f == locThing g && ys == bs && xs == as
      (TVar a, TVar b) -> locThing a == locThing b
      _ -> False

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

class PP tc => PPTyCon tc where
  ppTyCon :: tc -> [Type tc] -> [Integer] -> Doc
  wrapTC  :: tc -> Bool -> Doc -> Doc

instance PPTyCon DDLTCon where
  ppTyCon tc vargs szargs =
    case (tc,vargs,szargs) of
      (TArray,[a],[]) -> brackets (pp a)
      (TMap,[a,b],[]) -> brackets (pp a <+> ":" <+> pp b)
      _ -> pp tc <+> hsep (map (ppPrec 1) vargs) <+> hsep (map pp szargs)
  wrapTC _ = wrapIf

instance PPTyCon QName where
  ppTyCon tc xs ns =
    pp tc <.>
      case map pp xs ++ map pp ns of
        [] -> mempty
        ds  -> "<" <.> commaSep ds <.> ">"
  wrapTC _ = const id

instance PPTyCon tc => PP (Type tc) where
  ppPrec n ft = 
    case ft of
      TVar x -> pp x
      Type f [] [] -> ppTyCon (locThing f) [] []
      Type f xs ns ->
        wrapTC f' (n > 0) (ppTyCon f' xs ns)
        where f' = locThing f
        
instance (PPTyCon a, PPTyCon b) => PP (BasicExporterType a b) where
  pp (x :-> y) = pp x <+> "=>" <+> pp y
