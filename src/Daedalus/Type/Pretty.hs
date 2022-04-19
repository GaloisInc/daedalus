{-# Language OverloadedStrings #-}
{-# Language ImplicitParams #-}
module Daedalus.Type.Pretty
  ( ppTypes
  , ppNamedRuleType
  , ppRuleType
  , ppTypeInContext
  ) where

import Data.Map(Map)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Rec
import Daedalus.AST(nameScopeAsModScope)
import Daedalus.Type.AST



ppTypes :: TCModule a -> Doc
ppTypes m = vcat $ map ppD $ forgetRecs $ tcModuleDecls m


-- | Pick a consistent set of nice names for type variables
tyVarNames :: Poly a -> Map TVar Doc
tyVarNames (Poly tys _ _) = Map.fromList (zip tys names)
  where
  names = [ if v == 0 then char x else char x <> int v
          | v <- [ 0 .. ], x <- [ 'a' .. 'z' ] ]

ppNamedRuleType :: Name -> Poly RuleType -> Doc
ppNamedRuleType nm r = ("def" <+> ppNM nm <.> colon) $$ nest 2 (ppRuleType r)

ppRuleType :: Poly RuleType -> Doc
ppRuleType r@(Poly _ ctrs ((implParams,explParams) :-> res)) =
  let ?tpMap =  tyVarNames r
  in vcat [ "for any type" <+> hsep (Map.elems ?tpMap) <.> colon
            | not (Map.null ?tpMap) ]
    $$ nest 2 (vcat (map ppCtr ctrs))
    $$ vcat [ "implict parameter:" <+> pp x <+> ":" <+> ppTy 0 p
                                                  | (x,p) <- implParams ]
    $$ vcat [ "parameter:" <+> ppTy 0 p | p <- explParams ]
    $$ "defines:" <+> ppTy 0 res
    $$ " "

ppTypeInContext :: Poly a -> Type -> Doc
ppTypeInContext r t =
  let ?tpMap = tyVarNames r
  in ppTy 0 t

ppD :: TCDecl a -> Doc
ppD d = ppNamedRuleType (tcDeclName d) (declTypeOf d)

ppCtr :: (?tpMap :: Map TVar Doc) => Constraint -> Doc
ppCtr ctr =
  case ctr of
    Integral t -> ppBackTy t <+> "is an integral type"
    Arith t    -> ppBackTy t <+> "supports arithmetic"
    FloatingType t -> ppBackTy t <+> "is a floating point type"
    HasStruct ts l tf ->
      ppBackTy ts <+> "has a field" <+> pp l <+> ":" <+> ppTy 0 tf
    HasUnion tu l tf ->
      ppBackTy tu <+> "has a variant" <+> pp l <+> ":" <+> ppTy 0 tf

    StructCon {} -> empty -- shouldn't appear in schemas
    UnionCon {} -> empty -- shouldn't appear in schemas

    Coerce los t1 t2 ->
      ppBackTy t1 <+> "is coercible to" <+> ppBackTy t2 <+>
        case los of
          Lossy -> "(lossy)"
          NotLossy -> empty
          Dynamic -> "(dynamic check)"


    Literal n t -> ppBackTy t <+> "contains the number" <+> pp n
    CAdd t1 t2 t3 -> ppTy 1 t1 <+> "+" <+> ppTy 1 t2 <+> "=" <+> ppTy 1 t3
    Traversable t -> ppBackTy t <+> "supports iteration"
    Mappable t1 t2 -> "mapping over" <+> ppBackTy t1 <+>
                      "results in" <+> ppBackTy t2
    ColElType t1 t2 -> ppBackTy t1 <+> "is a collection of" <+> ppBackTy t2
    ColKeyType t1 t2 ->
      ppBackTy t1 <+> "is a collection with index" <+> ppBackTy t2
    IsNamed {} -> empty -- shouldn't appear in schemas

ppBackTy :: (?tpMap :: Map TVar Doc) => Type -> Doc
ppBackTy t = "`" <> ppTy 0 t <> "`"

ppTy :: (?tpMap :: Map TVar Doc) => Int -> Type -> Doc
ppTy n t =
  case t of
    Type tty ->
      case tty of
        TGrammar a -> "parser of" <+> ppTy n a
        TFun a b   -> wrap 1 (ppTy 1 a <+> "=>" <+> ppTy 0 b)
        TStream    -> "stream"
        TByteClass -> "byte class"
        TNum i     -> integer i
        TUInt i    -> wrap 2 ("uint" <+> ppTy 1 i)
        TSInt i    -> wrap 2 ("sint" <+> ppTy 1 i)
        TInteger   -> "int"
        TBool      -> "bool"
        TFloat     -> "float"
        TDouble    -> "double"
        TUnit      -> "{}"
        TArray a   -> "[" <+> ppTy 0 a <+> "]"
        TMaybe a   -> wrap 2 ("maybe" <+> ppTy 1 a)
        TMap a b   -> "[" <+> ppTy 1 a <+> "->" <+> ppTy 0 b <+> "]"
        TBuilder a -> wrap 2 ("builder" <+> ppTy 1 a)

    TCon f []      -> ppTC f
    TCon f ts      -> wrap 2 (ppTC f <+> hsep (map (ppTy 2) ts))
    TVar x         -> ?tpMap Map.! x

  where wrap p x = if n < p then x else parens x


ppTC :: TCTyName -> Doc
ppTC x = case x of
           TCTy a -> ppNM a
           TCTyAnon a i -> ppNM a <.> int i

ppNM :: Name -> Doc
ppNM = pp . snd . nameScopeAsModScope

