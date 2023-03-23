{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Talos.Analysis.SLExpr (SLExpr(..), exprToSLExpr, slExprToExpr, slExprToExpr') where

-- This module is for sliced expressions, essentially Expr + holes
import           Control.DeepSeq (NFData)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import           GHC.Generics    (Generic)


import           Daedalus.Core                   (Case, Expr (..), Label, Name,
                                                  Op0, Op1, Op2, Op3, OpN (..),
                                                  PPHow (..), SizeType (TSize),
                                                  TDecl (tDef), TDef (..),
                                                  TName, Type (..),
                                                  UserType (utName), arrayL,
                                                  arrayStream, boolL,
                                                  byteArrayL, floatL, inUnion,
                                                  intL, mapEmpty, newBuilder,
                                                  newIterator, nothing, ppOp2,
                                                  ppOp3, ppTApp, unit)
import           Daedalus.Core.Free              (FreeVars (..))
import           Daedalus.Core.TraverseUserTypes (TraverseUserTypes (..))
import           Daedalus.PP
import           Daedalus.Panic                  (panic)

import           Talos.Analysis.Eqv              (Eqv (..))
import           Talos.Analysis.Merge            (Merge (..))

-- Expressions with a hole
data SLExpr =
    SVar Name
  | SPureLet Name SLExpr SLExpr
  | SStruct UserType [ (Label, SLExpr) ]
  | SECase (Case SLExpr)
  
  | SAp0 Op0
  | SAp1 Op1 SLExpr
  | SAp2 Op2 SLExpr SLExpr
  | SAp3 Op3 SLExpr SLExpr SLExpr
  -- No CallE
  | SArrayL Type [SLExpr]
  | EHole Type
  deriving (Generic, NFData)

slExprToExpr :: (Type -> Expr) -> SLExpr -> Expr
slExprToExpr dflt sl =
  case sl of
    SVar n -> Var n
    SPureLet n e e'  -> PureLet n (go e) (go e') 
    SStruct ut flds  -> Struct ut [ (l, go e) | (l, e) <- flds ]
    SECase c         -> ECase (go <$> c)
    SAp0 op          -> Ap0 op
    SAp1 op e        -> Ap1 op (go e)
    SAp2 op e1 e2    -> Ap2 op (go e1) (go e2)
    SAp3 op e1 e2 e3 -> Ap3 op (go e1) (go e2) (go e3)
    SArrayL ty es    -> ApN (ArrayL ty) (map go es)
    EHole ty         -> dflt ty
  where
    go = slExprToExpr dflt

exprToSLExpr :: Expr -> SLExpr
exprToSLExpr expr =
  case expr of
    Var n           -> SVar n
    PureLet n e e'  -> SPureLet n (go e) (go e') 
    Struct ut flds  -> SStruct ut [ (l, go e) | (l, e) <- flds ]
    ECase c         -> SECase (go <$> c)
    ELoop {}        -> panic "Saw an expression-level Loop" [showPP expr]
    Ap0 op          -> SAp0 op
    Ap1 op e        -> SAp1 op (go e)
    Ap2 op e1 e2    -> SAp2 op (go e1) (go e2)
    Ap3 op e1 e2 e3 -> SAp3 op (go e1) (go e2) (go e3)
    ApN (ArrayL ty) es -> SArrayL ty (map go es)
    ApN _op _es        -> panic "Saw an expression-level Call" [showPP expr]
  where
    go = exprToSLExpr

slExprToExpr' :: Map TName TDecl -> SLExpr -> Expr
slExprToExpr' tys = slExprToExpr (typeToInhabitant tys)

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
      TFloat       -> floatL 0 ty
      TDouble      -> floatL 0 ty
    goUT ut
      | Just decl <- Map.lookup (utName ut) tdecls =
          case tDef decl of
            TStruct fs -> Struct ut [ (l, go ty) | (l, ty) <- fs ]
            TUnion  ((l, ty) : _) -> inUnion ut l (go ty)
            TUnion  _   -> panic "Empty union" [showPP ut]
            TBitdata {} -> panic "Bitdata not yet supported" [showPP ut]
      | otherwise = panic "Unknown user type " [showPP ut]
  

--------------------------------------------------------------------------------
-- Instances

instance TraverseUserTypes SLExpr where
  traverseUserTypes f e =
    case e of
      EHole ty -> EHole <$> traverseUserTypes f ty
      SVar n  -> SVar <$> traverseUserTypes f n
      SPureLet x e' e'' -> SPureLet <$> traverseUserTypes f x
                                    <*> traverseUserTypes f e'
                                    <*> traverseUserTypes f e''
      SStruct ut ls    -> SStruct  <$> traverseUserTypes f ut
                                   <*> traverse (\(l, e') -> (,) l
                                       <$> traverseUserTypes f e') ls
      SECase c          -> SECase   <$> traverseUserTypes f c
      SAp0 op0          -> SAp0 <$> traverseUserTypes f op0
      SAp1 op1 e'       -> SAp1 <$> traverseUserTypes f op1
                                <*> traverseUserTypes f e'
      SAp2 op2 e' e''   -> SAp2 op2 <$> traverseUserTypes f e'
                                    <*> traverseUserTypes f e''
      SAp3 op3 e1 e2 e3 -> SAp3 op3 <$> traverseUserTypes f e1
                                    <*> traverseUserTypes f e2
                                    <*> traverseUserTypes f e3
      SArrayL ty es     -> SArrayL <$> traverseUserTypes f ty
                                   <*> traverseUserTypes f es

instance FreeVars SLExpr where
  freeVars expr =
    case expr of
      EHole {}         -> Set.empty
      SVar x           -> freeVars x
      SPureLet x e1 e2 -> freeVars e1 `Set.union` Set.delete x (freeVars e2)
      SStruct _ fs     -> Set.unions [ freeVars e | (_,e) <- fs ]
      SECase e         -> freeVars e
      SAp0 _           -> Set.empty
      SAp1 _ e         -> freeVars e
      SAp2 _ e1 e2     -> freeVars [e1,e2]
      SAp3 _ e1 e2 e3  -> freeVars [e1,e2,e3]
      SArrayL _ es     -> freeVars es

  freeFVars expr =
    case expr of
      EHole {}         -> Set.empty
      SVar x           -> freeFVars x
      SPureLet _ e1 e2 -> freeFVars [e1,e2]
      SStruct _ fs     -> Set.unions [ freeFVars e | (_,e) <- fs ]
      SECase e         -> freeFVars e
      SAp0 _           -> Set.empty
      SAp1 _ e         -> freeFVars e
      SAp2 _ e1 e2     -> freeFVars [e1,e2]
      SAp3 _ e1 e2 e3  -> freeFVars [e1,e2,e3]
      SArrayL _ es     -> freeFVars es

instance PP SLExpr where
  ppPrec n expr =
    case expr of
      EHole ty       -> wrapIf (n > 0) $ "hole" <> parens (ppPrec 0 ty)
      SVar x -> pp x
      SPureLet x e1 e2 ->
        wrapIf (n > 0)
          $ "let" <+> pp x <+> "=" <+> pp e1 <+> "in"
          $$ pp e2

      SStruct t fs -> ppPrec 1 t <+> braces (commaSep (map ppF fs))
        where ppF (l,e) = pp l <+> "=" <+> pp e

      SECase c  -> pp c

      SAp0 op   -> ppPrec n op

      SAp1 op e -> wrapIf (n > 0) (pp op <+> ppPrec 1 e)

      SAp2 op e1 e2 -> wrapIf (n > 0) $
        case ppOp2 op of
          (how,d) ->
            case how of
              PPPref   -> d <+> ppPrec 1 e1 <+> ppPrec 1 e2
              PPInf    -> ppPrec 1 e1 <+> d <+> ppPrec 1 e2
              PPCustom -> panic "PP Ap2" [show d]

      SAp3 op e1 e2 e3 -> wrapIf (n > 0) $
        case ppOp3 op of
          (PPPref,d) -> d <+> ppPrec 1 e1 <+> ppPrec 1 e2 <+> ppPrec 1 e3
          (_,d) -> panic "PP Ap3" [show d]

      SArrayL ty es ->
        case es of
          [] -> ppTApp n "[]" [ty]
          _  -> brackets (commaSep (map pp es))

instance Eqv SLExpr where
  eqv l r =
    case (l,r) of
      (EHole {}, EHole {})    -> True
      (EHole {}, _)           -> False
      (_, EHole {})           -> False
      (SVar {}, SVar {})      -> True
      (SPureLet _x e1 e2 , SPureLet _x' e1' e2') ->
        (e1, e2) `eqv` (e1', e2')
      (SStruct _ty fs, SStruct _ty' fs') -> map snd fs `eqv` map snd fs'
      (SECase e, SECase e') -> e `eqv` e'
      (SAp0 {}, SAp0 {})   -> True
      (SAp1 _op e, SAp1 _op' e') -> e `eqv` e'
      (SAp2 _op e1 e2, SAp2 _op' e1' e2') -> (e1, e2) `eqv` (e1', e2')
      (SAp3 _op e1 e2 e3, SAp3 _op' e1' e2' e3') ->
        (e1, e2, e3) `eqv` (e1', e2', e3')
      (SArrayL _ty es, SArrayL _ty' es') -> es `eqv` es'
      _ -> panic "Mismatched terms in eqv (SLExpr)" ["Left", showPP l, "Right", showPP r]      

instance Merge SLExpr where
  merge l r =
    case (l,r) of
      (EHole {}, _)    -> r
      (_, EHole {})    -> l
      (SVar {}, SVar {}) -> l
      (SPureLet x e1 e2 , SPureLet _x e1' e2') ->
        SPureLet x (merge e1 e1') (merge e2 e2')
      (SStruct ty fs, SStruct _ty fs') ->
        -- FIXME: we assume the orders match up here.
        SStruct ty [ (l', merge e e') | ((l', e), (_, e')) <- zip fs fs' ] 
      (SECase e, SECase e') -> SECase (merge e e')
      (SAp0 {}, SAp0 {})   -> l
      (SAp1 op e, SAp1 _op e') -> SAp1 op (merge e e')
      (SAp2 op e1 e2, SAp2 _op e1' e2') -> SAp2 op (merge e1 e1') (merge e2 e2')
      (SAp3 op e1 e2 e3, SAp3 _op e1' e2' e3') ->
        SAp3 op (merge e1 e1') (merge e2 e2') (merge e3 e3')
      (SArrayL ty es, SArrayL _ty es') -> SArrayL ty (zipWith merge es es')
      _ -> panic "Mismatched terms in merge (SLExpr)" ["Left", showPP l, "Right", showPP r]
