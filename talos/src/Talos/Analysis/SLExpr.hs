{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Talos.Analysis.SLExpr where

-- This module is for sliced expressions, essentially Expr + holes
import           Control.DeepSeq                 (NFData)
import qualified Data.Set                        as Set
import           GHC.Generics                    (Generic)


import           Daedalus.Core                   (Case, Expr (..), Label, Name,
                                                  Op0, Op1, Op2, Op3, OpN (..),
                                                  PPHow (..), UserType, ppOp2,
                                                  ppOp3, ppTApp)
import           Daedalus.Core.Basics            (Type)
import           Daedalus.Core.Free              (FreeVars (..))
import           Daedalus.Core.TraverseUserTypes (TraverseUserTypes (..))
import           Daedalus.PP
import           Daedalus.Panic

import Talos.Analysis.Eqv
import Talos.Analysis.Merge

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
  | SApN OpN [SLExpr]
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
    SApN op es       -> ApN op (map go es)
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
    Ap0 op          -> SAp0 op
    Ap1 op e        -> SAp1 op (go e)
    Ap2 op e1 e2    -> SAp2 op (go e1) (go e2)
    Ap3 op e1 e2 e3 -> SAp3 op (go e1) (go e2) (go e3)
    ApN op es       -> SApN op (map go es)
  where
    go = exprToSLExpr
  

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
      SApN opN es       -> SApN <$> traverseUserTypes f opN
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
      SApN _ es        -> freeVars es

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
      SApN op es ->
        let fs = freeFVars es
        in case op of
            CallF f  -> Set.insert f fs
            ArrayL _ -> fs

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

      SECase c -> pp c

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

      SApN op es ->
        case op of
          ArrayL t ->
            case es of
              [] -> ppTApp n "[]" [t]
              _  -> brackets (commaSep (map pp es))
          CallF f -> pp f <.> parens (commaSep (map pp es))

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
      (SApN _op es, SApN _op' es') -> es `eqv` es'
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
      (SApN op es, SApN _op es') -> SApN op (zipWith merge es es')
      _ -> panic "Mismatched terms in merge (SLExpr)" ["Left", showPP l, "Right", showPP r]
