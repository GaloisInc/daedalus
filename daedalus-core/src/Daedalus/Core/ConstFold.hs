{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- Constant folding/propagation

module Daedalus.Core.ConstFold (constFold) where

import GHC.Float(float2Double)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')

import Daedalus.Value hiding (Label)
import Daedalus.Panic
import Daedalus.Rec
import MonadLib hiding (Label)
import qualified Data.Vector as V


import RTS.Input (inputBytes)
import Daedalus.Core
import Daedalus.Core.TypeCheck (getUserType)
import Daedalus.Core.Semantics.Env
import Daedalus.Core.Semantics.Decl (evalModuleEmptyEnv)
import qualified Daedalus.Core.Semantics.Expr as S
import qualified Data.ByteString.Char8 as BS8
import Daedalus.Core.Type (typeOf)
import Daedalus.PP (showPP)
import Data.Maybe (isJust)

type TEnv = Map TName TDecl
data ConstFoldEnv =
  ConstFoldEnv { partialEnv :: Env -- ^ contains only constant vars
               , typeEnv    :: TEnv
               }

type ConstFoldM m a = ReaderT ConstFoldEnv m a

bindVarIn :: Monad m => Name -> Value -> ConstFoldM m a -> ConstFoldM m a
bindVarIn n v m = do
  e' <- updEnv <$> ask
  local e' m
  where
    updEnv e = e { partialEnv = defLocal n v (partialEnv e) }

initEnv :: Module -> ConstFoldEnv
initEnv md =
  ConstFoldEnv { partialEnv = evalModuleEmptyEnv md
               , typeEnv    = tenv
               }
  where
    tenv = Map.fromList [ (tName td, td) | td <- forgetRecs (mTypes md) ]

-- -----------------------------------------------------------------------------
-- Top level function

constFold :: Monad m => Module -> m Module
constFold m = runReaderT (initEnv m) (constFoldM m)

constFoldM :: Monad m => Module -> ConstFoldM m Module
constFoldM m = do
  ffs  <- mapM (traverse constFoldE) (mFFuns m)
  bfs  <- mapM (traverse constFoldB) (mBFuns m)
  gfs  <- mapM (traverse constFoldG) (mGFuns m)
  pure $ m { mFFuns = ffs, mBFuns = bfs, mGFuns = gfs }

constFoldE :: Monad m => Expr -> ConstFoldM m Expr
-- Let is treated specially as it may need to bind.
constFoldE (PureLet x l r) = do
  l' <- constFoldE l
  case exprToValue l' of
    Nothing -> PureLet x l' <$> constFoldE r
    Just v  -> bindVarIn x v (constFoldE r)
constFoldE e = do
  e' <- childrenE constFoldE e
  env <- ask

  -- Partial, uses laziness 
  let folded = valueToExpr (typeEnv env) (typeOf e') (S.eval e' (partialEnv env))
  pure $ case e' of
    -- Special cases
    Var n | Map.member n (vEnv (partialEnv env)) -> folded
    Struct _ut flds | all (exprIsConst . snd) flds -> folded
    ECase c@(Case x _)
      | Map.member x (vEnv (partialEnv env)) ->
          S.evalCase const (panic "Missing case" [showPP e]) c (partialEnv env)

    Ap0 {} -> folded
    Ap1 _ e1 | exprIsConst e1 -> folded
    Ap2 _ e1 e2 | exprIsConst e1, exprIsConst e2 -> folded
    Ap3 _ e1 e2 e3 | exprIsConst e1, exprIsConst e2, exprIsConst e3 -> folded
    ApN _ es | all exprIsConst es -> folded

    -- Cannot reduce further
    _ -> e'

-- We don't really reduce here, just fold constant exprs
constFoldB :: Monad m => ByteSet -> ConstFoldM m ByteSet
constFoldB = ebChildrenB constFoldE constFoldB

constFoldG :: Monad m => Grammar -> ConstFoldM m Grammar
constFoldG (Do x l r) = do
  l' <- constFoldG l
  case l' of
    Pure e | Just v <- exprToValue e -> bindVarIn x v (constFoldG r)
    _ -> Do x l' <$> constFoldG r

constFoldG (Let x e r) = do
  e' <- constFoldE e
  case exprToValue e' of
    Just v -> bindVarIn x v (constFoldG r)
    _ -> Let x e' <$> constFoldG r

constFoldG g = do
  g' <- gebChildrenG constFoldG constFoldE constFoldB g
  env <- ask
  pure $ case g' of
    GCase c@(Case x _)
      | Map.member x (vEnv (partialEnv env)) ->
          S.evalCase const (sysErr (typeOf g') "Missing case alternative") c (partialEnv env)
    _ -> g'

exprIsConst :: Expr -> Bool
exprIsConst = isJust . exprToValue

-- Copied from DDL2Core

sysErr :: Type -> String -> Grammar
sysErr t msg = Fail ErrorFromSystem t (Just (byteArrayL (BS8.pack msg)))

-- -----------------------------------------------------------------------------
-- Expr <-> Value

-- Assumes the expression has no free variables which have a constant value.
exprToValue :: Expr -> Maybe Value
exprToValue (Ap0 op0) = Just $ S.partial (S.evalOp0 op0)
exprToValue _         = Nothing

userTypeToUnion :: TEnv -> UserType -> [(Label, Type)]
userTypeToUnion tenv ut =
  let tdef = (let ?tenv = tenv in getUserType ut)
  in case tdef of
    Right (TUnion flds) -> flds
    Right _ -> panic "Type is not a union" [showPP ut]
    Left err -> panic "Couldn't figure out user type" [showPP ut, show err]

userTypeToStruct :: TEnv -> UserType -> [(Label, Type)]
userTypeToStruct tenv ut =
  let tdef = (let ?tenv = tenv in getUserType ut)
  in case tdef of
    Right (TStruct flds) -> flds
    Right _ -> panic "Type is not a union" [showPP ut]
    Left err -> panic "Couldn't figure out user type" [showPP ut, show err]

-- Module is required to lookup the type of union elements
valueToExpr :: TEnv -> Type -> Value -> Expr
valueToExpr tenv ty v =
  case v of
    VUInt _n i -> Ap0 $ IntL i ty
    VSInt _n i -> Ap0 $ IntL i ty
    VInteger i -> Ap0 $ IntL i TInteger  -- check ty?
    VBool    b -> Ap0 $ BoolL b -- check ty?
    VFloat   f -> Ap0 $ FloatL (float2Double f) TFloat
    VDouble  d -> Ap0 $ FloatL d TDouble
    VUnionElem l e
      | TUser ut <- ty
      , Just ty' <- lookup l (userTypeToUnion tenv ut) ->
        inUnion ut l (valueToExpr tenv ty' e)
    VUnionElem l e -> panic "Malfomed union label/value/type" [showPP l, showPP e, showPP ty]

    -- We have no guarantee that the label order is the same ...    
    VStruct flds
      | TUser ut <- ty
      , Just flds' <- sequence [ (,) l . go ty' <$> lookup l flds
                               | (l, ty') <- userTypeToStruct tenv ut
                               ] ->
        Struct ut flds'
      | ty == TUnit -> unit
    VStruct {} -> panic "Malfomed struct value/type" [showPP v, showPP ty]
    VArray vs
      | ty == TArray (TUInt (TSize 8)) -> byteArrayL (valueToByteString v)
      | TArray ty' <- ty -> arrayL ty' (map (go ty') (V.toList vs))
    VArray {}  -> panic "Malfomed array value/type" [showPP v, showPP ty]
    VMaybe m_v
      | TMaybe ty' <- ty -> case m_v of
          Nothing -> nothing ty'
          Just v'  -> just $ go ty' v'
    VMaybe {} -> panic "Malfomed maybe value/type" [showPP v, showPP ty]
    VMap m | TMap kt vt <- ty -> fromMapEls kt vt (Map.toList m)
    VMap {} -> panic "Malfomed map value/type" [showPP v, showPP ty]
    -- Thes are a bit hacky, and probably shouldn't actually occur,
    -- but there are included for completeness.
    VStream inp -> arrayStream (byteArrayL (inputBytes inp)) (byteArrayL "array")
    VBuilder vs | TBuilder ty' <- ty ->
      let es = map (go ty') vs
          arr = arrayL ty' (reverse es)
      in emitArray (newBuilder ty') arr

    VBuilder {} -> panic "Malfomed builder value/type" [showPP v, showPP ty]
    VIterator vs
      | TIterator (TMap kt vt) <- ty -> newIterator (fromMapEls kt vt vs)
      -- This doesn't construct a bytearray (always an array)
      -- FIXME: assumes we start at 0 and are in order
      | TIterator (TArray ty') <- ty   -> newIterator (arrayL ty' $ map (go ty' . snd) vs)
    VIterator {} -> panic "Malfomed iterator value/type" [showPP v, showPP ty]
  where
    go = valueToExpr tenv
    fromMapEls kt vt = foldl' (\m (k, v') -> mapInsert m (go kt k) (go vt v')) (mapEmpty kt vt)

