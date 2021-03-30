{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Daedalus.Core.TraverseUserTypes where

import Control.Applicative

import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Daedalus.Core.ByteSet
import Daedalus.Core.Grammar
import Daedalus.Core.Decl

foldMapUserTypes :: (Monoid m, TraverseUserTypes t) => (UserType -> m) -> t -> m 
foldMapUserTypes f v = getConst (traverseUserTypes (Const . f) v)

-- Does not recurse into types
-- i.e. 
--   
class TraverseUserTypes a where
  traverseUserTypes :: Applicative f => (UserType -> f UserType) -> a -> f a
  
  default traverseUserTypes :: (Traversable t, TraverseUserTypes b, Applicative f, a ~ t b) =>
                           (UserType -> f UserType) -> a -> f a
  traverseUserTypes f = traverse (traverseUserTypes f)
    

instance TraverseUserTypes a => TraverseUserTypes [a] where {- default -}
instance (TraverseUserTypes a, TraverseUserTypes b) => TraverseUserTypes (a, b) where
  traverseUserTypes f (a, b) = (,) <$> traverseUserTypes f a <*> traverseUserTypes f b

instance TraverseUserTypes a => TraverseUserTypes (Maybe a) where {- default -}
instance TraverseUserTypes a => TraverseUserTypes (Case a) where {- default -}
instance TraverseUserTypes a => TraverseUserTypes (FunDef a) where {- default -}

instance TraverseUserTypes Type where
  traverseUserTypes f ty =
    case ty of
      TStream         -> pure ty
      TUInt {}        -> pure ty
      TSInt {}        -> pure ty
      TInteger        -> pure ty
      TBool           -> pure ty
      TUnit           -> pure ty
      TArray t        -> TArray <$> traverseUserTypes f t
      TMaybe t        -> TMaybe <$> traverseUserTypes f t
      TMap k v        -> TMap <$> traverseUserTypes f k <*> traverseUserTypes f v
      TBuilder t      -> TBuilder <$> traverseUserTypes f t
      TIterator t     -> TIterator <$> traverseUserTypes f t
      TUser ut        -> TUser <$> traverseUserTypes f ut
      TParam {}       -> pure ty

instance TraverseUserTypes a => TraverseUserTypes (Fun a) where
  traverseUserTypes f fn =
    Fun <$> traverseUserTypes f (fName fn) <*> traverseUserTypes f (fParams fn) <*> traverseUserTypes f (fDef fn)
  
instance TraverseUserTypes Name where
  traverseUserTypes f n =
    (\n' -> n { nameType = n' }) <$> traverseUserTypes f (nameType n)

instance TraverseUserTypes FName where
  traverseUserTypes f n = do
    (\n' -> n { fnameType = n' }) <$> traverseUserTypes f (fnameType n)

instance TraverseUserTypes Module where
  traverseUserTypes f m =
    (\ffs bfs gfs tys -> m { mFFuns = ffs, mBFuns = bfs, mGFuns = gfs, mTypes = tys })
    <$> traverse (traverseUserTypes f) (mFFuns m)
    <*> traverse (traverseUserTypes f) (mBFuns m)
    <*> traverse (traverseUserTypes f) (mGFuns m)
    <*> traverse (traverse (traverseUserTypes f)) (mTypes m)
  
instance TraverseUserTypes Match where
  traverseUserTypes f m =
    case m of
      MatchByte e  -> MatchByte  <$> traverseUserTypes f e
      MatchBytes e -> MatchBytes <$> traverseUserTypes f e
      MatchEnd     -> pure m
      
instance TraverseUserTypes Grammar where
  traverseUserTypes f gram =
    case gram of -- FIXME: use childrenG
      Pure e -> Pure <$> traverseUserTypes f e
      GetStream -> pure gram
      SetStream e -> SetStream <$> traverseUserTypes f e
      Match s m -> Match s <$> traverseUserTypes f m
      Fail es ty m_e -> Fail es <$> traverseUserTypes f ty <*> traverseUserTypes f m_e
      Do_ l r -> Do_ <$> traverseUserTypes f l <*> traverseUserTypes f r
      Do x l r -> Do <$> traverseUserTypes f x <*> traverseUserTypes f l <*> traverseUserTypes f r
      Let x l r -> Let <$> traverseUserTypes f x <*> traverseUserTypes f l <*> traverseUserTypes f r

      OrBiased l r -> OrBiased <$> traverseUserTypes f l <*> traverseUserTypes f r
      OrUnbiased l r -> OrUnbiased <$> traverseUserTypes f l <*> traverseUserTypes f r
      Call fn args -> Call <$> traverseUserTypes f fn <*> traverseUserTypes f args
      Annot a g'  -> Annot a <$> traverseUserTypes f g'
      GCase c     -> GCase <$> traverseUserTypes f c

instance TraverseUserTypes ByteSet where
  traverseUserTypes f bs =
    case bs of
      SetAny        -> pure bs
      SetSingle e   -> SetSingle <$> traverseUserTypes f e
      SetRange e e' -> SetRange <$> traverseUserTypes f e <*> traverseUserTypes f e'
      SetComplement bs' -> SetComplement <$> traverseUserTypes f bs'
      SetUnion bs' bs'' -> SetUnion <$> traverseUserTypes f bs' <*> traverseUserTypes f bs''
      SetIntersection bs' bs'' -> SetIntersection <$> traverseUserTypes f bs' <*> traverseUserTypes f bs''
      SetLet x e bs'           -> SetLet <$> traverseUserTypes f x <*> traverseUserTypes f e <*> traverseUserTypes f bs'
      SetCall fn es            -> SetCall <$> traverseUserTypes f fn <*> traverseUserTypes f es
      SetCase c                -> SetCase <$> traverseUserTypes f c

instance TraverseUserTypes Expr where
  traverseUserTypes f e =
    case e of
      Var n -> Var <$> traverseUserTypes f n
      PureLet x e' e'' -> PureLet <$> traverseUserTypes f x <*> traverseUserTypes f e' <*> traverseUserTypes f e''
      Struct ut ls     -> Struct  <$> traverseUserTypes f ut <*> traverse (\(l, e') -> (,) l <$> traverseUserTypes f e') ls
      ECase c          -> ECase   <$> traverseUserTypes f c
     
      Ap0 op0          -> Ap0 <$> traverseUserTypes f op0
      Ap1 op1 e'       -> Ap1 <$> traverseUserTypes f op1 <*> traverseUserTypes f e'
      Ap2 op2 e' e''   -> Ap2 op2 <$> traverseUserTypes f e' <*> traverseUserTypes f e''
      Ap3 op3 e1 e2 e3 -> Ap3 op3 <$> traverseUserTypes f e1 <*> traverseUserTypes f e2 <*> traverseUserTypes f e3
      ApN opN es       -> ApN <$> traverseUserTypes f opN <*> traverseUserTypes f es

instance TraverseUserTypes Op0 where
  traverseUserTypes f op0 =
    case op0 of
      Unit      -> pure op0
      IntL n ty -> IntL n <$> traverseUserTypes f ty
      BoolL {}  -> pure op0
      ByteArrayL {} -> pure op0
      NewBuilder ty -> NewBuilder <$> traverseUserTypes f ty
      MapEmpty dTy rTy -> MapEmpty <$> traverseUserTypes f dTy <*> traverseUserTypes f rTy
      ENothing ty      -> ENothing <$> traverseUserTypes f ty

instance TraverseUserTypes Op1 where
  traverseUserTypes f op1 =
    case op1 of
      CoerceTo ty -> CoerceTo <$> traverseUserTypes f ty
      SelStruct ty l -> flip SelStruct l <$> traverseUserTypes f ty
      InUnion ut l   -> flip InUnion   l <$> traverseUserTypes f ut
      FromUnion ty l -> flip FromUnion l <$> traverseUserTypes f ty
      _ -> pure op1  

instance TraverseUserTypes OpN where
  traverseUserTypes f opN =
    case opN of
      ArrayL ty -> ArrayL <$> traverseUserTypes f ty
      CallF n   -> CallF  <$> traverseUserTypes f n
  
instance TraverseUserTypes TDecl where
  traverseUserTypes f td =
    (\tdef' -> td { tDef = tdef' }) <$> traverseUserTypes f (tDef td)

instance TraverseUserTypes UserType where
  traverseUserTypes f ut = f ut -- we don't dig into args
  
instance TraverseUserTypes TDef where
  traverseUserTypes f tdef =
    case tdef of
      TStruct fs -> TStruct <$> go fs
      TUnion  fs -> TUnion  <$> go fs
    where
      go = traverse (\(l, e') -> (,) l <$> traverseUserTypes f e')
