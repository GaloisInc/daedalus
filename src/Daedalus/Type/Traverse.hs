{-# Language GADTs, ScopedTypeVariables #-}
{-# Language GADTs, TupleSections #-}
{-# Language RecordWildCards#-}
{-# Language RankNTypes #-}
module Daedalus.Type.Traverse where

import Control.Applicative
import Control.Monad.Identity
import Data.List.NonEmpty (NonEmpty)

import Daedalus.Type.AST


class TraverseTypes t where
  traverseTypes :: Applicative f => (Type -> f Type) -> t -> f t

instance TraverseTypes a => TraverseTypes (Maybe a) where
  traverseTypes f = traverse (traverseTypes f)

instance TraverseTypes a => TraverseTypes [a] where
  traverseTypes f = traverse (traverseTypes f)

instance TraverseTypes a => TraverseTypes (NonEmpty a) where
  traverseTypes f = traverse (traverseTypes f)


instance TraverseTypes (TCName k) where
  traverseTypes f n = (\t -> n { tcType = t }) <$> f (tcType n)

instance TraverseTypes e => TraverseTypes (Located e) where
  traverseTypes f e = (\x -> e { thingValue = x }) <$>
                      traverseTypes f (thingValue e)

instance TraverseTypes e => TraverseTypes (TCAnnot a e) where
  traverseTypes f e = (\x -> e { tcAnnotExpr = x }) <$>
                      traverseTypes f (tcAnnotExpr e)



instance TraverseTypes RuleType where
  traverseTypes f (xs :-> y) = (:->) <$> travArg xs <*> f y
    where
    traveIP (x,t) = (,) x <$> f t
    travArg (is,ts) = (,) <$> traverse traveIP is <*> traverse f ts

instance TraverseTypes (TC a k) where
  traverseTypes f (TC m) = TC <$> traverseTypes f m


instance TraverseTypes (LoopFlav a) where
  traverseTypes f lt =
    case lt of
      Fold x s -> Fold <$> traverseTypes f x <*> traverseTypes f s
      LoopMap  -> pure LoopMap

instance TraverseTypes (Loop a k) where
  traverseTypes f lp =
    Loop <$> traverseTypes f (loopFlav lp)
         <*> traverseTypes f (loopKName lp)
         <*> traverseTypes f (loopElName lp)
         <*> traverseTypes f (loopCol lp)
         <*> traverseTypes f (loopBody lp)
         <*> f (loopType lp)

instance TraverseTypes (TCF a k) where
  traverseTypes f expr =
    case expr of
      TCPure e        -> TCPure <$> traverseTypes f e
      TCDo x e1 e2    -> TCDo   <$> traverseTypes f x
                                <*> traverseTypes f e1 <*> traverseTypes f e2

      TCLabel l e     -> TCLabel l <$> traverseTypes f e

      TCGetByte {}    -> pure expr
      TCMatch s e     -> TCMatch s <$> traverseTypes f e
      TCMatchBytes s e -> TCMatchBytes s <$> traverseTypes f e

      TCChoice c es t -> TCChoice c <$> traverseTypes f es <*> f t
      TCOptional c e  -> TCOptional c <$> traverseTypes f e
      TCMany s c b e   -> TCMany s c <$> traverseTypes f b <*> traverseTypes f e
      TCEnd           -> pure expr
      TCOffset        -> pure expr
      TCMapEmpty t    -> TCMapEmpty <$> f t
      TCMapInsert s k v m -> TCMapInsert s <$> traverseTypes f k
                                       <*> traverseTypes f v
                                       <*> traverseTypes f m
      TCMapLookup s k m -> TCMapLookup s <$> traverseTypes f k
                                         <*> traverseTypes f m

      TCArrayLength e   -> TCArrayLength <$> traverseTypes f e
      TCArrayIndex s e ix -> TCArrayIndex s <$> traverseTypes f e
                                            <*> traverseTypes f ix

      TCFor lp -> TCFor <$> traverseTypes f lp

      TCIf be te fe   -> TCIf <$> traverseTypes f be <*> traverseTypes f te
                              <*> traverseTypes f fe

      TCLiteral l t   -> TCLiteral l <$> f t
      TCNothing t     -> TCNothing <$> f t
      TCJust e        -> TCJust <$> traverseTypes f e
      TCUnit          -> pure expr
      TCStruct fs t   -> TCStruct <$> traverse doField fs <*> f t
        where doField (x,e) = (x,) <$> traverseTypes f e
      TCArray e t     -> TCArray  <$> traverseTypes f e <*> f t
      TCIn l e t      -> TCIn l   <$> traverseTypes f e <*> f t

      TCVar x         -> TCVar    <$> traverseTypes f x
      TCCall x ts es  -> TCCall   <$> traverseTypes f x
                                  <*> traverse f ts
                                  <*> traverseTypes f es
      TCTriOp op e1 e2 e3 t -> TCTriOp op <$> traverseTypes f e1
                                          <*> traverseTypes f e2
                                          <*> traverseTypes f e3
                                          <*> f t
      TCBinOp op e1 e2 t  -> TCBinOp op <$> traverseTypes f e1
                                        <*> traverseTypes f e2
                                        <*> f t
      TCUniOp op e      -> TCUniOp op <$> traverseTypes f e

      TCSelStruct e l t -> (`TCSelStruct` l) <$> traverseTypes f e <*> f t

      TCSetAny          -> pure expr
      TCSetSingle e     -> TCSetSingle <$> traverseTypes f e
      TCSetRange e1 e2  -> TCSetRange <$> traverseTypes f e1
                                      <*> traverseTypes f e2
      TCSetComplement e -> TCSetComplement <$> traverseTypes f e
      TCSetUnion e      -> TCSetUnion <$> traverseTypes f e
      TCSetDiff e1 e2   -> TCSetDiff <$> traverseTypes f e1
                                     <*> traverseTypes f e2
      TCSetOneOf _      -> pure expr

      TCCoerce t1 t2 e      -> TCCoerce <$> f t1 <*> f t2
                                        <*> traverseTypes f e

      TCCoerceCheck s t1 t2 e -> TCCoerceCheck s <$> f t1 <*> f t2
                                                 <*> traverseTypes f e

      TCCurrentStream   -> pure expr
      TCSetStream s     -> TCSetStream <$> traverseTypes f s
      TCStreamLen se n s   -> TCStreamLen se <$> traverseTypes f n
                                             <*> traverseTypes f s
      TCStreamOff se n s   -> TCStreamOff se <$> traverseTypes f n
                                             <*> traverseTypes f s
      TCErrorMode m p      -> TCErrorMode m <$> traverseTypes f p
      TCFail mbM t         -> TCFail <$> traverseTypes f mbM <*> f t
      TCCase e pats mdef ->
        TCCase <$> traverseTypes f e
               <*> traverseTypes f pats
               <*> traverseTypes f mdef

instance TraverseTypes (Arg a) where
  traverseTypes f arg =
    case arg of
      ValArg e -> ValArg <$> traverseTypes f e
      ClassArg e -> ClassArg <$> traverseTypes f e
      GrammarArg e -> GrammarArg <$> traverseTypes f e

instance TraverseTypes Param where
  traverseTypes f arg =
    case arg of
      ValParam e -> ValParam <$> traverseTypes f e
      ClassParam e -> ClassParam <$> traverseTypes f e
      GrammarParam e -> GrammarParam <$> traverseTypes f e




instance TraverseTypes e => TraverseTypes (ManyBounds e) where
  traverseTypes f bnds =
    case bnds of
      Exactly e -> Exactly <$> traverseTypes f e
      Between e1 e2 -> Between <$> traverseTypes f e1 <*> traverseTypes f e2

instance TraverseTypes Constraint where
  traverseTypes f constraint =
    case constraint of
      Numeric t         -> Numeric <$> f t
      FloatingType t    -> FloatingType <$> f t
      HasStruct t1 l t2 -> HasStruct <$> f t1 <*> pure l <*> f t2
      StructCon nm t fs -> StructCon nm <$> f t <*> traverse tF fs
        where tF (x,ft) = (x,) <$> traverse f ft
      UnionCon nm t c ft -> UnionCon nm <$> f t <*> pure c <*> traverse f ft
      HasUnion t1 l t2  -> HasUnion  <$> f t1 <*> pure l <*> f t2
      Coerce l t t'     -> Coerce l <$> f t <*> f t'
      Literal n t       -> Literal n <$> f t
      CAdd t1 t2 t3     -> CAdd <$> f t1 <*> f t2 <*> f t3
      IsNamed t         -> IsNamed <$> f t
      Traversable t     -> Traversable <$> f t
      Mappable t s      -> Mappable <$> f t <*> f s
      ColElType c e     -> ColElType  <$> f c <*> f e
      ColKeyType c e    -> ColKeyType <$> f c <*> f e

instance TraverseTypes (TCDecl a) where
  traverseTypes f TCDecl { .. } =
                      mk <$> traverseTypes f tcDeclCtrs
                         <*> traverseTypes f tcDeclParams
                         <*> traverseTypes f tcDeclDef

    where mk cs ps e = TCDecl { tcDeclParams = ps,
                                tcDeclCtrs = cs,
                                tcDeclDef = e, .. }

instance TraverseTypes (TCDeclDef a k) where
  traverseTypes f def =
    case def of
      ExternDecl t -> ExternDecl <$> f t
      Defined d    -> Defined <$> traverseTypes f d

instance TraverseTypes TCTyDecl where
  traverseTypes f TCTyDecl { .. } =
    mk <$> traverseTypes f tctyDef
    where
    mk d = TCTyDecl { tctyDef = d, .. }

instance TraverseTypes TCTyDef where
  traverseTypes (f :: Type -> f Type) def =
    case def of
      TCTyStruct mb fs -> TCTyStruct mb <$> traverse doField fs
      TCTyUnion  fs -> TCTyUnion  <$> traverse doField fs
    where
      doField :: (Label, (Type, a)) -> f (Label, (Type, a))
      doField (x,(t,m)) = (x,) <$> ( (,) <$> f t <*> pure m)

instance TraverseTypes a => TraverseTypes (Rec a) where
  traverseTypes f r =
    case r of
      NonRec d  -> NonRec <$> traverseTypes f d
      MutRec ds -> MutRec <$> traverseTypes f ds

instance TraverseTypes (TCAlt a k) where
  traverseTypes f (TCAlt ps e) =
    TCAlt <$> traverseTypes f ps <*> traverseTypes f e

instance TraverseTypes TCPat where
  traverseTypes f pat =
    case pat of
      TCConPat t l p ->
        (\t1 p1 -> TCConPat t1 l p1) <$> f t <*> traverseTypes f p
      TCNumPat t i   -> (`TCNumPat` i) <$> f t
      TCBoolPat b    -> pure (TCBoolPat b)
      TCJustPat p    -> TCJustPat <$> traverseTypes f p
      TCNothingPat t -> TCNothingPat <$> f t
      TCVarPat x     -> TCVarPat <$> traverseTypes f x
      TCWildPat t    -> TCWildPat <$> f t

collectTypes :: (Monoid m, TraverseTypes t) => (Type -> m) -> t -> m
collectTypes f xs = res
  where Const res = traverseTypes (\t -> Const (f t)) xs

mapTypes :: TraverseTypes t => (Type -> Type) -> t -> t
mapTypes f = runIdentity . traverseTypes (Identity . f)

-- -----------------------------------------------------------------------------
-- Traversal



traverseTCF :: forall a b f k. Applicative f
            =>  (forall k'. TC a k' -> f (TC b k'))
            -> TCF a k -> f (TCF b k)
traverseTCF f = go
  where
    go :: forall k'. TCF a k' -> f (TCF b k')
    go texpr =
      case texpr of
        TCPure e      -> TCPure <$> f e
        TCDo  x e1 e2 -> TCDo x <$> f e1 <*> f e2

        TCGetByte x    -> pure (TCGetByte x)
        TCMatch s b    -> TCMatch s <$> f b

        TCLabel l e    -> TCLabel l <$> f e

        TCMatchBytes s b -> TCMatchBytes s <$> f b

        TCChoice c es t -> TCChoice c <$> traverse f es <*> pure t
        TCOptional c e  -> TCOptional c <$> f e

        TCMany s c bnds e-> TCMany s c <$> traverse f bnds <*> f e
        TCEnd          -> pure TCEnd
        TCOffset       -> pure TCOffset

        TCMapEmpty x   -> pure (TCMapEmpty x)
        TCMapInsert s k v m -> TCMapInsert s <$> f k <*> f v <*> f m
        TCMapLookup s k m -> TCMapLookup s <$> f k <*> f m

        -- Arrays
        TCArrayLength e -> TCArrayLength <$> f e
        TCArrayIndex s e ix -> TCArrayIndex s <$> f e <*> f ix

        -- Coercions
        TCCoerceCheck s t t' e -> TCCoerceCheck s t t' <$> f e
        TCCoerce      t t' e -> TCCoerce t t' <$> f e

        -- Values
        TCLiteral l t -> pure (TCLiteral l t)
        TCNothing x   -> pure (TCNothing x)
        TCJust e      -> TCJust <$> f e
        TCStruct xs t -> TCStruct <$> traverse (traverse f) xs <*> pure t
        TCUnit        -> pure TCUnit
        TCArray xs t  -> TCArray <$> traverse f xs <*> pure t
        TCIn l e t    -> TCIn l <$> f e <*> pure t

        -- numeric
        TCTriOp op e1 e2 e3 t -> TCTriOp op <$> f e1 <*> f e2 <*> f e3
                                                              <*> pure t
        TCBinOp op e1 e2 t -> TCBinOp op <$> f e1 <*> f e2 <*> pure t
        TCUniOp op    e    -> TCUniOp op <$> f e

        -- Sets
        TCSetAny      -> pure TCSetAny
        TCSetSingle e -> TCSetSingle <$> f e
        TCSetComplement e -> TCSetComplement <$> f e
        TCSetRange e1 e2 -> TCSetRange <$> f e1 <*> f e2
        TCSetUnion es    -> TCSetUnion <$> traverse f es
        TCSetOneOf x    -> pure (TCSetOneOf x)
        TCSetDiff e1 e2 -> TCSetDiff <$> f e1 <*> f e2

        -- Eliminators
        TCFor lp ->
          mk <$> travFlav (loopFlav lp) <*> f (loopCol lp) <*> f (loopBody lp)
          where
          mk t i e = TCFor lp { loopFlav = t, loopCol = i, loopBody = e }
          travFlav ty =
            case ty of
              Fold x s -> Fold x <$> f s
              LoopMap  -> pure LoopMap

        TCSelStruct x n t  -> TCSelStruct  <$> f x <*> pure n <*> pure t

        TCIf be te fe      -> TCIf <$> f be <*> f te <*> f fe

        TCVar x           -> pure (TCVar x)
        TCCall n ts xs    -> TCCall n ts <$> traverse (traverseArg f) xs

        TCCurrentStream     -> pure TCCurrentStream
        TCSetStream x       -> TCSetStream    <$> f x
        TCStreamLen se n s  -> TCStreamLen se <$> f n <*> f s
        TCStreamOff se n s  -> TCStreamOff se <$> f n <*> f s

        TCErrorMode m p     -> TCErrorMode m <$> f p

        TCFail mbM t        -> TCFail <$> traverse f mbM <*> pure t
        TCCase e pats mdef ->
          TCCase <$> f e
                 <*> traverse (traverseAlt f) pats
                 <*> traverse f mdef

traverseAlt :: Applicative f => (TC a k -> f (TC b k)) ->
                                TCAlt a k -> f (TCAlt b k)
traverseAlt f (TCAlt pats e) = TCAlt pats <$> f e

traverseArg :: Applicative f => (forall s. TC a s -> f (TC b s)) ->
                                Arg a -> f (Arg b)
traverseArg f arg =
  case arg of
    ValArg e     -> ValArg <$> f e
    GrammarArg e -> GrammarArg <$> f e
    ClassArg e   -> ClassArg <$> f e

-- -----------------------------------------------------------------------------
-- Tie the knot


traverseTC :: forall a b f k.
  Applicative f =>
  (a -> f b) ->
  (forall k'. TC a k' -> f (TC b k')) ->
  TC a k -> f (TC b k)
traverseTC ann f (TC m) =
  mk <$> ann (tcAnnot m) <*> traverseTCF f (tcAnnotExpr m)
  where mk a e = TC TCAnnot { tcAnnot = a, tcAnnotExpr = e }

foldMapTCF :: forall a m k. Monoid m
            =>  (forall k'. TC a k' -> m) -> TCF a k -> m
foldMapTCF f = getConst . traverseTCF (Const . f)


foldMapTC :: forall a m k. Monoid m
            =>  (forall k'. TC a k' -> m) -> TC a k -> m
foldMapTC f = getConst . traverseTC (const mempty) (Const . f)

mapTC :: (a -> b) -> (forall s. TC a s -> TC b s) -> TC a k -> TC b k
mapTC ann f (TC m) = TC TCAnnot { tcAnnot = ann (tcAnnot m)
                                , tcAnnotExpr = mapTCF f (tcAnnotExpr m) }

mapAnnotTC :: (a -> b) -> TC a k -> TC b k
mapAnnotTC f = mapTC f (mapAnnotTC f)


mapTCF :: (forall s. TC a s -> TC b s) -> TCF a k -> TCF b k
mapTCF f = runIdentity . traverseTCF (pure . f)

mapArg :: (a -> b) -> (forall s. TC a s -> TC b s) -> Arg a -> Arg b
mapArg ann f = runIdentity . traverseArg (pure . mapTC ann f)
