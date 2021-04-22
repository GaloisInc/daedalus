{-# Language OverloadedStrings, NamedFieldPuns, GADTs, RankNTypes
  , TupleSections
  , ScopedTypeVariables, FlexibleContexts #-}
module Daedalus.Type.Subst where

import Control.Monad.Reader

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)

import Data.Parameterized.Map (MapF)
import Data.Parameterized.Some
import qualified Data.Parameterized.Map as MapF

import Daedalus.Panic
import Daedalus.PP
import Daedalus.Type.AST
import Daedalus.Type.Free(tcBinds)
import Daedalus.Type.Traverse

--------------------------------------------------------------------------------
-- Type level substitution

ppSubst :: Map TVar Type -> Doc
ppSubst su = vcat [ pp x <+> ":=" <+> pp t | (x,t) <- Map.toList su ]

class ApSubst t where
  -- | Returns 'Nothing' if the substitution did not change the input.
  apSubstT' :: Map TVar Type -> t -> Maybe t

instance ApSubst Type where
  apSubstT' su ty =
    case ty of
      TVar x    -> Map.lookup x su
      TCon c ts -> tCon c <$> someJusts (apSubstT' su) ts
      Type tf ->
        case tf of
          TGrammar t -> tGrammar <$> apSubstT' su t
          TFun t1 t2 -> someJust2 tFun (apSubstT' su) (apSubstT' su) t1 t2
          TStream    -> Nothing
          TByteClass -> Nothing
          TNum {}    -> Nothing
          TUInt t    -> tUInt <$> apSubstT' su t
          TSInt t    -> tSInt <$> apSubstT' su t
          TInteger   -> Nothing
          TBool      -> Nothing
          TUnit      -> Nothing
          TArray t   -> tArray <$> apSubstT' su t
          TMaybe t   -> tMaybe <$> apSubstT' su t
          TMap kt vt -> do ~[k',t'] <- someJusts (apSubstT' su) [kt,vt]
                           pure (tMap k' t')

instance ApSubst TCTyDef where
  apSubstT' su td =
    case td of
      TCTyStruct fs -> TCTyStruct <$> someJusts apF fs
      TCTyUnion fs  -> TCTyUnion  <$> someJusts apF fs
    where
    apF (x,t) = (x,) <$> apSubstT' su t


instance ApSubst TCTyField where
  apSubstT' su tf = (\t' -> tf { tctyfType = t' }) <$> apSubstT' su (tctyfType tf)


instance ApSubst Constraint where
  apSubstT' su ctr =
    case ctr of
      Numeric t         -> Numeric <$> apSubstT' su t
      HasStruct t1 l t2 -> do ~[a,b] <- someJusts (apSubstT' su) [t1,t2]
                              pure (HasStruct a l b)
      TyDef ty nm t fs  -> someJust2 (TyDef ty nm)
                                      (apSubstT' su) (someJusts apF)
                                      t              fs
        where apF (x,ft) = (x,) <$> apSubstT' su ft
      HasUnion  t1 l t2 -> do ~[a,b] <- someJusts (apSubstT' su) [t1,t2]
                              pure (HasUnion a l b)
      Coerce l t1 t2    -> do ~[a,b] <- someJusts (apSubstT' su) [t1,t2]
                              pure (Coerce l a b)
      Literal i t       -> Literal i <$> apSubstT' su t
      CAdd t1 t2 t3     -> do ~[a,b,c] <- someJusts (apSubstT' su) [t1,t2,t3]
                              pure (CAdd a b c)
      IsNamed t          -> IsNamed <$> apSubstT' su t
      Traversable t      -> Traversable <$> apSubstT' su t
      Mappable t s       -> do ~[a,b] <- someJusts (apSubstT' su) [t,s]
                               pure (Mappable a b)
      ColElType c e      -> do ~[a,b] <- someJusts (apSubstT' su) [c,e]
                               pure (ColElType a b)
      ColKeyType c e     -> do ~[a,b] <- someJusts (apSubstT' su) [c,e]
                               pure (ColKeyType a b)

instance ApSubst a => ApSubst (Located a) where
  apSubstT' su l = (\v -> l { thingValue = v }) <$> apSubstT' su (thingValue l)

instance ApSubst a => ApSubst [a] where
  apSubstT' su = someJusts (apSubstT' su)


someJust2 :: (a -> b -> c) -> (a -> Maybe a) -> (b -> Maybe b) ->
             a -> b -> Maybe c
someJust2 mk f g a b =
  case (f a, g b) of
    (Nothing,Nothing) -> Nothing
    (Nothing,Just b') -> Just (mk a b')
    (Just a',Nothing) -> Just (mk a' b)
    (Just a',Just b') -> Just (mk a' b')

someJusts :: (a -> Maybe a) -> [a] -> Maybe [a]
someJusts f xs =
  case xs of
    [] -> Nothing
    a : as -> case f a of
                Just b  -> Just (b : [ fromMaybe z (f z) | z <- as ])
                Nothing -> case someJusts f as of
                             Nothing -> Nothing
                             Just bs -> Just (a : bs)




apSubstT :: ApSubst t => Map TVar Type -> t -> t
apSubstT su t = fromMaybe t (apSubstT' su t)




freeTCons :: Type -> Set TCTyName
freeTCons ty =
  case ty of
    TVar {}   -> Set.empty
    TCon c ts -> Set.insert c (foldMap freeTCons ts)
    Type tf   -> foldMap freeTCons tf

class FreeTVS t where
  freeTVS :: t -> Set TVar

instance FreeTVS Type where
  freeTVS ty =
    case ty of
      TVar x     -> Set.singleton x
      TCon _ ts  -> freeTVS ts
      Type t     -> freeTVS t

instance FreeTVS e => FreeTVS (ManyBounds e) where
  freeTVS e =
    case e of
      Exactly n -> freeTVS n
      Between m n -> freeTVS m `Set.union` freeTVS n

instance FreeTVS a => FreeTVS (Maybe a) where
  freeTVS mb =
    case mb of
      Nothing -> Set.empty
      Just e  -> freeTVS e

instance FreeTVS t => FreeTVS (TypeF t) where
  freeTVS ty =
    case ty of
      TGrammar t -> freeTVS t
      TFun t1 t2 -> freeTVS t1 <> freeTVS t2
      TStream    -> Set.empty
      TByteClass -> Set.empty
      TNum {}    -> Set.empty
      TUInt t    -> freeTVS t
      TSInt t    -> freeTVS t
      TInteger   -> Set.empty
      TBool      -> Set.empty
      TUnit      -> Set.empty
      TArray t   -> freeTVS t
      TMaybe t   -> freeTVS t
      TMap kt vt -> freeTVS kt `Set.union` freeTVS vt

instance FreeTVS a => FreeTVS [a] where
  freeTVS = Set.unions . map freeTVS

instance FreeTVS a => FreeTVS (Rec a) where
  freeTVS = freeTVS . recToList

instance FreeTVS a => FreeTVS (Poly a) where
  freeTVS (Poly as cs t) = (freeTVS cs `Set.union` freeTVS t)
                                  `Set.difference` Set.fromList as

instance FreeTVS RuleType where
  freeTVS = collectTypes freeTVS    -- assumes no nested poly thingValue

instance FreeTVS (TC a k) where
  freeTVS = collectTypes freeTVS   -- assumes no nested poly things.

instance FreeTVS (TCF a k) where
  freeTVS = collectTypes freeTVS   -- assumes no nested poly things.

instance FreeTVS (TCName k) where
  freeTVS = freeTVS . tcType

instance FreeTVS (TCDecl a) where
  freeTVS d@TCDecl { tcDeclDef } =
      Set.union (freeTVS (tcDeclParams d)) (freeTVS tcDeclDef)
              `Set.difference` Set.fromList (tcDeclTyParams d)

instance FreeTVS (TCDeclDef a k) where
  freeTVS def =
    case def of
      ExternDecl t -> freeTVS t
      Defined e    -> freeTVS e

instance FreeTVS TCTyDecl where
  freeTVS d = (freeTVS (tctyDef d) `Set.union` freeTVS (tctyBDWidth d))
              `Set.difference` Set.fromList (tctyParams d)

instance FreeTVS TCTyDef where
  freeTVS = collectTypes freeTVS

instance FreeTVS (Arg a) where
  freeTVS arg =
    case arg of
      ValArg e -> freeTVS e
      ClassArg e -> freeTVS e
      GrammarArg e -> freeTVS e

instance FreeTVS Param where
  freeTVS p =
    case p of
      ValParam x -> freeTVS x
      ClassParam x -> freeTVS x
      GrammarParam x -> freeTVS x

instance FreeTVS a => FreeTVS (Located a) where
  freeTVS = freeTVS . thingValue

instance FreeTVS Constraint where
  freeTVS c =
    case c of
      Numeric t         -> freeTVS t
      HasStruct t1 _ t2 -> freeTVS t1 <> freeTVS t2
      TyDef _ _ t fs    -> freeTVS t <> freeTVS (map snd fs)
      HasUnion  t1 _ t2 -> freeTVS t1 <> freeTVS t2
      Coerce _ t1 t2    -> Set.union (freeTVS t1) (freeTVS t2)
      Literal _ t       -> freeTVS t
      CAdd t1 t2 t3     -> Set.unions [ freeTVS t1, freeTVS t2, freeTVS t3 ]
      IsNamed t         -> freeTVS t
      Traversable t1    -> freeTVS t1
      Mappable t1 t2    -> Set.unions [ freeTVS t1, freeTVS t2 ]
      ColElType t1 t2   -> Set.unions [ freeTVS t1, freeTVS t2 ]
      ColKeyType t1 t2  -> Set.unions [ freeTVS t1, freeTVS t2 ]


--------------------------------------------------------------------------------
-- Term level substitution

-- FIXME: TCF here, not TC, as we replace a Var directly.  We could
-- fix this by special casing the TC case to Vars
type Subst a = MapF TCName (TCF a)

emptySubst :: Subst a
emptySubst = MapF.empty

forgetSubst :: TCName k -> Subst a -> Subst a
forgetSubst = MapF.delete

forgetSomeSubst :: Some TCName -> Subst a -> Subst a
forgetSomeSubst (Some x) = MapF.delete x

lookupSubst :: TCName k -> Subst a -> Maybe (TCF a k)
lookupSubst = MapF.lookup

addSubst :: TCName k -> TCF a k -> Subst a -> Subst a
addSubst = MapF.insert

-- | Combines the arguments, preferring the second argument..
mergeSubst :: Subst a -> Subst a -> Subst a
mergeSubst old new = MapF.union new old

apSubst' :: forall a m k. MonadReader (Subst a) m
         => (forall k'. TC a k' -> m (TC a k'))
         -> TCF a k -> m (TCF a k)
apSubst' go = go'
  where
    go' :: forall k'. TCF a k' -> m (TCF a k')
    go' texpr =
      case texpr of
        TCVar x -> doVar x texpr

        TCDo x e1 e2 ->
          TCDo x <$> go e1 <*> local (maybe id forgetSubst x) (go e2)

        TCCall f ts as | isLocalName (tcName f) -> do
          m_f' <- lookupSubst f <$> ask
          case m_f' of
            Nothing         -> TCCall f  ts <$> traverse (traverseArg go) as
            Just (TCVar f') -> TCCall f' ts <$> traverse (traverseArg go) as
            _               -> panic "Non-variable subst in Call" []

        TCFor lp ->
          mk <$> newFl
             <*> go (loopCol lp)
             <*> local newS (go (loopBody lp))
          where
          mk s i e = TCFor lp { loopFlav = s, loopCol = i, loopBody = e }

          newFl = case loopFlav lp of
                    Fold x s -> Fold x <$> go s
                    LoopMap  -> pure LoopMap

          newS s = foldr forgetSomeSubst s
                 (tcBinds (loopFlav lp, (loopKName lp, loopElName lp)))

        TCCase e pats mdef ->
          TCCase <$> go e <*> traverse doAlt pats <*> traverse go mdef
          where
            doAlt (TCAlt ps rhs) =
              TCAlt ps <$> foldr (local . forgetSubst) (go rhs) (patBinds (head ps))

        e  -> traverseTCF go e
      where
        doVar x def = fromMaybe def . lookupSubst x <$> ask



apSubst :: Subst a -> TC a k -> TC a k
apSubst s = flip runReader s . go
  where
    go :: forall a k'. TC a k' -> Reader (Subst a) (TC a k')
    go (TC v) = TC <$> (traverse (apSubst' go) v)

apSubstArg :: Subst a -> Arg a -> Arg a
apSubstArg s (ValArg     e) = ValArg     $ apSubst s e
apSubstArg s (GrammarArg e) = GrammarArg $ apSubst s e
apSubstArg s (ClassArg e)   = ClassArg   $ apSubst s e


