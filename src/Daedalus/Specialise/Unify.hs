{-# LANGUAGE GADTs, TypeOperators, PolyKinds, KindSignatures, FlexibleContexts, RankNTypes #-}
{-
  This module implements limited unification --- a unifier matches
  free variables only.
-} 
module Daedalus.Specialise.Unify where

import Control.Monad (zipWithM, foldM)

import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF

import Daedalus.Type.AST

data UnifyErr where
  UnificationMismatch :: forall k. TCName k -> TCName k -> TCName k
                      -> UnifyErr
  SyntaxMismatch :: UnifyErr

newtype Unifier = Unifier { getUnifier :: MapF TCName TCName }

emptyUnifier :: Unifier
emptyUnifier = Unifier MapF.empty

singletonUnifier :: TCName k -> TCName k -> Unifier
singletonUnifier k v = Unifier (MapF.singleton k v)

apUnifier :: Unifier -> TCName k -> TCName k
apUnifier u n | Just v <- MapF.lookup n (getUnifier u) = v
apUnifier _ n = n

-- Breaks if the two don't agree at all keys
mergeUnifiers :: Unifier -> Unifier -> Either UnifyErr Unifier
mergeUnifiers (Unifier u1) (Unifier u2) =
  Unifier <$> MapF.mergeWithKeyM merge pure pure u1 u2
  where
    merge :: TCName k -> TCName k -> TCName k
          -> Either UnifyErr (Maybe (TCName k))
    merge _ v1 v2 | v1 == v2 = Right (Just v1)
    merge k v1 v2            = Left (UnificationMismatch k v1 v2)

-- Ensures the first variables matches the second, and removes it from the unifier
variableCheck :: TCName k -> TCName k -> Unifier -> Either UnifyErr Unifier
variableCheck v1 v2 (Unifier u) =
  Unifier . MapF.updatedValue <$> MapF.updateAtKey v2 (Right Nothing) check u
  where
    check v | v1 == v = Right MapF.Delete
    check v           = Left (UnificationMismatch v2 v1 v)

-- Compute a unifier for the two arguments, s.t. the two arguments are
-- alpha-equivalend after substituting the resulting unifier into the
-- second argument --- the domain of the resulting unifier is exactly the
-- free variables of the second argument.
--
-- This isn't parameterised by the context as we can determine that
-- before unification.
class Unify t where
  unify :: t -> t -> Either UnifyErr Unifier

instance Unify a => Unify [a] where
  unify xs xs' | length xs == length xs'
    = do us <- zipWithM unify xs xs'
         foldM mergeUnifiers emptyUnifier us
  unify _ _ = Left SyntaxMismatch

instance (Unify a, Unify b) => Unify (a, b) where
  unify (a, b) (a', b') = do
    ua <- unify a a'
    ub <- unify b b'
    mergeUnifiers ua ub

instance (Unify a, Unify b, Unify c) => Unify (a, b, c) where
  unify (a, b, c) (a', b', c') = do
    uab <- unify (a, b) (a', b')
    uc  <- unify c c'
    mergeUnifiers uab uc

-- We ignore location data.
instance Unify (TC a k) where
  unify t1 t2 = unify (texprValue t1) (texprValue t2)

instance Unify a => Unify (Maybe a) where
  unify Nothing Nothing   = Right emptyUnifier
  unify (Just x) (Just y) = unify x y
  unify _        _        = Left SyntaxMismatch

instance Unify (Loop a k) where
  unify lp1 lp2 =
    do uB <- unify (loopBody lp1) (loopBody lp2)
       uB_1 <- case (loopKName lp1, loopKName lp2) of
                 (Nothing,Nothing) -> pure uB
                 (Just k,Just k')  -> variableCheck k k' uB
                 _                 -> Left SyntaxMismatch
       uB_2 <- variableCheck (loopElName lp1) (loopElName lp2) uB_1
       case (loopFlav lp1, loopFlav lp2) of
         (LoopMap,LoopMap) -> pure emptyUnifier
         (Fold x1 s1, Fold x2 s2) ->
            do uB_3 <- variableCheck x1 x2 uB_2
               uS   <- unify s1 s2
               mergeUnifiers uS uB_3
         _ -> Left SyntaxMismatch

instance Unify (TCF a k) where
  unify texpr1 texpr2 = 
    case (texpr1, texpr2) of
      ( TCPure e            , TCPure e'        ) -> unify e e'
      ( TCDo  Nothing e1 e2 , TCDo Nothing e1' e2' ) ->
        unify (e1, e2) (e1', e2')

      ( TCDo (Just x) e1 e2       , TCDo  (Just x') e1' e2' ) -> do
        do u1 <- unify e1 e1'
           u2 <- unify e2 e2'
           u2' <- variableCheck x x' u2
           mergeUnifiers u1 u2'

      ( TCGetByte s         , TCGetByte s'    ) | s == s' -> pure emptyUnifier
      ( TCMatch s b         , TCMatch s' b'   ) | s == s' -> unify b b'
      ( TCGuard e1          , TCGuard e2 )                -> unify e1 e2

      ( TCMatchBytes s b    , TCMatchBytes s' b' ) | s == s' -> unify b b'

      ( TCChoice c1 es t    , TCChoice c2 es' t' )
          | c1 == c2 && t == t' -> unify es es'

      ( TCOptional c e      , TCOptional c' e'   ) | c == c' -> unify e e'
      ( TCMany s c bnds e   , TCMany s' c' bnds' e' ) | s == s'
                                                      , c == c' ->
        unify (bnds, e) (bnds', e')

      ( TCCoerceCheck s t1 t2 e, TCCoerceCheck s' t1' t2' e')
        | s == s' && t1 == t1' && t2 == t2' -> unify e e'

      ( TCCoerce      t1 t2 e, TCCoerce      t1' t2' e')
        | t1 == t1' && t2 == t2' -> unify e e'

      -- Values
      ( TCLiteral l t        , TCLiteral l' t'  )
        | l == l' && t == t' -> pure emptyUnifier

      ( TCNothing t1, TCNothing t2 ) | t1 == t2 -> pure emptyUnifier
      ( TCJust e1,    TCJust e2) -> unify e1 e2

      -- We are claiming here that order of labels matters.
      ( TCStruct xs t       , TCStruct xs' t')
        | ls == ls' && t == t' -> unify es es'
        where
          (ls,  es)  = unzip xs
          (ls', es') = unzip xs'        

      ( TCArray xs t        , TCArray xs' t'  ) | t == t' -> unify xs xs'
      ( TCIn l e t          , TCIn l' e' t'    )
        | l == l' && t == t' -> unify e e'

      -- numeric            
      ( TCTriOp op e1 e2 e3 t1, TCTriOp op' e1' e2' e3' t2 )
        | op == op' && t1 == t2 -> unify (e1, (e2, e3)) (e1', (e2',e3'))

      ( TCBinOp op e1 e2 t1   , TCBinOp op' e1' e2' t2)
        | op == op' && t1 == t2 -> unify (e1, e2) (e1', e2')

      ( TCUniOp op    e     , TCUniOp op'     e' ) | op == op' -> unify e e'

      -- Sets               
      ( TCSetAny            , TCSetAny          ) -> pure emptyUnifier
      ( TCSetSingle e       , TCSetSingle e'    ) -> unify e e'
      ( TCSetComplement e   , TCSetComplement e') -> unify e e'
      ( TCSetRange e1 e2    , TCSetRange e1' e2') ->
        unify (e1, e2) (e1', e2')
      ( TCSetUnion es       , TCSetUnion es'    ) -> unify es es'
      ( TCSetOneOf bs       , TCSetOneOf bs'    ) | bs == bs' -> pure emptyUnifier
      ( TCSetDiff e1 e2     , TCSetDiff e1' e2' ) -> 
        unify (e1, e2) (e1', e2')

      -- Eliminators
      ( TCFor lp1, TCFor lp2 ) -> unify lp1 lp2

      ( TCSelStruct x n t   , TCSelStruct x' n' t')
        | n == n' && t == t' -> unify x x'

      ( TCIf be te fe       , TCIf be' te' fe'   ) ->
        unify (be, te, fe) (be', te', fe')
        -- FIXME: do we need to check Ctx?
      ( TCVar x             , TCVar x'        )
        | tcType x == tcType x' -> pure (singletonUnifier x' x)
      ( TCCall n ts xs      , TCCall n' ts' xs' )
        | n == n' && ts == ts' -> unify xs xs'        


      ( TCOffset, TCOffset ) -> pure emptyUnifier
      ( TCCurrentStream, TCCurrentStream ) -> pure emptyUnifier
      ( TCSetStream x, TCSetStream y ) -> unify x y

      ( TCStreamLen s1 x1 y1, TCStreamLen s2 x2 y2 )
          | s1 == s2 -> do u1 <- unify x1 x2
                           u2 <- unify y1 y2
                           mergeUnifiers u1 u2
      ( TCStreamOff s1 x1 y1, TCStreamOff s2 x2 y2 )
          | s1 == s2 -> do u1 <- unify x1 x2
                           u2 <- unify y1 y2
                           mergeUnifiers u1 u2


      ( TCErrorMode s1 p1, TCErrorMode s2 p2 )
        | s1 == s2 -> unify p1 p2

      _                                        -> Left SyntaxMismatch

instance Unify a => Unify (ManyBounds a) where
  unify (Exactly e) (Exactly e')      = unify e e'
  unify (Between l r) (Between l' r') =
    do u1 <- unify l l'
       u2 <- unify r r'
       mergeUnifiers u1 u2
  unify _             _               = Left SyntaxMismatch

instance Unify (Arg a) where
  unify (ValArg x)     (ValArg x')     = unify x x'
  unify (GrammarArg x) (GrammarArg x') = unify x x'
  unify _             _                = Left SyntaxMismatch
