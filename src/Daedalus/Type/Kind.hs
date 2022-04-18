{-# Language BlockArguments, OverloadedStrings #-}
module Daedalus.Type.Kind where

import Control.Monad(unless,forM)

import Daedalus.Panic(panic)
import Daedalus.PP

import Daedalus.Type.Monad
import Daedalus.Type.Constraints(unify)
import Daedalus.Type.AST
import Daedalus.AST

checkType :: Kind -> SrcType -> TypeM ctx Type
checkType expK srcty =
  case srcty of
    SrcCon x xs ->
      do ks <- lookupTySynArgs x
         ts <- forM (zip ks (map Just xs ++ repeat Nothing)) \(k,t) ->
               case t of
                 Just arg -> checkType k arg
                 Nothing  -> newTVar x k
         t  <- lookupTySyn x ts
         case kindOf t of
           KGrammar ->
             do expect KValue
                a <- newTVar srcty KValue
                unify t (srcty, tGrammar a)
                pure a
           KValue   -> expect KValue >> pure t
           KClass   -> expect KClass >> pure t
           KNumber  -> panic "checkType" ["kind of rule is a number"]

    SrcVar x ->
      do mb <- lookupLocalTyVar (thingValue x)
         case mb of
           Just t -> do expect (kindOf t)
                        pure t
           Nothing -> do t <- newTVar srcty expK
                         newLocalTyVar (thingValue x) t
                         pure t

    SrcType ty ->
      case thingValue ty of
        TGrammar {} -> panic "checkType" [ "TGrammar", show (pp ty) ]
        TFun {}     -> panic "checkType" [ "Funcion", show (pp ty) ]
        -- XXX: we need to change this if we allow users to write function types

        TNum n      -> expect KNumber >> pure (tNum n)

        TStream     -> expect KValue >> pure tStream    -- hmm
        TByteClass  -> expect KClass >> pure tByteClass

        TUInt t -> do expect KValue
                      tUInt <$> checkType KNumber t

        TSInt t -> do expect KValue
                      tSInt <$> checkType KNumber t

        TInteger -> expect KValue >> pure tInteger
        TBool    -> expect KValue >> pure tBool
        TFloat   -> expect KValue >> pure tFloat
        TDouble  -> expect KValue >> pure tDouble
        TUnit    -> expect KValue >> pure tUnit
        TArray t -> do expect KValue
                       tArray <$> checkType KValue t

        TMaybe t -> do expect KValue
                       tMaybe <$> checkType KValue t

        TMap k v -> do expect KValue
                       tMap <$> checkType KValue k <*> checkType KValue v
  where
  expect :: Kind -> TypeM ctx ()
  expect actK =
    unless (expK == actK) $
      reportDetailedError srcty "Invalid type"
        [ "Expected a type for" <+> pp expK
        , "Actual type is for" <+> pp actK
        ]




