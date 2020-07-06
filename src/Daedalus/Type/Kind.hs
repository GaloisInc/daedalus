{-# Language BlockArguments, OverloadedStrings #-}
module Daedalus.Type.Kind where

import Control.Monad(unless,forM)

import Daedalus.Panic(panic)
import Daedalus.PP

import Daedalus.Type.Monad
import Daedalus.Type.AST
import Daedalus.AST

checkType :: Kind -> SrcType -> TypeM ctx Type
checkType expK srcty =
  case srcty of
    SrcVar x ->
      case nameScope x of
        ModScope {} ->
          do let nm = TCTy x
             mb <- lookupTypeDef nm
             case mb of
               Just td -> do ps <- forM (tctyParams td) \_ ->
                                      newTVar x KValue
                             pure (tCon (tctyName td) ps)
               Nothing -> do needsDef x nm
                             pure (tCon nm [])

        Local {} ->
          do mb <- lookupLocalTyVar x
             case mb of
               Just t -> do expect (kindOf t)
                            pure t
               Nothing -> do t <- newTVar srcty expK
                             newLocalTyVar x t
                             pure t

        Unknown _ -> panic "checkType"
                        [ "Unresolved name in type: " ++ show (pp x) ]

    SrcType ty ->
      case thingValue ty of
        TGrammar t ->
          do expect KGrammar
             tGrammar <$> checkType KValue t

        TNum n      -> expect KNumber >> pure (tNum n)

        TStream     -> expect KValue >> pure tStream    -- hmm
        TByteClass  -> expect KClass >> pure tByteClass

        TUInt t -> do expect KValue
                      tUInt <$> checkType KNumber t

        TSInt t -> do expect KValue
                      tSInt <$> checkType KNumber t

        TInteger -> expect KValue >> pure tInteger
        TBool    -> expect KValue >> pure tBool
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




