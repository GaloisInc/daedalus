{-# Language ViewPatterns #-}
module Talos.Lib where

import Daedalus.Type.AST

-- -----------------------------------------------------------------------------
-- Daedalus helpers

typeOfStripG :: TypeOf a => a -> Type
typeOfStripG v = case typeOf v of
                   Type (TGrammar t) -> t
                   _ -> error "Not a grammar"

isBits :: Type -> Maybe (Bool, Integer)
isBits (isUInt -> Just n) = Just (False, n)
isBits (isSInt -> Just n) = Just (True, n)
isBits _                  = Nothing

isUInt :: Type -> Maybe Integer
isUInt (Type (TUInt (Type (TNum n)))) = Just n
isUInt _                              = Nothing

isSInt :: Type -> Maybe Integer
isSInt (Type (TSInt (Type (TNum n)))) = Just n
isSInt _                              = Nothing

isInteger :: Type -> Bool
isInteger (Type TInteger) = True
isInteger _              = False

isArray :: Type -> Maybe Type
isArray (Type (TArray t)) = Just t
isArray _                 = Nothing

isMaybe :: Type -> Maybe Type
isMaybe (Type (TMaybe t)) = Just t
isMaybe _                 = Nothing

stripParam :: Param -> TCName Value
stripParam (ValParam n) = n
stripParam _            = error "Shouldn't happen (stripParam)"
