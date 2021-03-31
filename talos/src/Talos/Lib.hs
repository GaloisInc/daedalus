{-# Language ViewPatterns #-}
module Talos.Lib where

import Daedalus.Core

-- -----------------------------------------------------------------------------
-- Daedalus helpers

-- isBits :: Type -> Maybe (Bool, Integer)
-- isBits (isUInt -> Just n) = Just (False, n)
-- isBits (isSInt -> Just n) = Just (True, n)
-- isBits _                  = Nothing

-- isUInt :: Type -> Maybe Integer
-- isUInt (TUInt (TSize n)) = Just n
-- isUInt _                 = Nothing

-- isSInt :: Type -> Maybe Integer
-- isSInt (TSInt (TSize n)) = Just n
-- isSInt _                 = Nothing

-- isInteger :: Type -> Bool
-- isInteger (Type TInteger) = True
-- isInteger _              = False

-- isArray :: Type -> Maybe Type
-- isArray (Type (TArray t)) = Just t
-- isArray _                 = Nothing

-- isMaybe :: Type -> Maybe Type
-- isMaybe (Type (TMaybe t)) = Just t
-- isMaybe _                 = Nothing

-- isBool :: Type -> Bool
-- isBool (Type TBool) = True
-- isBool _            = False

-- stripParam :: Param -> TCName Value
-- stripParam (ValParam n) = n
-- stripParam _            = error "Shouldn't happen (stripParam)"
