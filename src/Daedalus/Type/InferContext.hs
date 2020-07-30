{-# Language GADTs, RecordWildCards #-}
module Daedalus.Type.InferContext where

import Data.Parameterized.Some

import Daedalus.AST


-- | This picks a context for an expression that may be interpreted differently
-- in different contexts, when the context is not known.
-- Generally, we interpret things as values, unless we are sure that it
-- must be a grammar.
inferContext :: Expr -> Some Context
inferContext expr =
  case exprValue expr of
    ENumber {}  -> Some AValue
    EBool {}    -> Some AValue
    ENothing {} -> Some AValue
    EJust {}    -> Some AValue
    EStruct fs
      | any (isGrammar . inferStructField) fs -> Some AGrammar
      | otherwise                             -> Some AValue
    EArray es
      | any (isGrammar . inferContext) es     -> Some AGrammar
      | otherwise                             -> Some AValue

    EChoiceU {}           -> Some AGrammar
    EChoiceT {}           -> Some AGrammar
    EApp Name { .. } _    -> Some nameContext

    EVar Name { .. }      -> Some nameContext

    ETry {}               -> Some AGrammar

    EAnyByte              -> Some AGrammar
    EOptional {}          -> Some AGrammar
    EMany {}              -> Some AGrammar
    EEnd {}               -> Some AGrammar
    EOffset {}            -> Some AGrammar

    EHasType sig e _ ->
      case sig of
        MatchType   -> inferContext e
        CoerceCheck -> Some AValue
        CoerceForce -> Some AValue

    EQuiet {} -> Some AGrammar

    EMapEmpty     -> Some AValue
    EMapInsert {} -> Some AGrammar
    EMapLookup {} -> Some AGrammar

    EArrayLength {} -> Some AValue
    EArrayIndex {}  -> Some AGrammar

    EPure {}        -> Some AGrammar
    EFail {}        -> Some AGrammar

    EFor _ _ _ _ e  -> inferContext e

    EIf {}          -> Some AValue  -- XXX: we should make `if` work for grammar

    EBytes {}       -> Some AValue
    EByte {}        -> Some AValue
    EInRange {}     -> Some AGrammar

    ETriOp {}       -> Some AValue
    EBinOp {}       -> Some AValue
    EUniOp {}       -> Some AValue

    ESel _ sel ->
      case sel of
        SelStruct {}   -> Some AValue
        SelUnion {}    -> Some AGrammar
        SelTrue {}     -> Some AGrammar
        SelFalse {}    -> Some AGrammar
        SelNothing {}  -> Some AGrammar
        SelJust {}     -> Some AGrammar

    ECurrentStream    -> Some AGrammar
    ESetStream {}     -> Some AGrammar
    EStreamLen {}     -> Some AGrammar
    EStreamOff {}     -> Some AGrammar

isGrammar :: Some Context -> Bool
isGrammar ctx =
  case ctx of
    Some AGrammar -> True
    _ -> False

inferStructField :: StructField Expr -> Some Context
inferStructField fi =
  case fi of
    Anon  {}  -> Some AGrammar
    COMMIT {} -> Some AGrammar
    _ :@= _   -> Some AGrammar
    _ := e    -> inferContext e

