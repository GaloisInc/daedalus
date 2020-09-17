{-# Language GADTs, RecordWildCards #-}
module Daedalus.Type.InferContext where

import Data.Parameterized.Some

import Daedalus.AST


-- | Compute the "natural" kind of an expression.
inferContext :: Expr -> Some Context
inferContext expr =
  case exprValue expr of
    ELiteral {}  -> Some AValue
    ENothing {}  -> Some AValue
    EJust e      -> inferContext e
    EIn (_ :> e) -> inferContext e
    EMatch {}    -> Some AGrammar
    EMatch1 {}   -> Some AGrammar
    EStruct fs   -> AValue `grammarIf` map inferStructField fs
    EArray es    -> AValue `grammarIf` map inferContext es

    EChoiceU {}           -> Some AGrammar    -- XXX: This is wrong due to "or" to be fixed soon
    EChoiceT {}           -> Some AGrammar
    EApp Name { .. } []   -> Some nameContext
    EApp Name { .. } es   -> nameContext `grammarIf` map inferContext es
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
        CoerceCheck -> inferContext e
        CoerceForce -> inferContext e

    EQuiet {} -> Some AGrammar

    EMapEmpty       -> Some AValue
    EMapInsert {}   -> Some AGrammar
    EMapLookup {}   -> Some AGrammar

    EArrayLength e  -> inferContext e
    EArrayIndex {}  -> Some AGrammar

    EPure {}        -> Some AGrammar
    EFail {}        -> Some AGrammar

    EFor _ _ _ _ e  -> inferContext e

    EIf {}          -> Some AValue  -- XXX: we should make `if` work for grammar

    EInRange {}     -> Some AClass

    ETriOp _ e1 e2 e3 -> AValue `grammarIf` map inferContext [e1,e2,e3]
    EBinOp _ e1 e2    -> AValue `grammarIf` map inferContext [e1,e2]
    EUniOp _ e        -> inferContext e

    ESel e sel ->
      case sel of
        SelStruct {}   -> inferContext e
        SelUnion {}    -> Some AGrammar
        SelTrue {}     -> Some AGrammar
        SelFalse {}    -> Some AGrammar
        SelNothing {}  -> Some AGrammar
        SelJust {}     -> Some AGrammar

    ECurrentStream    -> Some AGrammar
    ESetStream {}     -> Some AGrammar
    EStreamLen {}     -> Some AGrammar
    EStreamOff {}     -> Some AGrammar


grammarIf :: Context c -> [Some Context] -> Some Context
grammarIf d xs = if any isGrammar xs then Some AGrammar else Some d

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

