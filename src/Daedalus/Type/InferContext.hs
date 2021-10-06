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
    EImplicit IPName { .. } -> Some ipContext

    ETry {}               -> Some AGrammar

    EAnyByte              -> Some AGrammar
    EOptional {}          -> Some AGrammar
    EMany {}              -> Some AGrammar
    EEnd {}               -> Some AGrammar
    EOffset {}            -> Some AGrammar

    EHasType sig e _ ->
      case sig of
        MatchType   -> inferContext e
        CoerceCheck -> Some AGrammar
        CoerceSafe  -> inferContext e
        CoerceForce -> inferContext e

    EQuiet {} -> Some AGrammar

    EMapEmpty       -> Some AValue
    EMapInsert {}   -> Some AGrammar
    EMapLookup {}   -> Some AGrammar

    EArrayLength e  -> inferContext e
    EArrayIndex {}  -> Some AGrammar

    EPure {}        -> Some AGrammar
    EFail {}        -> Some AGrammar

    EFor flav _k _v e body -> AValue `grammarIf` map inferContext es
      where es = case flav of
                   FMap -> [e,body]
                   FFold _ i -> [i,e,body]

    EIf e1 e2 e3    -> AValue `grammarIf` map inferContext [e1,e2,e3]

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
    ECase e alts
      | any isClass acs -> grammarIf AClass (ec : acs)
      | otherwise       -> grammarIf AValue (ec : acs)
      where
      ec  = inferContext e
      acs = map inferAlt alts
      inferAlt p =
        case p of
          PatternDefault rhs -> inferContext rhs
          PatternCase _ rhs  -> inferContext rhs

grammarIf :: Context c -> [Some Context] -> Some Context
grammarIf d xs =
  case d of
    AValue   -> if any (\x -> isGrammar x || isClass x) xs
                 then Some AGrammar
                 else Some d
    AClass   -> if any isGrammar xs then Some AGrammar else Some d
    AGrammar -> Some AGrammar

isGrammar :: Some Context -> Bool
isGrammar ctx =
  case ctx of
    Some AGrammar -> True
    _ -> False

isClass :: Some Context -> Bool
isClass ctx =
  case ctx of
    Some AClass -> True
    _ -> False


inferStructField :: StructField Expr -> Some Context
inferStructField fi =
  case fi of
    Anon  {}  -> Some AGrammar
    COMMIT {} -> Some AGrammar
    _ :@= _   -> Some AGrammar
    _ := e    -> inferContext e
    IPName { .. } :?= _   -> Some ipContext

