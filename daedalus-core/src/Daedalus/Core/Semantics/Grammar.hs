module Daedalus.Core.Semantics.Grammar where

import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS8

import qualified Data.BitVector.Sized as BV

import RTS.Parser
import RTS.ParserAPI( pPeek,pSetInput,(<||), (|||), pEnter
                    , pError', ParseErrorSource(..)
                    )

import Daedalus.Core
import Daedalus.Core.Semantics.Expr
import Daedalus.Core.Semantics.Value
import Daedalus.Core.Semantics.Env

evalG :: Grammar -> Env -> Parser Value
evalG gram env =
  case gram of
    Pure e    -> pure $! eval e env

    GetStream ->
      do v <- pPeek
         pure $! VInput v

    SetStream e ->
      do pSetInput $! fromVInput $ eval e env
         pure VUnit

    Fail src _ mbMsg -> pError' dsrc [] msg
      where
      dsrc = case src of
               ErrorFromUser   -> FromUser
               ErrorFromSystem -> FromSystem
      msg  = case mbMsg of
               Nothing -> "Parse error"
               Just e  -> BS8.unpack (fromVByteArray (eval e env))

    Do_ g1 g2 ->
      do _ <- evalG g1 env
         evalG g2 env

    Do x g1 g2 ->
      do v <- evalG g1 env
         evalG g2 $! defLocal x v env

    Let x e g ->
      evalG g $! defLocal x (eval e env) env

    OrBiased g1 g2   -> evalG g1 env <|| evalG g2 env
    OrUnbiased g1 g2 -> evalG g1 env ||| evalG g2 env
    Call f es -> lookupGFun f env $! evalArgs es env
    Annot a g ->
      case a of
        NoFail -> evalG g env
        SrcAnnot t -> pEnter (Text.unpack t) (evalG g env)

    If e g1 g2 ->
      if fromVBool (eval e env) then evalG g1 env else evalG g2 env

    Case e alts ->
      let v = eval e env
      in case [ k | (p,k) <- alts, matches p v ] of
           g : _ -> evalG g env
           []    -> pError' FromSystem [] "Pattern match failure"

matches :: Pattern -> Value -> Bool
matches pat v =
  case pat of
    PBool b  -> VBool b == v
    PNothing -> case v of
                  VNothing {} -> True
                  _           -> False
    PJust    -> case v of
                  VJust {} -> True
                  _        -> False
    PNum n ->
      case v of
        VInt i    -> i == n
        VUInt _ u -> BV.asUnsigned u == n
        VSInt w s -> BV.asSigned w s == n
        _         -> False

    PCon l ->
      case v of
        VUnion _ l1 _ -> l == l1
        _              -> False

    PAny -> True




