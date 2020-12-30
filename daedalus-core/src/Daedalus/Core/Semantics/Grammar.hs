module Daedalus.Core.Semantics.Grammar where

import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS8

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

    GCase c ->
      evalCase evalG (pError' FromSystem [] "Pattern match failure") c env



