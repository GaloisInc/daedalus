module Daedalus.Core.Semantics.Grammar where

import qualified Data.Text as Text
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad(unless)
import Data.Word(Word8)

import RTS.Input(advanceBy, inputEmpty, inputByte, inputBytes)
import RTS.Parser
import RTS.Numeric(intToSize)
import RTS.ParserAPI( pPeek,pSetInput,(<||), (|||), pEnter
                    , pError', ParseErrorSource(..)
                    )

import Daedalus.Value

import Daedalus.Core
import Daedalus.Core.Semantics.Expr
import Daedalus.Core.Semantics.Env

evalG :: Grammar -> Env -> Parser Value
evalG gram env =
  case gram of
    Pure e    -> pure $! eval e env

    GetStream ->
      do v <- pPeek
         pure $! VStream v

    SetStream e ->
      do pSetInput $! valueToStream $ eval e env
         pure vUnit

    Match s m -> evalMatch s m env

    Fail src _ mbMsg -> pError' dsrc [] msg
      where
      dsrc = case src of
               ErrorFromUser   -> FromUser
               ErrorFromSystem -> FromSystem
      msg  = case mbMsg of
               Nothing -> "Parse error"
               Just e  -> BS8.unpack (valueToByteString (eval e env))

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

evalMatch :: Sem -> Match -> Env -> Parser Value
evalMatch sem mat env =
  case mat of

    MatchEnd ->
      do i <- pPeek
         unless (inputEmpty i) (pError' FromSystem [] "left over input")
         pure vUnit

    MatchBytes e ->
      do i <- pPeek
         let v  = eval e env
             bs = valueToByteString v
             ok = bs `BS8.isPrefixOf` inputBytes i
         unless ok (pError' FromSystem [] "match failed")
         let Just i1 = advanceBy (intToSize (BS.length bs)) i
         pSetInput $! i1
         case sem of
           SemNo  -> pure vUnit
           SemYes -> pure v

    MatchByte b ->
      do i <- pPeek
         case inputByte i of
           Just (w,i1) ->
             if evalByteSet b env w
                then do pSetInput $! i1
                        case sem of
                          SemNo  -> pure vUnit
                          SemYes -> pure (vByte w)
                else pError' FromSystem [] "byte does not match spec"
           Nothing -> pError' FromSystem [] "unexpected end of file"



evalByteSet :: ByteSet -> Env -> Word8 -> Bool
evalByteSet bs env =
  case bs of
    SetAny      -> const True
    SetSingle e -> (valueToByte (eval e env) ==)
    SetRange a b -> \w -> x <= w && w <= y
      where x = valueToByte (eval a env)
            y = valueToByte (eval b env)

    SetComplement f     -> not . evalByteSet f env
    SetUnion a b        -> \w -> evalByteSet a env w || evalByteSet b env w
    SetIntersection a b -> \w -> evalByteSet a env w && evalByteSet b env w

    SetLet x e b -> evalByteSet b $! defLocal x (eval e env) env
    SetCall f es -> lookupBFun f env [ eval e env | e <- es ]
    SetCase b    -> evalCase evalByteSet (const False) b env


