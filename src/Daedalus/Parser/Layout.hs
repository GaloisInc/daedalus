{-# Language OverloadedStrings #-}
module Daedalus.Parser.Layout where

import Daedalus.Parser.Tokens
import AlexTools

virtual :: Token -> Lexeme Token -> Lexeme Token
virtual t l = l { lexemeToken = t, lexemeText = "" }

layout :: [Lexeme Token] -> [Lexeme Token]
layout tokIn = go False [] (error "Last token") tokIn
  where
  -- dbg = [ trace (show (lexemeToken x)) x |  x <- toks ]


  col = sourceColumn . sourceFrom . lexemeRange

  startBlock t next =
    case lexemeToken t of
      KWOf | nonBrace      -> True
      KWChoose | nonBrace  -> True
      KWChoose1 | nonBrace -> True
      KWblock              -> True
      _                    -> False
    where
    nonBrace = case next of
                 Lexeme { lexemeToken = OpenBrace } : _ -> False
                 _ -> True

  isEOF t = case lexemeToken t of
              TokEOF -> True
              _      -> False

  go first stack lastTok toks =
    case stack of
      c : cs ->
        case toks of
          [] -> virtual VClose lastTok : go first cs lastTok toks
          t : ts
            | col t < c || isEOF t ->
                            virtual VClose t : go False cs lastTok toks
            | not first && col t == c -> virtual VSemi t : emitTok stack t ts
            | otherwise  -> emitTok stack t ts
      [] -> case toks of
              [] -> []
              t : ts -> emitTok stack t ts

  emitTok stack t ts = t : checkStrtBlock stack t ts

  checkStrtBlock stack t toks
    | startBlock t toks = virtual VOpen t : go True (newBlock : stack) t toks
    | otherwise         = go False stack t toks
    where newBlock = case toks of
                       []     -> col t
                       t1 : _ -> col t1

