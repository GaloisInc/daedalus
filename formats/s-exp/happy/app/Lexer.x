{
module Lexer(Token(..), alexScanTokens) where

import Data.ByteString(ByteString, toStrict)
}

%wrapper  "basic-bytestring"

$WS    = [\0\9\12\32\n\r]
@IDENT = [a-z]+
:-

$WS+    ;
"("     { const OpenParen }
")"     { const CloseParen }
@IDENT  { Ident . toStrict }


{
data Token = OpenParen | CloseParen | Ident !ByteString
}
