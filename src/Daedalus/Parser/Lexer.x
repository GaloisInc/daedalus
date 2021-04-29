{
{-# Language TemplateHaskell, OverloadedStrings, BlockArguments #-}
module Daedalus.Parser.Lexer
  ( Lexeme(..), Token(..)
  , lexer
  ) where

import AlexTools
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Char(isDigit)

import Daedalus.Parser.Tokens
import Daedalus.Parser.Layout

-- import Debug.Trace

}

$bigAlpha   = [A-Z]
$smallAlpha = [a-z]
$alpha      = [$bigAlpha $smallAlpha _]
$digit      = [0-9]
$hexDigit   = [0-9a-fA-F]
$binDigit   = [01]
$anybyte    = [\32-\127]
$ws         = [\0\9\10\13\32]

@bigIdent   = $bigAlpha   [$alpha $digit]*
@smallIdent = $smallAlpha [$alpha $digit]*
@setIdent   = \$ @smallIdent
@natural    = $digit+
@integer    = \-? @natural
@hexLiteral = 0 [xX] $hexDigit $hexDigit*
@binLiteral = 0 [bB] $binDigit $binDigit*

@esc        = \\ (@natural | \\ | \' | " | n | t | r | [xX] $hexDigit $hexDigit* )
@byte       = \' ($anybyte # [\\] | @esc) \'
@bytes      = \" ($anybyte # [\"\\] | @esc)* \"

@comment    = "--" .* \n

:-

<comment> {
"{-"        { startComment }
"-}"        { endComment }
.           ;
\n          ;
}

<0> {
$ws+        ;
@comment    ;
"{-"        { startComment }
"("         { lexeme OpenParen }
")"         { lexeme CloseParen }
"{"         { lexeme OpenBrace }
"}"         { lexeme CloseBrace }
"{|"        { lexeme OpenBraceBar }
"|}"        { lexeme CloseBraceBar }
"["         { lexeme OpenBracket }
"]"         { lexeme CloseBracket }


"@"         { lexeme AtSign }
"!"         { lexeme Bang }
"^"         { lexeme Hat }
"="         { lexeme Equals }
"=="        { lexeme DoubleEquals }
"!="        { lexeme BangEquals }
"<="        { lexeme TokLeq }
">="        { lexeme TokGeq }
"<"         { lexeme OpenTri }
">"         { lexeme CloseTri }
":"         { lexeme Colon }
";"         { lexeme Semi }
".."        { lexeme DotDot }
"."         { lexeme Dot }
","         { lexeme Comma }
"|"         { lexeme Bar }
".|."       { lexeme DotBarDot }
".&."       { lexeme DotAmpDot }
".^."       { lexeme DotHatDot }
"||"        { lexeme BarBar }
"&&"        { lexeme AmpAmp }
"<|"        { lexeme LtBar }
"$$"        { lexeme DollarDollar }
"+"         { lexeme Plus }
"-"         { lexeme Minus }
"*"         { lexeme Star }
"/"         { lexeme ForwardSlash }
"%"         { lexeme Percent }
"#"         { lexeme Hash }
"<#"        { lexeme LeftHash }
"<<"        { lexeme ShiftL }
">>"        { lexeme ShiftR }
"~"         { lexeme BitwiseComplementT }
"->"        { lexeme RightArrow }
"_"         { lexeme Underscore }

"import"    { lexeme KWImport }
"def"       { lexeme KWDef }
"bitdata"   { lexeme KWBitData }
"where"     { lexeme KWWhere }

"for"       { lexeme KWFor }
"map"       { lexeme KWMap }
"in"        { lexeme KWIn }
"is"        { lexeme KWIs }
"of"        { lexeme KWOf }
"if"        { lexeme KWIf }
"try"       { lexeme KWTry }
"then"      { lexeme KWThen }
"else"      { lexeme KWElse }
"as"        { lexeme KWAs }
"as!"       { lexeme KWAsBang }
"as?"       { lexeme KWAsQuestion }
"case"      { lexeme KWCase }
"block"     { lexeme KWblock }
"let"       { lexeme KWlet }

"Choose"    { lexeme KWChoose }
"Choose1"   { lexeme KWChoose1 }
"Optional"  { lexeme KWOptional }
"Optional?" { lexeme KWOptionalQuestion }
"Many"      { lexeme KWMany }
"Many?"     { lexeme KWManyQuestion }
"UInt8"     { lexeme KWUInt8 }
"$uint"     { lexeme KWDollarUInt }
"Match"     { lexeme KWMatch }
"Match1"    { lexeme KWMatch1 }
"END"       { lexeme KWEND }
"commit"    { lexeme KWCOMMIT }
"Fail"      { lexeme KWFail }

"empty"     { lexeme KWMapEmpty }
"Insert"    { lexeme KWMapInsert }
"Lookup"    { lexeme KWMapLookup }

"Offset"    { lexeme KWOffset }
"SetStream" { lexeme KWSetStream }
"GetStream" { lexeme KWGetStream }
"Take"      { lexeme KWTake }
"Drop"      { lexeme KWDrop }
"arrayStream" { lexeme KWArrayStream }

"Index"     { lexeme KWArrayIndex }
"concat"    { lexeme KWConcat }
"length"    { lexeme KWArrayLength }
"rangeUp"   { lexeme KWRangeUp }
"rangeDown" { lexeme KWRangeDown }

"true"      { lexeme KWTrue }
"false"     { lexeme KWFalse }

"just"      { lexeme KWJust }
"nothing"   { lexeme KWNothing }

"int"       { lexeme KWInt }
"uint"      { lexeme KWUInt }
"sint"      { lexeme KWSInt }
"bool"      { lexeme KWBool }
"maybe"     { lexeme KWMaybe }
"stream"    { lexeme KWStream }

@bigIdent   { lexeme BigIdent }
@smallIdent { lexeme SmallIdent }
@setIdent   { lexeme SetIdent }
@byte       { lexByte }
@bytes      { lexBytes }
@integer    { lexInteger }
@hexLiteral { lexHexLiteral }
@binLiteral { lexBinLiteral }

.           { do { txt <- matchText
                 ; lexeme (TokError
                            ("Unexpected character: " ++ Text.unpack txt))
                 } }
}

{

lexInteger :: Action s [Lexeme Token]
lexInteger =
  do x <- Text.unpack <$> matchText
     lexeme $! Number (read x) Nothing

lexHexLiteral :: Action s [Lexeme Token]
lexHexLiteral =
  do x <- Text.unpack <$> matchText
     -- read supports hex literals too
     lexeme $! Number (read x) (Just ((length x - 2) * 4)) -- - 2 for '0x'

lexBinLiteral :: Action s [Lexeme Token]
lexBinLiteral =
  do ds <- Text.unpack <$> matchText
     let vs = map (\x -> if x == '0' then 0 else 1) (drop 2 ds) -- removing the '0b'
         r  = foldl (\acc x -> x + 2 * acc) 0 vs
     lexeme $! Number r (Just (length vs))

lexByte :: Action s [Lexeme Token]
lexByte =
  do x <- Text.unpack <$> matchText
     lexeme $! case unEsc (noQuotes x) of
                 Left err -> TokError err
                 Right (a,_) -> Byte a

lexBytes :: Action s [Lexeme Token]
lexBytes =
  do x <- Text.unpack <$> matchText
     lexeme $!
       case go (noQuotes x) of
         Left err -> TokError err
         Right bs -> Bytes (BS.pack bs)
  where
  go xs = case xs of
            [] -> Right []
            _  -> do (c,cs) <- unEsc xs
                     rest <- go cs
                     pure (c:rest)

noQuotes :: String -> String
noQuotes = init . drop 1


unEsc :: String -> Either String (Word8,String)
unEsc xs =
  case xs of
    '\\' : esc -> doEsc esc
    c    : cs  -> Right (w8 c, cs)
    _          -> error "Bug: empty byte"

  where
  w8 = toEnum . fromEnum

  doEsc cs =
    case span isDigit cs of
      ([],x : ys) ->
        case x of
          't'   -> Right (w8 '\t', ys)
          'n'   -> Right (w8 '\n', ys)
          'r'   -> Right (w8 '\r', ys)
          '\\'  -> Right (w8 '\\', ys)
          '\''  -> Right (w8 '\'', ys)
          '"'   -> Right (w8 '"', ys)
          'x'   -> getHexEsc ys
          'X'   -> getHexEsc ys
          _     -> error "Bug: unexpected escape"

      (ds,ys) -> let n = read ds
                 in if n < 256 then Right (fromInteger n, ys)
                               else Left "Byte literal is too large."
  getHexEsc cs = let (digits, other) = span isHexDigit cs
                 in case digits of
                      [] -> Left "No digits in hex escape"
                      _  -> let n = read ('0' : 'x' : digits)
                            in if n < 256
                                 then Right (fromInteger n, other)
                                 else Left "Byte hex literal is too large."
  isHexDigit = flip elem (['0'..'9'] ++ ['a' .. 'f'] ++ ['A' .. 'F'])

alexGetByte :: Input -> Maybe (Word8, Input)
alexGetByte = makeAlexGetByte (toEnum . fromEnum)

data LexState = Normal | InComment !SourceRange LexState

startComment :: Action LexState [Lexeme Token]
startComment =
  do r <- matchRange
     s <- getLexerState
     setLexerState (InComment r s)
     pure []

endComment :: Action LexState [Lexeme Token]
endComment =
  do ~(InComment _ s) <- getLexerState
     setLexerState s
     pure []

lexer :: Text -> Text -> [Lexeme Token]
lexer file txt = layout ($makeLexer cfg (initialInput file txt))
  where
  -- dbg xs = trace (unlines [ show (Text.unpack (lexemeText l)) ++
  --          "\t" ++ show (lexemeToken l) |  l <- xs ]) xs

  eof p = Lexeme { lexemeToken = TokEOF
                 , lexemeText  = ""
                 , lexemeRange = AlexTools.range p
                 }
  cfg = LexerConfig { lexerInitialState = Normal
                    , lexerStateMode = \s -> case s of
                                               Normal -> 0
                                               InComment {} -> 1
                    , lexerEOF = \s p ->
                        [ case s of
                            Normal -> eof p
                            InComment r _ ->
                              Lexeme { lexemeToken =
                                           TokError "Unterminated comment."
                                     , lexemeText = ""
                                     , lexemeRange = r
                                     } ]

                    }

test :: String -> IO ()
test txt = mapM_ pp (lexer (Text.pack "(test)") (Text.pack txt))
  where
  pp l = putStrLn $ prettySourceRange (lexemeRange l) ++ ": " ++
                                      show (lexemeToken l)

}

