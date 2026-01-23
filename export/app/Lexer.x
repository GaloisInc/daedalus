{
module Lexer where

import Data.Text(Text)
import Data.Text qualified as Text
import AlexTools
}

$alpha      = [a-z A-Z _]
$digit      = [0-9]
@ident      = $alpha ($alpha | $digit)*
@number     = $digit+
@comment    = "--" .* \n

:-

<0> {
"case"      { lexeme TokKW_case }
"def"       { lexeme TokKW_def }
"default"   { lexeme TokKW_default }
"extern"    { lexeme TokKW_extern }
"import"    { lexeme TokKW_import }
"of"        { lexeme TokKW_of }
"type"      { lexeme TokKW_type }

"["         { lexeme TokBracketOpen }
"]"         { lexeme TokBracketClose }
"("         { lexeme TokParenOpen }
")"         { lexeme TokParenClose }
"{"         { lexeme TokBraceOpen }
"}"         { lexeme TokBraceClose }
"<"         { lexeme TokLt }
">"         { lexeme TokGt }

"."         { lexeme TokDot }
","         { lexeme TokComma }
"::"        { lexeme TokColonColon }
":"         { lexeme TokColon }
"="         { lexeme TokEqual }
"->"        { startLayout }
"=>"        { lexeme TokFatRightArrow }

@comment    ;
$white      ;
@ident      { lexeme TokIdent }
@number     { lexeme TokNumber }
.           { matchText >>= \t -> lexeme (TokError ("Unexpected character: " <> t)) }
}

<comment> {
  "a"       ;
}

<object> {
  "$$"       { map (\l -> l { lexemeText = "$" }) <$> emit TokObject }
  "$"        { startSplice }
  [^\$]+     { emit TokObject }
  \n         { objectGotoNextLine }
}

<objectLineStart> {
  " "* \n   { emitObjectNL }
  " "*      { checkObjectEnd }
  \t        { lexeme (TokError "Please use spaces instead of tab characters for indentation") }
}

<splice> {
  $white    ;
  @ident    { emit TokIdent }
  "("       { emit TokParenOpen }
  ")"       { emit TokParenClose }
  "<"       { emit TokLt }
  ">"       { emit TokGt }
  "."       { emit TokDot }
  .         { emit (TokError "Unexpected character in slice.") }
}




{
data Token =
    TokKW_case
  | TokKW_def
  | TokKW_default
  | TokKW_extern
  | TokKW_import
  | TokKW_of
  | TokKW_type
  
  | TokIdent
  | TokNumber

  | TokParenOpen
  | TokParenClose
  | TokBracketOpen
  | TokBracketClose
  | TokBraceOpen
  | TokBraceClose
  | TokLt
  | TokGt

  | TokDot
  | TokComma
  | TokColon
  | TokColonColon
  | TokEqual
  | TokDollar
  | TokRightArrow
  | TokFatRightArrow
  | TokObject

  | TokError Text
  | TokEOF
    deriving Show

virtual :: SourcePos -> Token -> Lexeme Token
virtual pos t =
  Lexeme {
    lexemeRange = range pos,
    lexemeText  = "virtual: " <> Text.pack (show t),
    lexemeToken = t
  }


alexGetByte :: Input -> Maybe (Word8, Input)
alexGetByte = makeAlexGetByte \b ->
  let n = fromEnum b
  in if n > 127 then 0 else toEnum n

data LayoutState =
    LayoutStarting      -- ^ At the start of a layout block. Column of next token determines indentation.
  | LayoutBlock !Int    -- ^ Column for layout block

data LexState =
    Normal
  | InObject LayoutState            -- ^ in object language, not after new line
  | InObjectLineStart LayoutState   -- ^ in object language, after new line
  | InSplice !SourceRange !Int !Int -- ^ Splice start, counts number of open parens, outer layout
  | InComment !SourceRange LexState -- ^ XXX


startLayout :: Action LexState [Lexeme Token]
startLayout = setLexerState (InObject LayoutStarting) >> lexeme TokRightArrow
    
  
startSplice :: Action LexState [Lexeme Token]
startSplice =
  do
    s <- getLexerState
    r <- matchRange
    case s of
      InObject l ->
        do
          let newL =
                case l of
                  LayoutStarting -> sourceColumn (sourceFrom r)
                  LayoutBlock n  -> n
          setLexerState (InSplice r 0 newL)
          lexeme TokDollar
          
      _ -> error "[bug] `startSplice` not in object"


emitObjectNL :: Action LexState [Lexeme Token]
emitObjectNL =
  do
    r <- matchRange    
    s <- getLexerState
    case s of
      InObject LayoutStarting -> pure []
      InObjectLineStart LayoutStarting -> pure []
      _ -> pure [ Lexeme { lexemeToken = TokObject, lexemeText = "\n", lexemeRange = r } ]


objectGotoNextLine :: Action LexState [Lexeme Token]
objectGotoNextLine  =
  do
    s <- getLexerState
    case s of
      InObject l -> setLexerState (InObjectLineStart l) >> emitObjectNL
      _ -> error "[bug]: `objectLineStart` but not in object."


emit :: Token -> Action LexState [Lexeme Token]
emit t =
  do
    rng <- matchRange
    txt <- matchText

    let nextLex = Lexeme {
            lexemeRange = rng,
            lexemeToken = t,
            lexemeText = txt
          } 
        
    s <- getLexerState
    case s of
  
      InObject LayoutStarting -> setLexerState (InObject (LayoutBlock col))
        where col = sourceColumn (sourceFrom rng)
        
      InObject _ -> pure ()

      InSplice r n l ->
        case t of
          TokParenOpen -> setLexerState (InSplice r (n + 1) l)
          TokParenClose
            | n > 1     -> setLexerState (InSplice r (n - 1) l)
            | otherwise -> setLexerState (InObject (LayoutBlock l))
          _
            | n == 0 -> setLexerState (InObject (LayoutBlock l))
            | otherwise -> pure ()

      _ -> error "[BUG] `considerLayout`"

    pure [nextLex]


checkObjectEnd :: Action LexState [Lexeme Token]
checkObjectEnd =
  do
    s <- getLexerState
    case s of
      InObjectLineStart l ->
        do
          r <- matchRange
          n <- matchLength
          let col = sourceColumn (if n == 0 then sourceFrom r else sourceTo r) + 1
          setLexerState
            case l of
              LayoutBlock b | col < b -> Normal
              _ -> InObject l
          pure []
 
      _ -> error "[bug]: `checkObjectEnd` but not in object line start."


    

lexer :: Text -> Text -> [Lexeme Token]
lexer file = lexerAt (startPos file)

lexerAt :: SourcePos -> Text -> [Lexeme Token]
lexerAt loc txt = $makeLexer cfg (initialInputAt loc txt)
  where
  -- dbg xs = trace (unlines [ show (Text.unpack (lexemeText l)) ++
  --          "\t" ++ show (lexemeToken l) |  l <- xs ]) xs

  cfg = LexerConfig { lexerInitialState = Normal
                    , lexerStateMode = \s -> case s of
                                               Normal {} -> 0
                                               InObject {} -> object
                                               InObjectLineStart {} -> objectLineStart
                                               InSplice {} -> splice
                                               InComment {} -> comment
                    , lexerEOF = \s p ->
                        case s of
                          Normal -> [ virtual p TokEOF ]
                          InObject {} -> [ virtual p TokEOF ]
                          InObjectLineStart {} -> [ virtual p TokEOF ]
                          InSplice r _ _ -> 
                            [ Lexeme { lexemeToken =
                                         TokError "Unterminated splice."
                                   , lexemeText = ""
                                   , lexemeRange = r
                                   }
                            ]
                          InComment r _ ->
                            [ Lexeme { lexemeToken =
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

testFromFile :: FilePath -> IO ()
testFromFile file = test =<< readFile file


}