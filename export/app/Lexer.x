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
"case"      { emit TokKW_case }
"def"       { emit TokKW_def }
"default"   { emit TokKW_default }
"extern"    { emit TokKW_extern }
"for"       { emit TokKW_for }
"import"    { emit TokKW_import }
"in"        { emit TokKW_in }
"init"      { emit TokKW_init }
"of"        { startLayout TokKW_of }
"return"    { emit TokKW_return }
"type"      { emit TokKW_type }

"["         { emit TokBracketOpen }
"]"         { emit TokBracketClose }
"("         { emit TokParenOpen }
")"         { emit TokParenClose }
"{"         { emit TokBraceOpen }
"}"         { emit TokBraceClose }
"<"         { emit TokLt }
">"         { emit TokGt }

"."         { emit TokDot }
","         { emit TokComma }
"::"        { emit TokColonColon }
":"         { emit TokColon }
"="         { emit TokEqual }
"->"        { startLayout TokRightArrow }
"=>"        { emit TokFatRightArrow }

@comment    ;
$white      ;
@ident      { emit TokIdent }
@number     { emit TokNumber }
.           { matchText >>= \t -> lexeme (TokError ("Unexpected character: " <> t)) }
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
  | TokKW_for
  | TokKW_import
  | TokKW_in
  | TokKW_init
  | TokKW_of
  | TokKW_return
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

  | TokLayoutSep

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

type OuterLayouts = Maybe Int

data LexState =
    Normal (Maybe LayoutState)
  | InObject LayoutState OuterLayouts      -- ^ in object language, not after new line
  | InObjectLineStart LayoutState OuterLayouts  -- ^ in object language, after new line
  | InSplice !SourceRange !Int !Int OuterLayouts -- ^ Splice start, counts number of open parens, outer layoutIn


startLayout :: Token -> Action LexState [Lexeme Token]
startLayout tok =
  do
    s <- getLexerState
    case (tok, s) of
      (TokKW_of, Normal Nothing) ->
        setLexerState (Normal (Just LayoutStarting)) >> lexeme tok
      (TokRightArrow, Normal Nothing) ->
        setLexerState (InObject LayoutStarting Nothing) >> lexeme tok
      (TokRightArrow, Normal (Just (LayoutBlock n))) ->
        setLexerState (InObject LayoutStarting (Just n)) >> lexeme tok
      _ ->
        do
          r <- matchText
          lexeme (TokError ("Unexpected `" <> r <> "`"))
    
  
startSplice :: Action LexState [Lexeme Token]
startSplice =
  do
    s <- getLexerState
    r <- matchRange
    case s of
      InObject l out ->
        do
          let newL =
                case l of
                  LayoutStarting -> sourceColumn (sourceFrom r)
                  LayoutBlock n  -> n
          setLexerState (InSplice r 0 newL out)
          lexeme TokDollar
          
      _ -> error "[bug] `startSplice` not in object"


emitObjectNL :: Action LexState [Lexeme Token]
emitObjectNL =
  do
    r <- matchRange    
    s <- getLexerState
    case s of
      InObject LayoutStarting _ -> pure []
      InObjectLineStart LayoutStarting _ -> pure []
      _ -> pure [ Lexeme { lexemeToken = TokObject, lexemeText = "\n", lexemeRange = r } ]


objectGotoNextLine :: Action LexState [Lexeme Token]
objectGotoNextLine  =
  do
    s <- getLexerState
    case s of
      InObject l out -> setLexerState (InObjectLineStart l out) >> emitObjectNL
      _ -> error "[bug]: `objectLineStart` but not in object."


emit :: Token -> Action LexState [Lexeme Token]
emit t =
  do
    
    rng <- matchRange
    txt <- matchText
    let col = sourceColumn (sourceFrom rng)

    let nextLex = Lexeme {
            lexemeRange = rng,
            lexemeToken = t,
            lexemeText = txt
          } 
        
    s <- getLexerState
    sep <-
      case s of

        Normal Nothing -> pure []

        Normal (Just LayoutStarting) ->
          setLexerState (Normal (Just (LayoutBlock col))) >> pure []

        Normal (Just (LayoutBlock b))
          | col > b   -> pure []
          | col == b  -> pure [nextLex { lexemeToken = TokLayoutSep }]
          | otherwise -> setLexerState (Normal Nothing) >> pure []

        InObject LayoutStarting out ->
          setLexerState (InObject (LayoutBlock col) out) >> pure []

        InObject _ _ -> pure []

        InSplice r n l out ->
          do
            case t of
              TokParenOpen -> setLexerState (InSplice r (n + 1) l out)
              TokParenClose
                | n > 1     -> setLexerState (InSplice r (n - 1) l out)
                | otherwise -> setLexerState (InObject (LayoutBlock l) out)
              _
                | n == 0 -> setLexerState (InObject (LayoutBlock l) out)
                | otherwise -> pure ()
            pure []

        _ -> error "[BUG] `considerLayout`"

    pure (sep ++ [nextLex])


checkObjectEnd :: Action LexState [Lexeme Token]
checkObjectEnd =
  do
    s <- getLexerState
    case s of
      InObjectLineStart l out ->
        do
          r <- matchRange
          n <- matchLength
          let col = sourceColumn (if n == 0 then sourceFrom r else sourceTo r) + 1
          case l of
            LayoutBlock b | col < b ->
              case out of
                Nothing -> setLexerState (Normal Nothing)
                Just outB
                  | col < outB  -> setLexerState (Normal Nothing)
                  | otherwise   -> setLexerState (Normal (Just (LayoutBlock outB)))
            _ -> setLexerState (InObject l out)
      _ -> error "[bug]: `checkObjectEnd` but not in object line start."
    pure []


    

lexer :: Text -> Text -> [Lexeme Token]
lexer file = lexerAt (startPos file)

lexerAt :: SourcePos -> Text -> [Lexeme Token]
lexerAt loc txt = $makeLexer cfg (initialInputAt loc txt)
  where
  -- dbg xs = trace (unlines [ show (Text.unpack (lexemeText l)) ++
  --          "\t" ++ show (lexemeToken l) |  l <- xs ]) xs

  cfg = LexerConfig { lexerInitialState = Normal Nothing
                    , lexerStateMode = \s -> case s of
                                               Normal {} -> 0
                                               InObject {} -> object
                                               InObjectLineStart {} -> objectLineStart
                                               InSplice {} -> splice
                    , lexerEOF = \s p ->
                        case s of
                          Normal {} -> [ virtual p TokEOF ]
                          InObject {} -> [ virtual p TokEOF ]
                          InObjectLineStart {} -> [ virtual p TokEOF ]
                          InSplice r _ _ _ -> 
                            [ Lexeme { lexemeToken =
                                         TokError "Unterminated splice."
                                   , lexemeText = ""
                                   , lexemeRange = r
                                   }
                            ]
                    }

test :: String -> IO ()
test txt = mapM_ pp (lexer (Text.pack "(test)") (Text.pack txt))
  where
  pp l = putStrLn $ prettySourceRange (lexemeRange l) ++ ": " ++
                                      show (lexemeToken l)

testFromFile :: FilePath -> IO ()
testFromFile file = test =<< readFile file


}