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
"def"       { emit TokKW_def }
"case"      { emit TokKW_case }
"of"        { startLayout TokKW_of }

"["         { emit TokBracketOpen }
"]"         { emit TokBracketClose }
"("         { emit TokParenOpen }
")"         { emit TokParenClose }
"{"         { emit TokBraceOpen }
"}"         { emit TokBraceClose }

"."         { emit TokDot }
","         { emit TokComma }
"::"        { emit TokColonColon }
":"         { emit TokColon }
"="         { emit TokEqual }
"->"        { startLayout TokRightArrow }
@comment    ;
$white      ;
@ident      { emit TokIdent }
@number     { emit TokNumber }
.           { matchText >>= \t -> lexeme (TokError ("Unexpected character: " <> t)) }
}

<comment> {
  "a"       ;
}

<object> {
  "$$"       { map (\l -> l { lexemeText = "$" }) <$> emit TokObject }
  "$"        { startSplice }
  [~\$]+     { emit TokObject }
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
  "."       { emit TokDot }
  .         { emit (TokError "Unexpected character in slice.") }
}




{
data Token =
    TokKW_def
  | TokKW_case
  | TokKW_of
  
  | TokIdent
  | TokNumber

  | TokParenOpen
  | TokParenClose
  | TokBracketOpen
  | TokBracketClose
  | TokBraceOpen
  | TokBraceClose

  | TokDot
  | TokComma
  | TokColon
  | TokColonColon
  | TokEqual
  | TokDollar
  | TokRightArrow
  | TokObject

  | TokLayoutStart
  | TokLayoutSep
  | TokLayoutEnd

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

data Layout = Layout {
  layoutState :: LayoutState,   -- ^ Current block
  layoutStack :: [Int]          -- ^ Still open layout blocks
}

initialLayout :: Layout
initialLayout = Layout { layoutState = LayoutStarting, layoutStack = [] }

data LexState =
    Normal Layout
  | InObject Layout           -- ^ in object language, not after new line
  | InObjectLineStart Layout  -- ^ in object language, after new line
  | InSplice !SourceRange !Int Layout  -- ^ Splice start, counts number of open parens, outer layout
  | InComment !SourceRange LexState


emitObjectNL :: Action LexState [Lexeme Token]
emitObjectNL =
  do
    r <- matchRange
    s <- getLexerState
    case s of
      InObject l | LayoutStarting <- layoutState l -> pure []
      InObjectLineStart l | LayoutStarting <- layoutState l -> pure []
      _ -> pure [ Lexeme { lexemeToken = TokObject, lexemeText = "\n", lexemeRange = r } ]


objectGotoNextLine :: Action LexState [Lexeme Token]
objectGotoNextLine  =
  do
    s <- getLexerState
    case s of
      InObject l -> setLexerState (InObjectLineStart l) >> emitObjectNL
      _ -> error "[bug]: `objectLineStart` but not in object."

checkObjectEnd :: Action LexState [Lexeme Token]
checkObjectEnd =
  do
    s <- getLexerState
    case s of
      InObjectLineStart l ->
        do
          r <- matchRange
          let col = sourceColumn (sourceTo r) + 1
          case layoutState l of
            LayoutStarting -> setLexerState (InObject l) >> pure []
            LayoutBlock b
              | col >= b -> setLexerState (InObject l) >> pure []
              | b' : bs <- layoutStack l ->
                do
                  setLexerState (Normal (Layout { layoutState = LayoutBlock b', layoutStack = bs }))
                  pure [virtual (sourceTo r) TokLayoutEnd]
              | otherwise -> error "[bug]: `checkObjectEnd` no outer layout"
      _ -> error "[bug]: `checkObjectEnd` but not in object line start."





startLayout :: Token -> Action LexState [Lexeme Token]
startLayout tok =
  do
    s <- getLexerState
    case s of
      Normal l ->
        case layoutState l of
          LayoutStarting -> bad
          LayoutBlock n ->
            do
              let newL = l { layoutState = LayoutStarting, layoutStack = n : layoutStack l }
              case tok of
                TokRightArrow -> setLexerState (InObject newL)
                _ -> setLexerState (Normal newL)
              lexeme tok
      InSplice {} -> error "[bug] `startLayout` in splice"
      InObject {} -> error "[bug] `startLayout` in object"
      InObjectLineStart {} -> error "[bug] `startLayout` in object line start"
      InComment {} -> error "[bug] `startLayout` in comment"
  where
  bad =
    lexeme (
      TokError
        case tok of
          TokKW_of -> "Unexpected token: `of`"
          _ -> error ("Unexpected layout starter: " ++ show tok)
    )
      


considerLayout :: Lexeme Token -> Action LexState [Lexeme Token]
considerLayout nextLex =
  do
    s <- getLexerState
    case s of
      Normal l ->
        do
          let (mbL, toks) = go l
          maybe (pure ()) (setLexerState . Normal) mbL
          pure toks
      InObject l ->
        do
          case checkStarting l of
            Nothing -> pure [nextLex]
            Just newL ->
              do
                setLexerState (InObject newL)
                pure [virt TokLayoutStart, nextLex]
      InSplice r n l ->
        do
          case lexemeToken nextLex of
            TokParenOpen -> setLexerState (InSplice r (n + 1) l)
            TokParenClose
              | n > 1     -> setLexerState (InSplice r (n - 1) l)
              | otherwise -> setLexerState (InObject l)
            _
              | n == 0 -> setLexerState (InObject l)
              | otherwise -> pure ()
          pure [nextLex]


      _ -> error "[bug]: `considerLayout` but not in normal or object."
        
  where
  checkStarting l =
    case layoutState l of
      LayoutStarting -> Just l { layoutState = LayoutBlock (sourceColumn pos) }
      _ -> Nothing

  go l =
    case layoutState l of
      LayoutStarting -> (Just l { layoutState = LayoutBlock col }, [ virt TokLayoutStart, nextLex ])
      LayoutBlock n ->
        case compare col n of
          EQ -> (Nothing, [ virt TokLayoutSep, nextLex ])
          GT -> (Nothing, [ nextLex ])
          LT -> close [virt TokLayoutEnd] (layoutStack l) 
            where
            close toks xs =
              case xs of
                b : bs ->
                  case compare col b of
                    LT -> close (virt TokLayoutEnd : toks) bs
                    EQ -> (Just Layout { layoutState = LayoutBlock b, layoutStack = bs }, toks ++ [ virt TokLayoutSep, nextLex])
                    GT -> (Just Layout { layoutState = LayoutBlock b, layoutStack = bs }, toks ++ [ nextLex ])
                [] -> (Just Layout { layoutState = LayoutStarting, layoutStack = [] }, toks ++ [err])

  virt    = virtual pos
  pos     = sourceFrom (lexemeRange nextLex)
  col     = sourceColumn pos
  err     = nextLex { lexemeToken = TokError "Unexpected token, possibly due to bad layout." }
  
endLayouts :: SourcePos -> Layout -> [Lexeme Token]
endLayouts p l =
  case layoutState l of
    LayoutStarting -> virtual p TokLayoutStart : virtual p TokLayoutEnd : rest
    LayoutBlock _  -> virtual p TokLayoutEnd : rest
  where
  rest = map (const (virtual p TokLayoutEnd)) (layoutStack l) ++ [ virtual p TokEOF ]
  

startSplice :: Action LexState [Lexeme Token]
startSplice =
  do
    s <- getLexerState
    r <- matchRange
    case s of
      InObject l -> setLexerState (InSplice r 0 l)
      _          -> error "[bug] `startSplice` not in object"
    lexeme TokDollar

emit :: Token -> Action LexState [Lexeme Token]
emit t =
  do
    r <- matchRange
    txt <- matchText
    considerLayout 
      Lexeme {
        lexemeRange = r,
        lexemeToken = t,
        lexemeText = txt
      } 


lexer :: Text -> Text -> [Lexeme Token]
lexer file = lexerAt (startPos file)

lexerAt :: SourcePos -> Text -> [Lexeme Token]
lexerAt loc txt = $makeLexer cfg (initialInputAt loc txt)
  where
  -- dbg xs = trace (unlines [ show (Text.unpack (lexemeText l)) ++
  --          "\t" ++ show (lexemeToken l) |  l <- xs ]) xs

  cfg = LexerConfig { lexerInitialState = Normal initialLayout
                    , lexerStateMode = \s -> case s of
                                               Normal {} -> 0
                                               InObject {} -> object
                                               InObjectLineStart {} -> objectLineStart
                                               InSplice {} -> splice
                                               InComment {} -> comment
                    , lexerEOF = \s p ->
                        case s of
                          Normal l -> endLayouts p l
                          InObject l -> endLayouts p l
                          InObjectLineStart l -> endLayouts p l
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