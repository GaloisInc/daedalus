
module Daedalus.LSP.SemanticTokens (semanticTokens) where

import           Control.Lens             hiding (op)
import           Daedalus.Parser.Lexer    (Lexeme (..), Token (..))
import           Data.Default
import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as Text

import qualified Language.LSP.Types       as J
import qualified Language.LSP.Types.Lens  as J
import qualified Language.LSP.Types.Lens  as JL

import           Daedalus.Panic
import           Daedalus.SourceRange

import           Daedalus.LSP.Diagnostics (sourceRangeToRange)
import           Daedalus.LSP.Monad

semanticTokens :: Maybe J.Range -> Maybe J.SemanticTokensClientCapabilities ->
                  ModuleState -> Maybe J.SemanticTokens
semanticTokens m_range m_caps ms = do
  toks <- passStatusToMaybe (ms ^. msTokens)
  let toks' = case m_range of
        Nothing -> toks
        Just r  -> filter (flip inRange r . sourceRangeToRange . range) toks

  let legend = case m_caps of
        Nothing -> def -- Just guess?
        Just caps -> J.SemanticTokensLegend (caps ^. J.tokenTypes)
                                            (caps ^. JL.tokenModifiers)

  case J.makeSemanticTokens legend (mapMaybe lexemeToSemanticToken toks') of
    Left err -> panic "makeSemanticTokens" [Text.unpack err]
    Right r  -> pure r

lexemeToSemanticToken :: Lexeme Token -> Maybe J.SemanticTokenAbsolute
lexemeToSemanticToken lexeme = uncurry (J.SemanticTokenAbsolute l c (fromIntegral len)) <$> tokenToSTT (lexemeToken lexeme)
  where
    r = range lexeme
    J.Position l c = sourceRangeToRange r ^. J.start
    len  = sourceIndex (sourceTo r) - sourceIndex (sourceFrom r) + 1 -- apparently we are one off otherwise

-- Probably we only care about
--  * SttVariable
--  * SttKeyword
--  * SttString
--  * SttNumber
--  * SttOperator

-- We might want to also look at the parsed module as some things
-- e.g. labels, are lexed as variables.
tokenToSTT :: Token -> Maybe (J.SemanticTokenTypes, [J.SemanticTokenModifiers])
tokenToSTT tok = flip (,) [] <$> typ
  where
    var = Just J.SttVariable
    num = Just J.SttNumber
    str = Just J.SttString
    op  = Just J.SttOperator
    kw  = Just J.SttKeyword

    typ = case tok of
      BigIdent    -> var
      SmallIdent  -> var
      SetIdent    -> var
      SmallIdentI -> var
      BigIdentI   -> var
      SetIdentI   -> var
      Number {}   -> num
      Bytes  {}   -> str
      Byte   {}   -> num

      -- OpenBrace
      -- CloseBrace
      -- OpenBraceBar
      -- CloseBraceBar
      -- OpenParen
      -- CloseParen
      -- OpenBracket
      -- CloseBracket
      -- OpenTri
      -- CloseTri

      -- VOpen | VSemi | VClose    -- inserted via layout

      -- Semi
      -- Colon
      -- Dot
      -- DotDot
      -- Comma

      AtSign       -> op
      Equals       -> op
      DoubleEquals -> op
      BangEquals   -> op
      Bang         -> op
      Hat          -> op
      Bar          -> op
      DotBarDot    -> op
      DotAmpDot    -> op
      DotHatDot    -> op
      BarBar       -> op
      AmpAmp       -> op
      LtBar        -> op
      DollarDollar -> var
      Plus         -> op
      Minus        -> op
      Star         -> op
      ForwardSlash -> op
      Percent      -> op
      TokLeq       -> op
      TokGeq       -> op
      Hash         -> op
      LeftHash     -> op
      ShiftL       -> op
      ShiftR       -> op
      RightArrow   -> op
      Underscore   -> var

      BitwiseAndT        -> op
      BitwiseOrT         -> op
      BitwiseXorT        -> op
      BitwiseComplementT -> op

      KWChoose            -> kw
      KWblock             -> kw
      KWlet               -> kw
      KWTry               -> kw
      KWMatch             -> kw
      KWMany              -> kw
      KWManyQuestion      -> kw
      KWOptional          -> kw
      KWOptionalQuestion  -> kw
      KWUInt8             -> kw
      KWTrue              -> kw
      KWFalse             -> kw
      KWFor               -> kw
      KWMap               -> kw
      KWIn                -> kw
      KWIs                -> kw
      KWOf                -> kw
      KWInt               -> kw
      KWUInt              -> kw
      KWSInt              -> kw
      KWBool              -> kw
      KWMaybe             -> kw
      KWStream            -> kw
      KWIf                -> kw
      KWThen              -> kw
      KWElse              -> kw
      KWImport            -> kw
      KWAs                -> kw
      KWAsBang            -> kw
      KWAsQuestion        -> kw
      KWConcat            -> kw
      KWEND               -> kw
      KWCOMMIT            -> kw
      KWMapEmpty          -> kw
      KWMapInsert         -> kw
      KWMapLookup         -> kw
      KWArrayLength       -> kw
      KWArrayIndex        -> kw
      KWRangeUp           -> kw
      KWRangeDown         -> kw
      KWOffset            -> kw
      KWGetStream         -> kw
      KWSetStream         -> kw
      KWTake              -> kw
      KWDrop              -> kw
      KWJust              -> kw
      KWNothing           -> kw
      KWDef               -> kw
      KWArrayStream       -> kw
      KWFail              -> kw
      KWCase              -> kw
      KWBitData           -> kw
      KWWhere             -> kw

      _ -> Nothing

-- | Returns true if r is inside (maybe partially) r'
inRange :: J.Range -> J.Range -> Bool
inRange r r' =
  r ^. J.start <= r' ^. J.end && r ^. J.end >= r' ^. J.start
