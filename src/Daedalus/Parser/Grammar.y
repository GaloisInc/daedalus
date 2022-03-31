{
{-# Language ViewPatterns, OverloadedStrings #-}
module Daedalus.Parser.Grammar (moduleP) where

import Data.Word(Word8)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.ByteString(ByteString)

import Daedalus.SourceRange
import Daedalus.GUID

import Daedalus.AST
import Daedalus.Parser.Tokens
import Daedalus.Parser.Lexer
import Daedalus.Parser.Monad
}

%tokentype    { Lexeme Token }

%token
  BIGIDENT    { (isLexBigIdent    -> Just $$) }
  SMALLIDENT  { (isLexSmallIdent  -> Just $$) }
  BIGIDENTI   { (isLexBigIdentI   -> Just $$) }
  SMALLIDENTI { (isLexSmallIdentI -> Just $$) }
  SETIDNET    { (isLexSetIdent    -> Just $$) }
  SETIDNETI   { (isLexSetIdentI   -> Just $$) }
  BYTE        { (isLexByte        -> Just $$) }
  BYTES       { (isLexBytes       -> Just $$) }
  NUMBER      { (isLexNumber      -> Just $$) }

  '('         { Lexeme { lexemeRange = $$, lexemeToken = OpenParen } }
  ')'         { Lexeme { lexemeRange = $$, lexemeToken = CloseParen } }
  '{'         { Lexeme { lexemeRange = $$, lexemeToken = OpenBrace } }
  '}'         { Lexeme { lexemeRange = $$, lexemeToken = CloseBrace } }
  'v{'        { Lexeme { lexemeRange = $$, lexemeToken = VOpen } }
  'v;'        { Lexeme { lexemeRange = $$, lexemeToken = VSemi } }
  'v}'        { Lexeme { lexemeRange = $$, lexemeToken = VClose } }
  '{|'        { Lexeme { lexemeRange = $$, lexemeToken = OpenBraceBar } }
  '|}'        { Lexeme { lexemeRange = $$, lexemeToken = CloseBraceBar } }
  '['         { Lexeme { lexemeRange = $$, lexemeToken = OpenBracket } }
  ']'         { Lexeme { lexemeRange = $$, lexemeToken = CloseBracket } }
  '<'         { Lexeme { lexemeRange = $$, lexemeToken = OpenTri } }
  '>'         { Lexeme { lexemeRange = $$, lexemeToken = CloseTri } }

  '<='        { Lexeme { lexemeRange = $$, lexemeToken = TokLeq } }
  '>='        { Lexeme { lexemeRange = $$, lexemeToken = TokGeq } }
  ';'         { Lexeme { lexemeRange = $$, lexemeToken = Semi } }
  '.'         { Lexeme { lexemeRange = $$, lexemeToken = Dot } }
  '..'        { Lexeme { lexemeRange = $$, lexemeToken = DotDot } }
  ','         { Lexeme { lexemeRange = $$, lexemeToken = Comma } }
  '='         { Lexeme { lexemeRange = $$, lexemeToken = Equals } }
  '=='        { Lexeme { lexemeRange = $$, lexemeToken = DoubleEquals } }
  '!='        { Lexeme { lexemeRange = $$, lexemeToken = BangEquals } }
  ':'         { Lexeme { lexemeRange = $$, lexemeToken = Colon  } }
  '@'         { Lexeme { lexemeRange = $$, lexemeToken = AtSign } }
  '!'         { Lexeme { lexemeRange = $$, lexemeToken = Bang } }
  '^'         { Lexeme { lexemeRange = $$, lexemeToken = Hat } }
  '|'         { Lexeme { lexemeRange = $$, lexemeToken = Bar } }
  '<|'        { Lexeme { lexemeRange = $$, lexemeToken = LtBar } }
  '$'         { Lexeme { lexemeRange = $$, lexemeToken = Dollar } }
  '$$'        { Lexeme { lexemeRange = $$, lexemeToken = DollarDollar } }
  '+'         { Lexeme { lexemeRange = $$, lexemeToken = Plus } }
  '-'         { Lexeme { lexemeRange = $$, lexemeToken = Minus } }
  '*'         { Lexeme { lexemeRange = $$, lexemeToken = Star } }
  '/'         { Lexeme { lexemeRange = $$, lexemeToken = ForwardSlash } }
  '%'         { Lexeme { lexemeRange = $$, lexemeToken = Percent } }
  '#'         { Lexeme { lexemeRange = $$, lexemeToken = Hash } }
  '<#'        { Lexeme { lexemeRange = $$, lexemeToken = LeftHash } }
  '<<'        { Lexeme { lexemeRange = $$, lexemeToken = ShiftL } }
  '>>'        { Lexeme { lexemeRange = $$, lexemeToken = ShiftR } }
  '.&.'       { Lexeme { lexemeRange = $$, lexemeToken = DotAmpDot } }
  '.|.'       { Lexeme { lexemeRange = $$, lexemeToken = DotBarDot } }
  '.^.'       { Lexeme { lexemeRange = $$, lexemeToken = DotHatDot } }
  '&&'        { Lexeme { lexemeRange = $$, lexemeToken = AmpAmp } }
  '||'        { Lexeme { lexemeRange = $$, lexemeToken = BarBar } }
  '~'         { Lexeme { lexemeRange = $$, lexemeToken = BitwiseComplementT } }
  '->'        { Lexeme { lexemeRange = $$, lexemeToken = RightArrow } }
  '_'         { Lexeme { lexemeRange = $$, lexemeToken = Underscore } }

{- NOTE: If you add a new keyword, please update the production
   for 'label` also, as it is likely to be a valid union label. -}

  'def'       { Lexeme { lexemeRange = $$, lexemeToken = KWDef } }
  'bitdata'   { Lexeme { lexemeRange = $$, lexemeToken = KWBitData } }
  'where'     { Lexeme { lexemeRange = $$, lexemeToken = KWWhere } }

  'true'      { Lexeme { lexemeRange = $$, lexemeToken = KWTrue } }
  'false'     { Lexeme { lexemeRange = $$, lexemeToken = KWFalse } }
  'for'       { Lexeme { lexemeRange = $$, lexemeToken = KWFor } }
  'map'       { Lexeme { lexemeRange = $$, lexemeToken = KWMap } }
  'in'        { Lexeme { lexemeRange = $$, lexemeToken = KWIn } }
  'is'        { Lexeme { lexemeRange = $$, lexemeToken = KWIs } }
  'of'        { Lexeme { lexemeRange = $$, lexemeToken = KWOf } }
  'int'       { Lexeme { lexemeRange = $$, lexemeToken = KWInt } }
  'uint'      { Lexeme { lexemeRange = $$, lexemeToken = KWUInt } }
  '$uint'     { Lexeme { lexemeRange = $$, lexemeToken = KWDollarUInt } }
  'sint'      { Lexeme { lexemeRange = $$, lexemeToken = KWSInt } }
  'float'     { Lexeme { lexemeRange = $$, lexemeToken = KWFloat } }
  'double'    { Lexeme { lexemeRange = $$, lexemeToken = KWDouble } }
  'bool'      { Lexeme { lexemeRange = $$, lexemeToken = KWBool } }
  'maybe'     { Lexeme { lexemeRange = $$, lexemeToken = KWMaybe } }
  'stream'    { Lexeme { lexemeRange = $$, lexemeToken = KWStream } }
  'Choose'    { Lexeme { lexemeRange = $$, lexemeToken = KWChoose } }
  'Choose1'   { Lexeme { lexemeRange = $$, lexemeToken = KWChoose1 } }
  'First'     { Lexeme { lexemeRange = $$, lexemeToken = KWFirst } }
  'Accept'    { Lexeme { lexemeRange = $$, lexemeToken = KWAccept } }
  'block'     { Lexeme { lexemeRange = $$, lexemeToken = KWblock } }
  'let'       { Lexeme { lexemeRange = $$, lexemeToken = KWlet } }
  'Optional'  { Lexeme { lexemeRange = $$, lexemeToken = KWOptional } }
  'Optional?' { Lexeme { lexemeRange = $$, lexemeToken = KWOptionalQuestion } }
  'Many'      { Lexeme { lexemeRange = $$, lexemeToken = KWMany } }
  'Many?'     { Lexeme { lexemeRange = $$, lexemeToken = KWManyQuestion } }
  'Fail'      { Lexeme { lexemeRange = $$, lexemeToken = KWFail } }
  'UInt8'     { Lexeme { lexemeRange = $$, lexemeToken = KWUInt8 } }
  'Match'     { Lexeme { lexemeRange = $$, lexemeToken = KWMatch } }
  'Match1'    { Lexeme { lexemeRange = $$, lexemeToken = KWMatch1 } }
  'try'       { Lexeme { lexemeRange = $$, lexemeToken = KWTry } }
  'if'        { Lexeme { lexemeRange = $$, lexemeToken = KWIf } }
  'then'      { Lexeme { lexemeRange = $$, lexemeToken = KWThen } }
  'else'      { Lexeme { lexemeRange = $$, lexemeToken = KWElse } }
  'case'      { Lexeme { lexemeRange = $$, lexemeToken = KWCase } }
  'import'    { Lexeme { lexemeRange = $$, lexemeToken = KWImport } }
  'as'        { Lexeme { lexemeRange = $$, lexemeToken = KWAs } }
  'as!'       { Lexeme { lexemeRange = $$, lexemeToken = KWAsBang } }
  'as?'       { Lexeme { lexemeRange = $$, lexemeToken = KWAsQuestion } }
  'concat'    { Lexeme { lexemeRange = $$, lexemeToken = KWConcat } }
  'END'       { Lexeme { lexemeRange = $$, lexemeToken = KWEND } }
  'COMMIT'    { Lexeme { lexemeRange = $$, lexemeToken = KWCOMMIT } }
  'empty'     { Lexeme { lexemeRange = $$, lexemeToken = KWMapEmpty } }
  'Insert'    { Lexeme { lexemeRange = $$, lexemeToken = KWMapInsert } }
  'insert'    { Lexeme { lexemeRange = $$, lexemeToken = KWMapinsert } }
  'Lookup'    { Lexeme { lexemeRange = $$, lexemeToken = KWMapLookup } }
  'lookup'    { Lexeme { lexemeRange = $$, lexemeToken = KWMaplookup } }
  'Offset'    { Lexeme { lexemeRange = $$, lexemeToken = KWOffset } }
  'GetStream' { Lexeme { lexemeRange = $$, lexemeToken = KWGetStream } }
  'SetStream' { Lexeme { lexemeRange = $$, lexemeToken = KWSetStream } }
  'Take'      { Lexeme { lexemeRange = $$, lexemeToken = KWTake } }
  'Drop'      { Lexeme { lexemeRange = $$, lexemeToken = KWDrop } }
  'arrayStream' { Lexeme { lexemeRange = $$, lexemeToken = KWArrayStream } }
  'bytesOfStream' { Lexeme { lexemeRange = $$, lexemeToken = KWBytesOfStream} }
  'nothing'   { Lexeme { lexemeRange = $$, lexemeToken = KWNothing } }
  'just'      { Lexeme { lexemeRange = $$, lexemeToken = KWJust } }
  'length'    { Lexeme { lexemeRange = $$, lexemeToken = KWArrayLength } }
  'Index'     { Lexeme { lexemeRange = $$, lexemeToken = KWArrayIndex } }
  'rangeUp'   { Lexeme { lexemeRange = $$, lexemeToken = KWRangeUp } }
  'rangeDown' { Lexeme { lexemeRange = $$, lexemeToken = KWRangeDown } }

  'pi'             { Lexeme { lexemeRange = $$, lexemeToken = KWpi } }
  'wordToFloat'    { Lexeme { lexemeRange = $$, lexemeToken = KWWordToFloat } }
  'wordToDouble'   { Lexeme { lexemeRange = $$, lexemeToken = KWWordToDouble } }
  'isNaN'          { Lexeme { lexemeRange = $$, lexemeToken = KWIsNaN } }
  'isInfinite'     { Lexeme { lexemeRange = $$, lexemeToken = KWIsInfinite } }
  'isDenormalized' { Lexeme { lexemeRange = $$,
                              lexemeToken = KWIsDenormalized } }
  'isNegativeZero' { Lexeme { lexemeRange = $$,
                              lexemeToken = KWIsNegativeZero } }

%monad { Parser }
%lexer { nextToken } { Lexeme { lexemeToken = TokEOF } }

%name moduleP module

%nonassoc 'else'
%left '|' '<|'
%left '^' '@'
%left 'is'
%nonassoc '..'
%left ':' 'as' 'as!' 'as?'
%left '||'
%left '&&'
%left '.|.' '.^.'
%left '.&.'
%nonassoc '==' '!='
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%left '<<' '>>'
%left '#' '<#'
%left '~' '!'
%left '.'
-- High Precedence
%%

module ::                                     { ([Located ModuleName], [Decl]) }
  : imports decls                             { ($1, $2) }

imports ::                                    { [Located ModuleName] }
  : {- empty -}                               { [] }
  | imports import                            { $2 : $1 }

-- This is a bit ugly, we could allow any sort of module name, but the
-- lexer differentiates between upper- and lower-case initiated
-- identifiers
import ::                                     { Located ModuleName }
  : 'import' BIGIDENT                         { Located (fst $2) (snd $2) }
  | 'import' SMALLIDENT                       { Located (fst $2) (snd $2) }

decls ::                                      { [Decl] }
  : listOf(decl)                              { $1 }

decl                                       :: { Decl }
  : rule                                      { DeclRule $1 }
  | bitdata                                   { DeclBitData $1 }

rule                                     :: { Rule }
  : 'def' name listOf(ruleParam)
          optRetType optDef                 { mkRule $1 $2 $3 $4 $5 }

bitdata ::                                  { BitData }
  : 'bitdata' name 'where'
     'v{' bitdata_body 'v}'                 { BitData { bdName  = $2
                                                      , bdBody  = $5
                                                      , bdRange = $1 <-> $6 
                                                      } }

bitdata_body                             :: { BitDataBody }
  : separated1(bitdata_ctor, virtSep)       { BitDataUnion $1 }
  | separated(bitdata_field, virtSep)       { BitDataStruct $1 }


bitdata_ctor                             :: { BitDataCon }
  : label '=' bitdata_defn                  { ( $1, $3 ) }

bitdata_defn                             :: { [ Located BitDataField ] }
  : bitdata_tag                             { [ $1 ] }
  | '{'
    separated1(bitdata_field, commaOrSemi)
    '}'                                     { $2 }

bitdata_field                            :: { Located BitDataField }
  : bitdata_tag                             { $1 }
  | '_'   ':' type                          { loc ($1 <-> $3)
                                              (BDFWildcard (Just $3)) }
  | label ':' type                          { loc ($1 <-> $3)
                                              (BDFField (thingValue $1)
                                                        (Just $3)) }

bitdata_tag                              :: { Located BitDataField }
  : NUMBER                                  { loc (nRange $1)
                                                  (BDFLiteral (nValue $1)
                                                               (nType $1)) }
  | NUMBER ':' type                         { loc (nRange $1)
                                                  (BDFLiteral (nValue $1)
                                                              (Just $3)) }


optDef                                   :: { Maybe Expr }
  : '=' expr                                { Just $2 }
  | {- empty -}                             { Nothing }

optRetType                               :: { Maybe SrcType }
  : ':' type                                { Just $2 }
  | {- empty -}                             { Nothing }


ruleParam                                :: { RuleParam }
  : name                                      { RuleParam
                                                  { paramName = $1
                                                  , paramType = Nothing } }
  | '(' name ':' type ')'                    { RuleParam
                                                  { paramName = $2
                                                  , paramType = Just $4
                                                  } }

name                                     :: { Name }
  : BIGIDENT                                { mkName AGrammar $1 }
  | SMALLIDENT                              { mkName AValue $1 }
  | SETIDNET                                { mkName AClass $1 }
  | '$$'                                    { mkName AValue ($1,"$$") }

label                                    :: { Located Label }
  : BIGIDENT                                { mkLabel $1 }
  | SMALLIDENT                              { mkLabel $1 }
  | 'for'                                   { mkLabel ($1, "for") }
  | 'map'                                   { mkLabel ($1, "map") }
  | 'in'                                    { mkLabel ($1, "in") }
  | 'is'                                    { mkLabel ($1, "is") }
  | 'int'                                   { mkLabel ($1, "int") }
  | 'uint'                                  { mkLabel ($1, "uint") }
  | 'sint'                                  { mkLabel ($1, "sint") }
  | 'bool'                                  { mkLabel ($1, "bool") }
  | 'maybe'                                 { mkLabel ($1, "maybe") }
  | 'stream'                                { mkLabel ($1, "stream") }
  | 'Choose'                                { mkLabel ($1, "Choose") }
  | 'Choose1'                               { mkLabel ($1, "Choose1") }
  | 'Optional'                              { mkLabel ($1, "Optional") }
  | 'Many'                                  { mkLabel ($1, "Many") }
  | 'Match'                                 { mkLabel ($1, "Match") }
  | 'Match1'                                { mkLabel ($1, "Match1") }
  | 'UInt8'                                 { mkLabel ($1, "UInt8") }
  | 'if'                                    { mkLabel ($1, "if") }
  | 'then'                                  { mkLabel ($1, "then") }
  | 'else'                                  { mkLabel ($1, "else") }
  | 'case'                                  { mkLabel ($1, "case") }
  | 'import'                                { mkLabel ($1, "import") }
  | 'as'                                    { mkLabel ($1, "as") }
  | '$$'                                    { mkLabel ($1,"$$") }
  | 'END'                                   { mkLabel ($1,"END") }
  | 'empty'                                 { mkLabel ($1,"empty") }
  | 'Lookup'                                { mkLabel ($1,"Lookup") }
  | 'lookup'                                { mkLabel ($1,"lookup") }
  | 'Insert'                                { mkLabel ($1,"Insert") }
  | 'insert'                                { mkLabel ($1,"insert") }
  | 'Offset'                                { mkLabel ($1,"Offset") }
  | 'GetStream'                             { mkLabel ($1,"GetStream") }
  | 'SetStream'                             { mkLabel ($1,"SetStream") }
  | 'Take'                                  { mkLabel ($1,"Take") }
  | 'Drop'                                  { mkLabel ($1,"Drop") }
  | 'length'                                { mkLabel ($1,"length") }
  | 'Index'                                 { mkLabel ($1,"Index") }
  | 'rangeUp'                               { mkLabel ($1,"rangeUp") }
  | 'rangeDown'                             { mkLabel ($1,"rangeDown") }
  | 'try'                                   { mkLabel ($1,"try") }
  | 'arrayStream'                           { mkLabel ($1,"arrayStream") }
  | 'bytesOfStream'                         { mkLabel ($1,"bytesOfStream") }
  | 'Fail'                                  { mkLabel ($1,"Fail") }

expr                                     :: { Expr }
  : call_expr                               { $1 }
  | expr '+' expr                           { at ($1,$3) (EBinOp Add $1 $3) }
  | expr '-' expr                           { at ($1,$3) (EBinOp Sub $1 $3) }
  | expr '*' expr                           { at ($1,$3) (EBinOp Mul $1 $3) }
  | expr '/' expr                           { at ($1,$3) (EBinOp Div $1 $3) }
  | expr '%' expr                           { at ($1,$3) (EBinOp Mod $1 $3) }
  | expr '#' expr                           { at ($1,$3) (EBinOp Cat $1 $3) }
  | expr '<#' expr                          { at ($1,$3) (EBinOp LCat $1 $3) }
  | expr '<<' expr                          { at ($1,$3) (EBinOp LShift $1 $3) }
  | expr '>>' expr                          { at ($1,$3) (EBinOp RShift $1 $3) }


  | expr '.|.' expr                         { at ($1,$3)
                                                 (EBinOp BitwiseOr $1 $3) }
  | expr '.&.' expr                         { at ($1,$3)
                                                 (EBinOp BitwiseAnd $1 $3) }
  | expr '.^.' expr                         { at ($1,$3)
                                                 (EBinOp BitwiseXor $1 $3) }

  | expr '&&' expr                          { at ($1,$3)
                                                 (EBinOp LogicAnd $1 $3) }

  | expr '||' expr                          { at ($1,$3)
                                                 (EBinOp LogicOr $1 $3) }


  | expr '<' expr                           { at ($1,$3) (EBinOp Lt  $1 $3) }
  | expr '>' expr                           { at ($1,$3) (EBinOp Lt  $3 $1) }
  | expr '<=' expr                          { at ($1,$3) (EBinOp Leq $1 $3) }
  | expr '>=' expr                          { at ($1,$3) (EBinOp Leq $3 $1) }
  | expr '==' expr                          { at ($1,$3) (EBinOp Eq  $1 $3) }
  | expr '!=' expr                          { at ($1,$3) (EBinOp NotEq $1 $3) }

  | expr '|' expr                           { at ($1,$3)
                                                 (EChoiceU Backtrack $1 $3) }

  | expr '<|' expr                          { at ($1,$3)
                                                 (EChoiceU Commit $1 $3) }


  | expr '..' expr                          { at ($1,$3)
                                              (EInRange (Just $1) (Just  $3)) }
  | expr '..'                               { at ($1,$2)
                                              (EInRange (Just $1) Nothing) }
  | '..' expr                               { at ($1,$2)
                                              (EInRange Nothing (Just $2)) }

  | expr 'is' label                         { at ($1,$3)
                                                     (ESel $1 (SelUnion $3))}
  | expr 'is' 'true'                        { at ($1,$3) (ESel $1 SelTrue) }
  | expr 'is' 'false'                       { at ($1,$3) (ESel $1 SelFalse) }
  | expr 'is' 'just'                        { at ($1,$3) (ESel $1 SelJust) }
  | expr 'is' 'nothing'                     { at ($1,$3) (ESel $1 SelNothing) }


  | expr ':' type                           { at ($1,$3)
                                                 (EHasType MatchType $1 $3) }
  | expr 'as' type                          { at ($1,$3)
                                                 (EHasType CoerceSafe $1 $3) }
  | expr 'as!' type                         { at ($1,$3)
                                                 (EHasType CoerceForce $1 $3) }
  | expr 'as?' type                         { at ($1,$3)
                                                 (EHasType CoerceCheck $1 $3) }
  | '^' expr                                { at ($1,$2) (EPure $2) }
  | '@' expr                                { at ($1,$2) (EQuiet $2) }

  | 'if' expr 'then' expr 'else' expr       { at ($1,$6)(EIf $2 $4 $6)}
  | 'for' '(' name '=' expr ';' for_binder ')'
           expr               %prec 'else'  { at ($1,$9) (mkFor $3 $5 $7 $9) }
  | 'map' '(' for_binder ')'
           expr               %prec 'else'  { at ($1,$5) (mkForMap $3 $5) }



for_binder                               :: { ((Maybe Name, Name), Expr) }
  : for_name_bind 'in' expr                 { ($1, $3) }

for_name_bind                            :: { (Maybe Name, Name) }
  : name                                    { (Nothing, $1) }
  | name ',' name                           { (Just $1, $3) }


call_expr                                :: { Expr }
  : name listOf1(aexpr)                     { at ($1, last $2) (EApp $1 $2) }
  | 'just' aexpr                            { at ($1,$2) (EJust $2) }
  | 'concat' aexpr                          { at ($1,$2) (EUniOp Concat $2) }
  | 'Optional' aexpr                        { at ($1,$2) (EOptional Commit $2) }
  | 'Optional?' aexpr                       { at ($1,$2)
                                                 (EOptional Backtrack $2) }
  | 'Match' aexpr                           { at ($1,$2) (EMatch $2) }
  | 'Match1' aexpr                          { at ($1,$2) (EMatch1 $2) }
  | 'UInt8' aexpr                           { at ($1,$2) (EMatch1 $2) }
  | manyKW aexpr                            { mkMany $1 Nothing $2 }
  | manyKW aexpr aexpr                      { mkMany $1 (Just $2) $3 }

  | 'Fail' aexpr                            { at ($1,$2) (EFail $2) }

  | '-' aexpr                               { at ($1,$2) (EUniOp Neg $2) }
  | '!' aexpr                               { at ($1,$2) (EUniOp Not $2) }
  | '~' aexpr                               { at ($1,$2)
                                              (EUniOp BitwiseComplement $2) }

  | 'Lookup' aexpr aexpr                    { at ($1,$3) (EMapLookup $2 $3) }
  | 'lookup' aexpr aexpr                    { at ($1,$3) (EBinOp LookupMap $2 $3) }
  | 'Insert' aexpr aexpr aexpr              { at ($1,$4) (EMapInsert $2 $3 $4) }
  | 'insert' aexpr aexpr aexpr              { mkDoInsert $1 $2 $3 $4 }
  | 'SetStream' aexpr                       { at ($1,$2) (ESetStream $2) }
  | 'Take' aexpr aexpr                      { at ($1,$2) (EStreamLen $2 $3) }
  | 'Drop' aexpr aexpr                      { at ($1,$2) (EStreamOff $2 $3) }
  | 'length' aexpr                          { at ($1,$2) (EArrayLength $2)  }
  | 'Index' aexpr aexpr                     { at ($1,$2) (EArrayIndex $2 $3) }

  | 'rangeUp' aexpr                         { mkRngUp1 $1 $2 }
  | 'rangeUp' aexpr aexpr                   { mkRngUp2 $1 $2 $3 }
  | 'rangeUp' aexpr aexpr aexpr             { mkRngUp3 $1 $2 $3 $4 }
  | 'rangeDown' aexpr                       { mkRngDown1 $1 $2 }
  | 'rangeDown' aexpr aexpr                 { mkRngDown2 $1 $2 $3 }
  | 'rangeDown' aexpr aexpr aexpr           { mkRngDown3 $1 $2 $3 $4 }

  | 'try' aexpr                             { at ($1,$2) (ETry $2) }
  | 'arrayStream' aexpr         { at ($1,$2)(EBinOp ArrayStream
                                              (at ($1,$2) (ELiteral (LBytes "array")))
                                              $2) }
  | 'arrayStream' aexpr aexpr   { at ($1,$3)(EBinOp ArrayStream $2 $3)}
  | 'bytesOfStream' aexpr       { at ($1,$2) (EUniOp BytesOfStream $2) }

  | 'wordToFloat' aexpr         { at ($1,$2) (EUniOp WordToFloat $2) }
  | 'wordToDouble' aexpr        { at ($1,$2) (EUniOp WordToDouble $2) }
  | 'isNaN' aexpr               { at ($1,$2) (EUniOp IsNaN $2) }
  | 'isInfinite' aexpr          { at ($1,$2) (EUniOp IsInfinite $2) }
  | 'isDenormalized' aexpr      { at ($1,$2) (EUniOp IsDenormalized $2) }
  | 'isNegativeZero' aexpr      { at ($1,$2) (EUniOp IsNegativeZero $2) }



  | aexpr                                   { $1 }

aexpr                                    :: { Expr }
  : literal                                 { at (fst $1) (ELiteral (snd $1)) }
  | 'pi'                                    { at $1 (ELiteral LPi) }
  | 'UInt8'                                 { at $1      EAnyByte }
  | 'Accept'                                { mkAccept $1 }
  | '$uint' NUMBER                          {% mkUInt $1 $2 }

  | '$' '[' separated(expr, commaOrSemi) ']'{ at ($1,$4) (EMatch1
                                              (at ($2,$4) (EArray $3))) }

  | name                                    { at $1 (EVar $1) }
  | implicitParam                           { at $1 (EImplicit $1) }
  | 'END'                                   { at $1 EEnd }
  | 'empty'                                 { at $1 EMapEmpty }
  | 'nothing'                               { at $1 ENothing }
  | 'Offset'                                { at $1 EOffset }
  | 'GetStream'                             { at $1 ECurrentStream }

  | '(' expr ')'                            { $2 }
  | 'block' 'v{' separated(struct_field, virtSep) 'v}'
                                            { at ($1,$4) (EStruct $3) }
  | '{' separated(struct_field, commaOrSemi) '}'
                                            { at ($1,$3) (EStruct $2) }
  | '{|' label '=' expr '|}'                { at ($1,$5) (mkIn $2 $5 (Just $4))}
  | '{|' label '|}'                         { at ($1,$3) (mkIn $2 $3 Nothing) }

  | '[' separated(expr, commaOrSemi) ']'    { at ($1,$3) (EArray $2) }
  | chooseKW 'v{' separated(union_field, virtSep) 'v}'
                                            {% at ($1,$4) `fmap`
                                               mkUnion (thingValue $1) $3 }
  | chooseKW '{' separated(union_field, commaOrSemi) '}'
                                            {% at ($1,$4) `fmap`
                                               mkUnion (thingValue $1) $3 }

  | 'First' 'v{' separated(union_field, virtSep) 'v}'
                                            {% at ($1,$4) `fmap`
                                               mkUnion Commit $3 }



  | 'case' expr 'of' 'v{' separated(case_patterns, virtSep) 'v}'
                                            { at ($1,$6) (ECase $2 $5) } 

  | 'case' expr 'of' '{' separated(case_patterns, ';') '}'
                                            { at ($1,$6) (ECase $2 $5) } 

  | aexpr '.' label                         { at ($1,$3)
                                                 (ESel $1 (SelStruct $3))}

implicitParam                            :: { IPName }
  : SMALLIDENTI                             { mkIP AValue   $1 }
  | BIGIDENTI                               { mkIP AGrammar $1 }
  | SETIDNETI                               { mkIP AClass   $1 }

commaOrSemi                              :: { () }
  : ','                                     { () }
  | ';'                                     { () }

virtSep                                  :: { () }
  : ';'                                     { () }
  | 'v;'                                    { () }
  | virtSep 'v;'                            { () }

chooseKW                                 :: { Located Commit }
  : 'Choose'                                { loc $1 Backtrack }
  | 'Choose1'                               { loc $1 Commit }

manyKW                                   :: { Located Commit }
  : 'Many'                                  { loc $1 Commit }
  | 'Many?'                                 { loc $1 Backtrack }

struct_field                             :: { StructField Expr }
  : expr                                    { Anon $1 }
  | '@' name '=' expr                       { $2 :@= $4 }
  | 'let' name '=' expr                     { $2 :@= $4 }
  | 'let' implicitParam '=' expr            { $2 :?= $4 }
  | name '=' expr                           { $1 := $3 }
  | 'COMMIT'                                { COMMIT $1 }

union_field                              :: { Either Expr (UnionField Expr) }
  : expr                                    { Left $1 }
  | label '=' expr                          { Right ($1 :> $3) }

literal                                  :: { (SourceRange, Literal) }
  : NUMBER                                  { (nRange $1, mkNumLit $1) }
  | 'true'                                  { ($1, LBool True) }
  | 'false'                                 { ($1, LBool False) }
  | BYTES                                   { LBytes `fmap` $1 }
  | BYTE                                    { (nRange $1, mkByteLit $1) }

case_patterns                            :: { PatternCase Expr }
  :  '_' '->' expr                          { PatternDefault $3 }
  | separated(case_pattern, ',') '->' expr  { PatternCase $1 $3 }

case_pattern                             :: { Pattern }
  : literal                                 { LitPattern (uncurry loc $1) }
  | con_name                                { ConPattern $1
                                                (WildPattern (range $1)) }
  | con_name nested_pattern                 { ConPattern $1 $2 }

con_name                                 :: { Located Con }
  : 'nothing'                               { loc (range $1) ConNothing }
  | 'just'                                  { loc (range $1) ConJust }
  | label                                   { ConUser `fmap` $1 }

nested_pattern                           :: { Pattern }
  : name                                    { VarPattern $1 }
  | '_'                                     { WildPattern $1 }
  -- XXX: maybe add matching on structs?




separated(p,s)                           :: { [p] }
  : {- empty -}                             { [] }
  | separated1(p,s)                         { $1 }

separated1(p,s)                          :: { [p] }
  : p                                       { [$1] }
  | p s separated(p,s)                      { $1 : $3 }

type                                     :: { SrcType }
  : 'bool'                                  { atT $1 TBool }
  | 'float'                                 { atT $1 TFloat }
  | 'double'                                { atT $1 TDouble }
  | 'int'                                   { atT $1 TInteger }
  | 'uint' type                             { atT ($1 <-> $2) (TUInt $2) }
  | 'sint' type                             { atT ($1 <-> $2) (TSInt $2) }
  | 'maybe' type                            { atT ($1 <-> $2) (TMaybe $2) }
  | 'stream'                                { atT $1 TStream }
  | '(' type  ')'                           { $2 }
  | '[' arr_or_map ']'                      { atT ($1 <-> $3) $2 }
  | '{' '}'                                 { atT ($1 <-> $2) TUnit }
  | NUMBER                                  { atT (nRange $1)
                                                  (TNum (nValue $1)) }
  | name                                    { SrcCon $1 }
  | SMALLIDENTI                             { SrcVar (loc (fst $1) (snd $1)) }

arr_or_map                               :: { TypeF SrcType }
  : type                                    { TArray $1 }
  | type '->' type                          { TMap $1 $3 }


--------------------------------------------------------------------------------
sepBy1(s,p)                              :: { [p] }
  : p revSep(s,p)                           { $1 : reverse $2 }

sepBy(s,p)                               :: { [p] }
  : sepBy1(s,p)                             { $1 }
  | {- empty -}                             { [] }

revSep(s,p)                              :: { [p] }
  : revSep(s,p) s p                         { $3 : $1 }
  | {- empty -}                             { [] }

listOf1(p)                                :: { [p] }
  : p listOf(p)                              { $1 : $2 }

listOf(p)                                 :: { [p] }
  : revListOf(p)                             { reverse $1 }

revListOf(p)                              :: { [p] }
  : revListOf(p) p                           { $2 : $1 }
  | {- empty -}                              { [] }



{

type LexPattern a = Lexeme Token -> Maybe (SourceRange, a)

isLexBigIdent :: LexPattern Text
isLexBigIdent x = case lexemeToken x of
                    BigIdent -> Just (lexemeRange x, lexemeText x)
                    _     -> Nothing

isLexSmallIdent :: LexPattern Text
isLexSmallIdent x = case lexemeToken x of
                      SmallIdent -> Just (lexemeRange x, lexemeText x)
                      _     -> Nothing

isLexBigIdentI :: LexPattern Text
isLexBigIdentI x =
  case lexemeToken x of
    BigIdentI -> Just (lexemeRange x, lexemeText x)
    _         -> Nothing

isLexSmallIdentI :: LexPattern Text
isLexSmallIdentI x =
  case lexemeToken x of
    SmallIdentI -> Just (lexemeRange x, lexemeText x)
    _           -> Nothing



isLexSetIdent :: LexPattern Text
isLexSetIdent x = case lexemeToken x of
                    SetIdent -> Just (lexemeRange x, lexemeText x)
                    _        -> Nothing

isLexSetIdentI :: LexPattern Text
isLexSetIdentI x =
  case lexemeToken x of
    SetIdentI -> Just (lexemeRange x, lexemeText x)
    _         -> Nothing



isLexBytes :: LexPattern ByteString
isLexBytes x = case lexemeToken x of
                 Bytes v -> Just (lexemeRange x, v)
                 _      -> Nothing

data NumL a = NumL { nRange :: SourceRange
                   , nText :: Text
                   , nValue ::a
                   , nType :: Maybe SrcType
                   }

isLexNumber :: Lexeme Token -> Maybe (NumL Integer)
isLexNumber x =
  case lexemeToken x of
    Number v m_n ->
      let r   = lexemeRange x
          m_t = atT r . TUInt . atT r . TNum . fromIntegral <$> m_n
      in Just NumL { nText = lexemeText x
                   , nRange = r
                   , nValue = v
                   , nType = m_t
                   }
    _            -> Nothing

isLexByte :: Lexeme Token -> Maybe (NumL Word8)
isLexByte x = case lexemeToken x of
                Byte v -> Just NumL { nRange = lexemeRange x
                                    , nText = lexemeText x
                                    , nValue = v
                                    , nType = Nothing
                                    }
                _      -> Nothing



--------------------------------------------------------------------------------

class AtExpr t where
  at :: t -> ExprF Expr -> Expr


instance AtExpr SourceRange where
  at x e = Expr Located { thingRange = x, thingValue = e }

instance AtExpr Name where
  at x = at (range x)

instance AtExpr IPName where
  at x = at (range x)

instance (HasRange a, HasRange b) => AtExpr (a,b) where
  at (x,y) = at (x <-> y)

atT :: HasRange r => r -> TypeF SrcType -> SrcType
atT r t = SrcType Located { thingRange = range r, thingValue = t }

loc :: SourceRange -> a -> Located a
loc r v = Located { thingRange = r, thingValue = v }


mkUnion :: Commit -> [Either Expr (UnionField Expr)] -> Parser (ExprF Expr)
mkUnion cmt fs
  | null anon  = pure (EChoiceT cmt named)
  | otherwise  = case named of
                   [] -> case anon of
                           [] -> pure (EChoiceT cmt [])
                           _  -> pure (exprValue (chU anon))
                   (x :> _) : _ ->
                      parseError (sourceFrom (thingRange x))
                                 "Cannot mix tagged and untagged choices."

  where
  anon  = [ e | Left e  <- fs ]
  named = [ f | Right f <- fs ]
  chU es = case es of
             []     -> error "Bug"
             [e]    -> e
             e : es1 ->
                let res = chU es1
                in at (e,res) (EChoiceU cmt e res)

mkName :: Context ctx -> (SourceRange, Text) -> Name
mkName ctx x = Name { nameScopedIdent = Unknown (snd x)
                    , nameContext     = ctx
                    , nameRange       = fst x 
                    , nameID          = invalidGUID }

mkIP :: Context ctx -> (SourceRange, Text) -> IPName
mkIP ctx (r,t) = IPName { ipName = t, ipContext = ctx, ipRange = r }

mkLabel :: (SourceRange, Text) -> Located Label
mkLabel (r,t) = Located { thingRange = r, thingValue = t }

mkUInt :: SourceRange -> NumL Integer -> Parser Expr
mkUInt r1 n
  | nValue n == 8 = pure (at (r1,r2) EAnyByte)
  | otherwise = parseError (sourceFrom r2) "Only size 8 supported for now"
  where
  r2 = nRange n


mkMany :: Located Commit -> Maybe Expr -> Expr -> Expr
mkMany c mb e = at (c,e) (EMany (thingValue c) bnds e)
  where bnds = case mb of
                 Nothing -> Between Nothing Nothing
                 Just eb ->
                   case exprValue eb of
                     EInRange a b -> Between a b
                     _            -> Exactly eb


mkFor :: Name -> Expr -> ((Maybe Name,Name), Expr) -> Expr -> ExprF Expr
mkFor s s0 ((k,v),xs) e = EFor (FFold s s0) k v xs e

mkForMap :: ((Maybe Name,Name), Expr) -> Expr -> ExprF Expr
mkForMap ((k,v),xs) e = EFor FMap k v xs e

mkNumber :: Integer -> ExprF Expr
mkNumber x = ELiteral (LNumber x (Text.pack (show x)))

mkRngUp1 :: HasRange r => r -> Expr -> Expr
mkRngUp1 kw end = expr (ETriOp RangeUp (expr (mkNumber 0)) end (expr (mkNumber 1)))
  where expr = at (kw,end)

mkRngUp2 :: HasRange r => r -> Expr -> Expr -> Expr
mkRngUp2 kw start end = expr (ETriOp RangeUp start end (expr (mkNumber 1)))
  where expr = at (kw,end)

mkRngUp3 :: HasRange r => r -> Expr -> Expr -> Expr -> Expr
mkRngUp3 kw start end step = expr (ETriOp RangeUp start end step)
  where expr = at (kw,step)

mkDoInsert :: HasRange r => r -> Expr -> Expr -> Expr -> Expr
mkDoInsert kw k v m = expr (ETriOp MapDoInsert k v m)
  where expr = at (kw,m)

mkRngDown1 :: HasRange r => r -> Expr -> Expr
mkRngDown1 kw start = expr (ETriOp RangeDown start
                                             (expr (mkNumber 0))
                                             (expr (mkNumber 1)))
  where expr = at (kw,start)

mkRngDown2 :: HasRange r => r -> Expr -> Expr -> Expr
mkRngDown2 kw start end = expr (ETriOp RangeDown start end (expr (mkNumber 1)))
  where expr = at (kw,end)

mkRngDown3 :: HasRange r => r -> Expr -> Expr -> Expr -> Expr
mkRngDown3 kw start end step = expr (ETriOp RangeDown start end step)
  where expr = at (kw,step)

mkRule :: HasRange r =>
  r -> Name -> [RuleParam] -> Maybe SrcType -> Maybe Expr -> Rule
mkRule d nm ps t e = Rule { ruleName = nm,
                            ruleParams = ps,
                            ruleResTy = t,
                            ruleDef = e,
                            ruleRange = d <-> end
                          }
  where end = case e of
                Just e' -> range e'
                Nothing ->
                  case t of
                    Just t' -> range t'
                    Nothing ->
                      case ps of
                        _ : _ -> range (last ps)
                        [] -> range nm

mkIn :: Located Label -> SourceRange -> Maybe Expr -> ExprF Expr
mkIn l b mbE = EIn (l :> case mbE of
                           Just e  -> e
                           Nothing -> at (l,b) (EStruct []))

mkNumLit :: NumL Integer -> Literal
mkNumLit n = LNumber (nValue n) (nText n)

mkByteLit :: NumL Word8 -> Literal
mkByteLit n = LByte (nValue n) (nText n)

mkAccept :: SourceRange -> Expr
mkAccept r = at r (EHasType MatchType (at r (EStruct [])) t)
  where t = SrcType Located { thingRange = r, thingValue = TUnit }

}
