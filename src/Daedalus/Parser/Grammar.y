{
{-# Language ViewPatterns, OverloadedStrings #-}
module Daedalus.Parser.Grammar (moduleP) where

import Data.Word(Word8)
import Data.Text(Text)
import Data.ByteString(ByteString)

import Daedalus.SourceRange

import Daedalus.AST
import Daedalus.Parser.Tokens
import Daedalus.Parser.Lexer
import Daedalus.Parser.Monad
}

%tokentype    { Lexeme Token }

%token
  BIGIDENT    { (isLexBigIdent   -> Just $$) }
  SMALLIDENT  { (isLexSmallIdent -> Just $$) }
  SETIDNET    { (isLexSetIdent   -> Just $$) }
  BYTE        { (isLexByte       -> Just $$) }
  BYTES       { (isLexBytes      -> Just $$) }
  NUMBER      { (isLexNumber     -> Just $$) }

  '('         { Lexeme { lexemeRange = $$, lexemeToken = OpenParen } }
  ')'         { Lexeme { lexemeRange = $$, lexemeToken = CloseParen } }
  '{'         { Lexeme { lexemeRange = $$, lexemeToken = OpenBrace } }
  '}'         { Lexeme { lexemeRange = $$, lexemeToken = CloseBrace } }
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
  'true'      { Lexeme { lexemeRange = $$, lexemeToken = KWTrue } }
  'false'     { Lexeme { lexemeRange = $$, lexemeToken = KWFalse } }
  'for'       { Lexeme { lexemeRange = $$, lexemeToken = KWFor } }
  'map'       { Lexeme { lexemeRange = $$, lexemeToken = KWMap } }
  'in'        { Lexeme { lexemeRange = $$, lexemeToken = KWIn } }
  'is'        { Lexeme { lexemeRange = $$, lexemeToken = KWIs } }
  'int'       { Lexeme { lexemeRange = $$, lexemeToken = KWInt } }
  'uint'      { Lexeme { lexemeRange = $$, lexemeToken = KWUInt } }
  '$uint'     { Lexeme { lexemeRange = $$, lexemeToken = KWDollarUInt } }
  'sint'      { Lexeme { lexemeRange = $$, lexemeToken = KWSInt } }
  'bool'      { Lexeme { lexemeRange = $$, lexemeToken = KWBool } }
  'maybe'     { Lexeme { lexemeRange = $$, lexemeToken = KWMaybe } }
  'stream'    { Lexeme { lexemeRange = $$, lexemeToken = KWStream } }
  'Choose'    { Lexeme { lexemeRange = $$, lexemeToken = KWChoose } }
  'Choose1'   { Lexeme { lexemeRange = $$, lexemeToken = KWChoose1 } }
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
  'concat'    { Lexeme { lexemeRange = $$, lexemeToken = KWConcat } }
  'END'       { Lexeme { lexemeRange = $$, lexemeToken = KWEND } }
  'COMMIT'    { Lexeme { lexemeRange = $$, lexemeToken = KWCOMMIT } }
  'empty'     { Lexeme { lexemeRange = $$, lexemeToken = KWMapEmpty } }
  'Insert'    { Lexeme { lexemeRange = $$, lexemeToken = KWMapInsert } }
  'Lookup'    { Lexeme { lexemeRange = $$, lexemeToken = KWMapLookup } }
  'Offset'    { Lexeme { lexemeRange = $$, lexemeToken = KWOffset } }
  'GetStream' { Lexeme { lexemeRange = $$, lexemeToken = KWGetStream } }
  'SetStream' { Lexeme { lexemeRange = $$, lexemeToken = KWSetStream } }
  'Take'      { Lexeme { lexemeRange = $$, lexemeToken = KWTake } }
  'Drop'      { Lexeme { lexemeRange = $$, lexemeToken = KWDrop } }
  'arrayStream' { Lexeme { lexemeRange = $$, lexemeToken = KWArrayStream } }
  'nothing'   { Lexeme { lexemeRange = $$, lexemeToken = KWNothing } }
  'just'      { Lexeme { lexemeRange = $$, lexemeToken = KWJust } }
  'length'    { Lexeme { lexemeRange = $$, lexemeToken = KWArrayLength } }
  'Index'     { Lexeme { lexemeRange = $$, lexemeToken = KWArrayIndex } }
  'rangeUp'   { Lexeme { lexemeRange = $$, lexemeToken = KWRangeUp } }
  'rangeDown' { Lexeme { lexemeRange = $$, lexemeToken = KWRangeDown } }

%monad { Parser }
%lexer { nextToken } { Lexeme { lexemeToken = TokEOF } }

%name moduleP module

%nonassoc 'else'
%left '|' '<|'
%left '^' '@'
%left 'is'
%nonassoc '..'
%left ':' 'as' 'as!'
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

module ::                                     { ([Located ModuleName], [Rule]) }
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

decls ::                                      { [Rule] }
  : listOf(decl)                              { $1 }

decl                                       :: { Rule }
  : 'def' name listOf(ruleParam)
    optRetType optDef                         { mkRule $1 $2 $3 $4 $5 }

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
  | 'Insert'                                { mkLabel ($1,"Insert") }
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
                                                 (EHasType CoerceCheck $1 $3) }
  | expr 'as!' type                         { at ($1,$3)
                                                 (EHasType CoerceForce $1 $3) }
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
  | manyKW aexpr                            { mkMany $1 Nothing $2 }
  | manyKW aexpr aexpr                      { mkMany $1 (Just $2) $3 }

  | 'Fail' aexpr                            { at ($1,$2) (EFail $2) }

  | '!' aexpr                               { at ($1,$2) (EUniOp Not $2) }
  | '~' aexpr                               { at ($1,$2)
                                              (EUniOp BitwiseComplement $2) }

  | 'Lookup' aexpr aexpr                    { at ($1,$3) (EMapLookup $2 $3) }
  | 'Insert' aexpr aexpr aexpr              { at ($1,$4) (EMapInsert $2 $3 $4) }
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



  | aexpr                                   { $1 }

aexpr                                    :: { Expr }
  : literal                                 { at (fst $1) (ELiteral (snd $1)) }
  | 'UInt8'                                 { at $1      EAnyByte }
  | '$uint' NUMBER                          {% mkUInt $1 $2 }
  | name                                    { at $1 (EVar $1) }
  | 'END'                                   { at $1 EEnd }
  | 'empty'                                 { at $1 EMapEmpty }
  | 'nothing'                               { at $1 ENothing }
  | 'Offset'                                { at $1 EOffset }
  | 'GetStream'                             { at $1 ECurrentStream }

  | '(' expr ')'                            { $2 }
  | '{' separated(struct_field, commaOrSemi) '}' 
                                            { at ($1,$3) (EStruct $2) }
  | '{|' label '=' expr '|}'                { at ($1,$3) (EIn ($2 :> $4)) }
  | '[' separated(expr, commaOrSemi) ']'    { at ($1,$3) (EArray $2) }
  | chooseKW '{' separated(union_field, commaOrSemi) '}' 
                                            {% at ($1,$4) `fmap`
                                               mkUnion (thingValue $1) $3 }
  | 'case' expr 'is' '{' separated(case_patterns, ';') '}'
                                            { at ($1,$6) (ECase $2 $5) } 

  | aexpr '.' label                         { at ($1,$3)
                                                 (ESel $1 (SelStruct $3))}

commaOrSemi                              :: { () }
  : ','                                     { () }
  | ';'                                     { () }

chooseKW                                 :: { Located Commit }
  : 'Choose'                                { loc $1 Backtrack }
  | 'Choose1'                               { loc $1 Commit }

manyKW                                   :: { Located Commit }
  : 'Many'                                  { loc $1 Commit }
  | 'Many?'                                 { loc $1 Backtrack }

struct_field                             :: { StructField Expr }
  : expr                                    { Anon $1 }
  | '@' name '=' expr                       { $2 :@= $4 }
  | name '=' expr                           { $1 := $3 }
  | 'COMMIT'                                { COMMIT $1 }

union_field                              :: { Either Expr (UnionField Expr) }
  : expr                                    { Left $1 }
  | label '=' expr                          { Right ($1 :> $3) }

literal                                  :: { (SourceRange, Literal) }
  : NUMBER                                  { LNumber `fmap` $1 }
  | 'true'                                  { ($1, LBool True) }
  | 'false'                                 { ($1, LBool False) }
  | BYTES                                   { LBytes `fmap` $1 }
  | BYTE                                    { LByte  `fmap` $1 }

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
  | p                                       { [$1] }
  | p s separated(p,s)                      { $1 : $3 }

type                                     :: { SrcType }
  : 'bool'                                  { atT $1 TBool }
  | 'int'                                   { atT $1 TInteger }
  | 'uint' type                             { atT ($1 <-> $2) (TUInt $2) }
  | 'sint' type                             { atT ($1 <-> $2) (TSInt $2) }
  | 'maybe' type                            { atT ($1 <-> $2) (TMaybe $2) }
  | 'stream'                                { atT $1 TStream }
  | '(' type  ')'                           { $2 }
  | '[' arr_or_map ']'                      { atT ($1 <-> $3) $2 }
  | '{' '}'                                 { atT ($1 <-> $2) TUnit }
  | NUMBER                                  { atT (fst $1) (TNum (snd $1)) }
  | name                                    { SrcVar $1 }

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

isLexSetIdent :: LexPattern Text
isLexSetIdent x = case lexemeToken x of
                    SetIdent -> Just (lexemeRange x, lexemeText x)
                    _        -> Nothing

isLexByte :: LexPattern Word8
isLexByte x = case lexemeToken x of
                Byte v -> Just (lexemeRange x, v)
                _      -> Nothing


isLexBytes :: LexPattern ByteString
isLexBytes x = case lexemeToken x of
                 Bytes v -> Just (lexemeRange x, v)
                 _      -> Nothing

isLexNumber :: LexPattern Integer
isLexNumber x = case lexemeToken x of
                  Number v -> Just (lexemeRange x, v)
                  _        -> Nothing

--------------------------------------------------------------------------------

class AtExpr t where
  at :: t -> ExprF Expr -> Expr


instance AtExpr SourceRange where
  at x e = Expr Located { thingRange = x, thingValue = e }

instance AtExpr Name where
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
mkName ctx x = Name { nameScope = Unknown (snd x)
                    , nameContext = ctx
                    , nameRange = fst x }

mkLabel :: (SourceRange, Text) -> Located Label
mkLabel (r,t) = Located { thingRange = r, thingValue = t }

mkUInt :: SourceRange -> (SourceRange,Integer) -> Parser Expr
mkUInt r1 (r2,n)
  | n == 8 = pure (at (r1,r2) EAnyByte)
  | otherwise = parseError (sourceFrom r2) "Only size 8 supported for now"


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
mkNumber = ELiteral . LNumber

mkRngUp1 :: HasRange r => r -> Expr -> Expr
mkRngUp1 kw end = expr (ETriOp RangeUp (expr (mkNumber 0)) end (expr (mkNumber 1)))
  where expr = at (kw,end)

mkRngUp2 :: HasRange r => r -> Expr -> Expr -> Expr
mkRngUp2 kw start end = expr (ETriOp RangeUp start end (expr (mkNumber 1)))
  where expr = at (kw,end)

mkRngUp3 :: HasRange r => r -> Expr -> Expr -> Expr -> Expr
mkRngUp3 kw start end step = expr (ETriOp RangeUp start end step)
  where expr = at (kw,step)

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

}
