" Daedalus syntax file
" Language:     daedalus
" Author:       Iavor S. Diatchki

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword ddlKeywordImport import

syn keyword ddlKeyword bitdata where
syn keyword ddlKeyword def
syn keyword ddlKeyword struct union
syn keyword ddlKeyword for map in many many?
syn keyword ddlKeyword if then else
syn keyword ddlKeyword is
syn keyword ddlKeyword of
syn keyword ddlKeyword as
syn match   ddlKeyword "\$\$"
syn keyword ddlKeyword Choose First
syn keyword ddlKeyword commit
syn keyword ddlKeyword case
syn keyword ddlKeyword block
syn keyword ddlKeyword let

syn keyword ddlKeywordFun   Optional Optional?
syn keyword ddlKeywordFun   Many Many?
syn keyword ddlKeywordFun   Fail Accept
syn keyword ddlKeywordFun   UInt8 $any Match END
syn keyword ddlKeywordFun   Offset SetStream GetStream take Take Drop

syn keyword ddlKeywordConst true false
syn keyword ddlKeywordConst nothing just
syn keyword ddlKeywordConst empty
syn keyword ddlKeywordFun   emit emitArray emitBuilder build
syn keyword ddlKeywordFun   arrayStream bytesOfStream
syn keyword ddlKeywordFun   insert Insert Lookup lookup
syn keyword ddlKeywordFun   Index length concat rangeUp rangeDown
syn keyword ddlKeywordFun   try
syn keyword ddlKeywordFun   pi wordToFloat wordToDouble
syn keyword ddlKeywordFun   isNaN isInfinite isDenormalized isNegativeZero

syn keyword ddlKeywordType int uint sint bool maybe stream float double builder

syn match ddlIdentBig    "\u\(\a\|\d\|_\)*"
syn match ddlIdentSmall  "\l\(\a\|\d\|_\)*"
syn match ddlIdentDollar "\$\(\a\|\d\|_\)\+"



syn match ddlOperator "\."
syn match ddlOperator "!"
syn match ddlOperator "\~"
syn match ddlOperator "\^"
syn match ddlOperator "|"
syn match ddlOperator "<"
syn match ddlOperator ">"
syn match ddlOperator "+"
syn match ddlOperator "/"
syn match ddlOperator "%"
syn match ddlOperator "-"
syn match ddlOperator "*"
syn match ddlOperator "#"
syn match ddlOperator "!="
syn match ddlOperator "<="
syn match ddlOperator ">="
syn match ddlOperator "&&"
syn match ddlOperator "||"
syn match ddlOperator "<|"
syn match ddlOperator "<#"
syn match ddlOperator "<<"
syn match ddlOperator ">>"
syn match ddlOperator "\.&\."
syn match ddlOperator "\.|\."
syn match ddlOperator "\.\^\."

syn match ddlDelimiter "@"
syn match ddlDelimiter ";"
syn match ddlDelimiter ","
syn match ddlDelimiter "\.\."
syn match ddlDelimiter ":"
syn match ddlDelimiter "("
syn match ddlDelimiter ")"
syn match ddlDelimiter "{"
syn match ddlDelimiter "}"
syn match ddlDelimiter "\["
syn match ddlDelimiter "\]"
syn match ddlDelimiter "{|"
syn match ddlDelimiter "{|"
syn match ddlDelimiter "|}"
syn match ddlDelimiter "="

" Needs to be after =
syn match ddlOperator "=="

syn match   ddlLineComment      "--.*$"
syn region  ddlBlockComment     start="{-" end="-}" contains=ddlBlockComment



syn match   ddlEsc contained "\\\""
syn match   ddlEsc contained "\\'"
syn match   ddlEsc contained "\\\\"
syn match   ddlEsc contained "\\n"
syn match   ddlEsc contained "\\t"
syn match   ddlEsc contained "\\r"
syn match   ddlEsc contained "\\\d\+"
syn match   ddlEsc contained "\\\(x\|X\)\x\+"
syn region  ddlString start="\"" skip="\\\"" end="\"" contains=ddlEsc
syn region  ddlByte   start="'"  skip="\\'"  end="'"  contains=ddlEsc


syn match   ddlNumber "\(0\(x\|X\|b\|B\|o\|O\)\x\+\)\|-\?\d\+"

hi def link ddlKeywordImport  Include
hi def link ddlKeyword        Structure

hi def link ddlKeywordFun     Keyword
hi def link ddlKeywordConst   Constant
hi def link ddlKeywordType    Type

hi def link ddlOperator       Operator
hi def link ddlDelimiter      Delimiter

hi def link ddlString         String
hi def link ddlByte           String
hi def link ddlEsc            Special

hi def link ddlNumber         Number

hi def link ddlLineComment  Comment
hi def link ddlBlockComment Comment

let b:current_syntax = "daedalus"
