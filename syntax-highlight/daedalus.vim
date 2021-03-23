" Daedalus syntax file
" Language:     daedalus
" Author:       Iavor S. Diatchki

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword ddlKeywordImport import

syn keyword ddlKeyword def
syn keyword ddlKeyword for map in
syn keyword ddlKeyword if then else
syn keyword ddlKeyword is
syn keyword ddlKeyword of
syn keyword ddlKeyword as
syn match   ddlKeyword "\$\$"
syn keyword ddlKeyword Choose Choose1
syn keyword ddlKeyword commit
syn keyword ddlKeyword case
syn keyword ddlKeyword block
syn keyword ddlKeyword let

syn keyword ddlKeywordFun   Optional Optional?
syn keyword ddlKeywordFun   Many Many?
syn keyword ddlKeywordFun   Fail
syn keyword ddlKeywordFun   UInt8 $uint Match Match1 END
syn keyword ddlKeywordFun   Offset SetStream GetStream Take Drop

syn keyword ddlKeywordConst true false
syn keyword ddlKeywordConst nothing just
syn keyword ddlKeywordConst empty
syn keyword ddlKeywordFun   arrayStream
syn keyword ddlKeywordFun   Insert Lookup
syn keyword ddlKeywordFun   Index length concat rangeUp rangeDown
syn keyword ddlKeywordFun   try

syn keyword ddlKeywordType int uint sint bool maybe stream

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


syn match   ddlNumber "\(0\(x\|X\)\x\+\)\|-\?\d\+"

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
