{-|
  Name:        Separated Values
  Description: Common code for "separated values" specifications (e.g.
               CSV, TSV).
               Based on: https://www.rfc-editor.org/info/rfc4180
                         https://www.loc.gov/preservation/digital/formats/fdd/fdd000323.shtml
                         https://super-csv.github.io/super-csv/csv_specification.html
                         https://www.creativyst.com/Doc/Articles/CSV/CSV01.shtml
               Most notably, the facilities in this specification are
               suitable for many of the variations given in the second
               link's "Notes" section.
  Maintainer:  Chris Phifer <cphifer@galois.com>
  Note:        There is no _formal_ specification for CSV or the other
               formats supported by the infrastructure in this
               specification.
               Where possible, we use implicit parameters that allow
               one to 'customize' a parser for a separated-values
               format based on specific needs, such as:

               - Field separators other than ','
               - Inclusion of a header
-}

import Daedalus
import utf8

-- Common instantiations

def CSV_header = { let ?sep = ','; let ?includeHeader = true; SV; }
def CSV_no_header = { let ?sep = ','; let ?includeHeader = false; SV; }

def TSV_header = { let ?sep = '\t'; let ?includeHeader = true; SV; }
def TSV_no_header = { let ?sep = '\t'; let ?includeHeader = false; SV; }

-- Separated values

def SV =
  block
    Optional BOM
    header =
      if ?includeHeader
        then just { $$ = Header; Newline }
        else nothing
    data = Records

-- UTF-8 Byte order mark (BOM)

def BOM = @Match "\xEF\xBB\xBF"

-- Headers

def Header = OneOrMoreSepBy Name $[?sep]

def Name = Field

-- Data records

def Records =
  block
    let init = many (buf = builder) (emit buf { $$ = Record; Newline })
    let last = just { LookAhead UInt8; $$ = Record } <| nothing
    ^ build case last of
       just last -> emit init last
       nothing -> init

def Record = OneOrMoreSepBy Field $[?sep]

def Field = Escaped <| NonEscaped

def Escaped =
  block
    $dquote
    $$ = build (many (s = builder) (Textdata s <| emit s $[?sep] <| emit s $cr <| emit s $lf <| { emit s $dquote; emit s $dquote }))
    $dquote

def NonEscaped = build (many (s = builder) Textdata s)

-- Characters / character classes

def $dquote   = '"'

def $cr       = '\r'
def $lf       = '\n'
def CRLF      = { $cr; $lf }
def Newline   = CRLF <| $cr <| $lf

-- All printable characters except the separator and double-quote
def Textdata s =
  block
    let c = UTF8
    if (c < 32)                    {- ASCII control -}
       || (c == 34)                {- " -}
       || (c == (?sep as uint 32)) {- separator -}
      then Fail "Character is not valid in an non-escaped field"
      else EmitUTF8 s c

-- Helpers

-- Parse one or more `P` separated by `Sep`, producing an array of the
-- results.
def OneOrMoreSepBy P Sep = ManyStart P {Sep; P}
