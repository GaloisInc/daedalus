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

def SV =
  block
    Optional BOM
    header =
      if ?includeHeader
        then just { $$ = Header; Newline }
        else nothing
    data = Records

-- UTF-8 Byte order mark (BOM)

def BOM = @{ $[ 0xEF ]; $[ 0xBB ]; $[ 0xBF ] }

-- Headers

def Header = OneOrMoreSepBy Name ?$sep

def Name = Field

-- Data records

def Records =
  block
    let init = many (buf = builder) (emit buf { $$ = Record; Newline })
    let last = just { LookAhead UInt8; $$ = Record } <| nothing
    ^ build case last of
       just last -> emit init last
       nothing -> init

def Record = OneOrMoreSepBy Field ?$sep

def Field = Escaped <| NonEscaped

def Escaped =
  block
    $dquote
    $$ = Many ($textdata <| ?$sep <| $cr <| $lf <| { $dquote; $dquote })
    $dquote

def NonEscaped = Many $textdata

-- Characters / character classes

def $dquote   = '"'

def $cr       = '\r'
def $lf       = '\n'
def CRLF      = { $cr; $lf }
def Newline   = CRLF <| $cr <| $lf

-- All printable characters except the separator and double-quote
def $textdata = (' ' | '!' | '#' .. '~') - ?$sep

-- Helpers

-- Parse one or more `P` separated by `Sep`, producing an array of the
-- results.
def OneOrMoreSepBy P Sep = ManyStart P {Sep; P}
