{-|
  Name:        CSV
  Description: Common Format for Comma-Separated Values (CSV)
               Based on: https://www.rfc-editor.org/info/rfc4180
               See specifically the ABNF grammar in Section 2.
  Maintainer:  Chris Phifer <cphifer@galois.com>
  Note:        There is no _formal_ specification for CSV, so as noted
               in the linked RFC, this represents the format that is
               "followed by most implementations".

               In this implementation:

               - The first line is _always_ treated as a header
                 - Which implies every valid CSV has at least _two_
                   lines
               - CSVs where records don't have the proper number of
                 columns (as determined by the header) are rejected
-}

-- Complete CSV

def CSV =
  block
    header = { $$ = Header; CRLF }
    data = OneOrMoreSepBy (Record (length header)) CRLF
    Optional CRLF

-- Headers

def Header = OneOrMoreSepBy Name $comma

def Name = Field

-- Data records

def Record n = NSepBy n Field $comma

def Field = Escaped <| NonEscaped

def Escaped =
  block
    $dquote
    $$ = Many ($textdata <| $comma <| $cr <| $lf <| { $dquote; $dquote })
    $dquote

def NonEscaped = Many $textdata

-- Characters / character classes

def $comma    = ','
def $dquote   = '"'

def $cr       = '\r'
def $lf       = '\n'
def CRLF      = { $cr; $lf }

def $textdata = ' ' | '!' | '#' .. '+' | '-' .. '~'

-- Helpers

-- Parse one or more `P` separated by `Sep`, producing an array of the
-- results.
def OneOrMoreSepBy P Sep =
  build (emitArray (emit builder P) (Many { Sep; P }))

-- Parse `P` `n` times, separated by `Sep`, producing an array of the
-- results.
def NSepBy n P Sep =
  build (emitArray (emit builder P) (Many (n - 1) { Sep; P }))
