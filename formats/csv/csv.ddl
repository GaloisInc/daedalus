{-|
  Name:        CSV
  Description: Common Format for Comma-Separated Values (CSV)
               Based on: https://www.rfc-editor.org/info/rfc4180
               See specifically the ABNF grammar in Section 2.
  Maintainer:  Chris Phifer <cphifer@galois.com>
  Note:        There is no _formal_ specification for CSV, so as noted
               in the linked RFC, this represents the format that is
               "followed by most implementations".
-}

-- Complete CSV

def CSV =
  block
    header = Optional { $$ = Header; CRLF }
    data = build (emitArray (emit builder Record) (Many { CRLF; Record }))
    Optional CRLF

-- Headers

def Header = build (emitArray (emit builder Name) (Many { $comma; Name }))

def Name = Field

-- Data records

def Record = build (emitArray (emit builder Field) (Many { $comma; Field }))

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
