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

-- Characters / character classes

def $comma    = ','
def $dquote   = '"'

def $cr       = '\r'
def $lf       = '\n'
def CRLF      = { $cr; $lf }

def $textdata = ' ' | '!' | '#' .. '+' | '-' .. '~'
