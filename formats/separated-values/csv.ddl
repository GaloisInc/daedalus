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
               - Both CRLF and LF are accepted as the separator
                 between header/records; this extra permissiveness is
                 common in other CSV parser implementations
-}

import SeparatedValues

def CSV = { let ?$sep = ','; $$ = SV; }

def Main = { $$ = CSV; END }
