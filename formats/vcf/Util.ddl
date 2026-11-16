import Daedalus

def $cr = '\r'
def $lf = '\n'
def $tab = '\t'
def $dq = 34  -- double-quote, defined like so due to #510
def $bs = '\\'

def $printable = ' ' .. '~'
def $alnum = ('a' .. 'z') | ('A' .. 'Z') | ('0' .. '9')
def $digit = '0' .. '9'


-- Parse an integer in base 10, failing if the number doesn't fit in 64 bits
def Number: uint 64 =
  block
    let d = ($digit - '0') as? uint 64
    many (s = d)
      10 * s + (($digit - '0') as? uint 64)


-- Parse zero or more occurrences of `P`, separated by `Sep`
def SepBy (Sep: ?a) (P: ?b): [?b] =
  SepBy1 Sep P <| []


-- Parse one or more occurrences of `P`, separated by `Sep`
def SepBy1 (Sep: ?a) (P: ?b): [?b] =
  ManyStart P { Sep; P }


-- Parse one or more occurrences of `P`
def Many1 (P: ?a): [?a] =
  Many (1 ..) P


-- Currently unused, but we could parse this specially if we need to refer to
-- the version information in order to support version-dependent parsing
def Fileformat =
  block
    Match "VCFv"
    maj = Number
    $['.']
    min = Number


-- Parse a string enclosed in double-quotes. The string may have double-quotes
-- within it, as long as they are escaped by backslashes.
def QuotedString: [uint 8] =
  block
    $dq
    let buf =
      many (s = { str = builder, end = false })
        if s.end
          then Fail "<internal>"
          else
            block
              let c = $printable
              case c of
                34 {- double-quote, see #510 -} -> { str = s.str, end = true }
                '\\' -> { str = Escape s.str, end = false }
                _  -> { str = emit s.str c, end = false }
    if buf.end
      then build buf.str
      else Fail "Unterminated string"


def Escape (s: builder (uint 8)): builder (uint 8) =
  case $any of
    '"'  -> emit s '"'
    '\\' -> emit s '\\'
    'n'  -> emit s '\n'
    'r'  -> emit s '\r'
    't'  -> emit s '\t'


def Bracketed $c: [uint 8] =
  block
    $['<']
    $$ = Many $[ $c - '>' ]
    $['>']
