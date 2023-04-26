{- This is an example of how one might parse a simple
expression language with DaeDaLus -}


def Main = Expr

def Expr =
  many (lhs = Term)
    First
      Add = Binary lhs { Token "+"; Expr }
      Sub = Binary lhs { Token "-"; Expr }

def Term : Expr =
  many (lhs = Atom)
    First
      Mul = Binary lhs { Token "*"; Term }
      Div = Binary lhs { Token "/"; Term }
      Mod = Binary lhs { Token "%"; Term }

def Atom : Expr =
  First
    { Token "("; $$ = Expr; Token ")" }
    {| Var = Ident |}
    {| Num = Natural |}
    {| Num = { Token "-"; - Natural } |}


--------------------------------------------------------------------------------
-- Lexical considerations

def Ident =
  block
    WS
    let buf = many (buf = emit builder $alpha)
                   (emit buf $[ $alpha | $digit | '_' ])
    build buf

def Natural =
  block
    WS
    many (val = Digit)
      First
        { $['_']; val }
        (10 * val + Digit)

def Digit = $digit - '0' as int



def CommentStart = @Match "/*"

def Comment =
  block
    CommentStart
    let unterminated =
          many (depth = 1 : uint 64)
            block
              depth > 0 is true
              case Optional CommentStart of
                just -> depth + 1
                nothing ->
                  First
                    { Match "*/"; depth - 1 }
                    { $any; depth }

    unterminated > 0 is false <| Fail "Unterminated comment"

-- White space follow by some text
def Token x =
  block
    WS
    Match x

-- White space or comment
def WS = @Many (Comment <| @$ws)

def $ws    = ' ' | '\t' | '\n' | '\r'
def $alpha = 'a' .. 'z' | 'A' .. 'Z'
def $digit = '0' .. '9'



--------------------------------------------------------------------------------
def Binary (P : Expr) (Q : Expr) =
  block
    lhs = P
    rhs = Q


