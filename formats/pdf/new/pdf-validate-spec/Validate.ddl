import PdfValue
import PdfDecl

--------------------------------------------------------------------------------
-- Simple "validation" we look for JavaScript or URI actions

-- Return `true` if this declaration is "safe"
-- ENTRY
def CheckRef (r : Ref) : bool =
  case ResolveDeclRef r of
    stream  -> true
    value v ->
      First
        block
          FindUnsafe v
          false
        true


def FindUnsafe (v : Value) =
  case v of
    dict d ->
      First
        block
          let act = LookupName "S" d
          (act == "JavaScript" || act == "URI") is true
        @Lookup "JS" d

    array xs ->
      block
        let ?vals = xs
        AnyUnsafe 0

    _        -> Fail "not unsafe"


def AnyUnsafe i =
  block
    let v = Index ?vals i
    FindUnsafe v <| AnyUnsafe (i+1)

