import PdfValue
import PdfDecl
import Stdlib


def PdfCatalog (r : Ref) =
  block
    let d = ResolveValRef r is dict
    pageTree = PdfPageTreeRoot (LookupRef "Pages" d)
    -- other fields omitted

def PdfPageTreeRoot (r : Ref) = PdfPageTree nothing empty r

def PdfPageTree (p : maybe Ref) (pResources : Dict) (r : Ref) =
  block
    let node = ResolveValRef r is dict
    PdfCheckParent p node
    let resources = case Optional (Lookup "Resources" node) of
                      just v  -> ResolveVal v is dict
                      nothing -> pResources
    let type = LookupResolve "Type" node is name
    if type == "Pages"
       then {| Node = map (child in (LookupResolve "Kids" node is array))
                          (PdfPageTree (just r) resources (child is ref))
            |}

       else if type == "Page"
              then {| Leaf = PdfPage resources node |}
              else Fail "Unexpected `Type` in page tree"

def PdfCheckParent (p : maybe Ref) (d : Dict) =
  case Optional (Lookup "Parent" d) of
    nothing -> p is nothing
    just v  -> Guard (p == just (v is ref))
  <| Fail "Malformed node parent"


def PdfPage (resources : Dict) (pageNode : Dict) =
  case Optional (Lookup "Contents" pageNode) of
    nothing -> {| EmptyPage = Accept |}
    just vr -> {| ContentStreams = PdfPageContent resources vr |}

def PdfPageContent (resources : Dict) (vr : Value) =
  block
    resources = resources
    SetStream
      case vr of
        ref r    -> ContentStreamBytes r

        -- inefficient!
        array xs ->
          block
            let chunks =
                  map (x in xs)
                    block
                    let bs = bytesOfStream (ContentStreamBytes (x is ref))
                    concat [bs, " "] -- yikes.

            arrayStream (concat chunks)
    values =
      block
        ManyWS
        $$ = Many ContentStreamEntry
        END

def ContentStreamBytes (r : Ref) : stream = (ResolveStreamRef r).body is ok

def ContentStreamEntry =
  First
    operator = Many (1..) (Token $['a'..'z','A'..'Z',"*'",'"'])
    value    = Value


