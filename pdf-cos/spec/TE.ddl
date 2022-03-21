import PdfValue
import PdfDecl
import Stdlib
def Catalog (r : Ref) =
  block
    let d = ResolveValRef r is dict
    pageTree = PageTreeRoot (LookupRef "Pages" d)

def PageTreeRoot (r : Ref) = PageTree nothing r

def PageTree (p : maybe Ref) (r : Ref) =
  block
    let node = ResolveValRef r is dict
    CheckParent p node
    let type = LookupResolve "Type" node is name
    if type == "Pages"
       then {| Node = map (child in (LookupResolve "Kids" node is array))
                          (PageTree (just r) (child is ref))
            |}

       else if type == "Page"
              then {| Leaf = Page node |}
              else Fail "Unexpected `Type` in page tree"

def CheckParent (p : maybe Ref) (d : Dict) =
  case Optional (Lookup "Parent" d) of
    nothing -> p is nothing
    just v  -> Guard (p == just (v is ref))
  <| Fail "Malformed node parent"


def Page (pageNode : Dict) =
  case Optional (Lookup "Contents" pageNode) of
    nothing -> {| EmptyPage = Accept |}
    just vr -> {| ContentStreams = PageContnet vr |}

def PageContnet (vr : Value) =
  case vr of
    ref r    -> [ContentStreamBytes r]
    array xs -> map (x in xs) (ContentStreamBytes (x is ref))

def ContentStreamBytes (r : Ref) : stream = (ResolveStreamRef r).body is ok




