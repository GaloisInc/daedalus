import PdfValue
import PdfDecl
import Stdlib

{-
def ExtractCatalogText (r : Ref) =
  block
    let cat = ResolveValRef r is dict
    CheckType "Catalog" cat
    ExtractRootText1 (LookupRef "Pages" cat)

def ExtractRootText1 (r : Ref) =
  concat (
    map (cs in PageTreeContentStreams (PageTreeP r))
        (ExtractContentStreamText cs)
  )

-}

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
              then {| Leaf = node |}
              else Fail "Unexpected `Type` in page tree"

def CheckParent (p : maybe Ref) (d : Dict) =
  case Optional (Lookup "Parent" d) of
    nothing -> p is nothing
    just v  -> Guard (p == just (v is ref))
  <| Fail "Malformed node parent"
