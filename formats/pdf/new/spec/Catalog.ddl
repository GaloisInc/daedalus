import Debug
import Daedalus
import PdfValue
import PdfDecl
import StandardEncodings
import ContentStream
import Fonts

--------------------------------------------------------------------------------
-- Section 7.7.2, Table 29

-- ENTRY
def PdfCatalog (strict : bool) (enc : maybe StdEncodings) (r : Ref) =
  block
    let ?strict = strict
    let ?doText  = enc
    let d        = ResolveValRef r is dict
    pageTree     = PdfPageTreeRoot (LookupRef "Pages" d)
    stdEncodings = case enc of
                     nothing -> noStdEncodings
                     just e  -> e
    -- other fields omitted


--------------------------------------------------------------------------------
-- Page Tree; Section 7.7.3

def PdfPageTreeRoot (r : Ref) = PdfPageTree nothing noResources r

def PdfPageTree (p : maybe Ref) (parentResources : Resources) (r : Ref) =
  block
    let node = ResolveValRef r is dict
    PdfCheckParent p node

    {- Since resources are shared, we want to process them once at the
    node and pass the processed resources down the tree, otherwise
    we are going to reprocess them again for each leaf which is a lot
    of repeated work in large doucuments. -}
    let resources = case lookup "Resources" node of
                      just v  -> Resources v <| parentResources
                                  -- Since font parsing is incomplete,
                                  -- we ignore resources that we couldn't
                                  -- parse, for the time being
                      nothing -> parentResources
    let type = LookupResolve "Type" node is name
    if type == "Pages"
       then {| Node = map (child in (LookupResolve "Kids" node is array))
                          (PdfPageTree (just r) resources (child is ref))
            |}

       else if type == "Page"
              then {| Leaf = PdfPage resources node |}
              else Fail "Unexpected `Type` in page tree"

def noResources : Resources =
  block
    fonts = empty

def Resources (v : Value) =
  block
    fonts = GetFonts (ResolveVal v is dict)
    -- others resources omitted
    -- we need the fonts because they determine the character encoding to use

def PdfCheckParent (p : maybe Ref) (d : Dict) =
  First

    case lookup "Parent" d of
      nothing -> p is nothing
      just v  -> p == just (v is ref) is true

    Fail "Malformed parent node"


--------------------------------------------------------------------------------
-- Page. Section 7.7.3.3, Table 31

def PdfPage (resources : Resources) (pageNode : Dict) =
  case lookup "Contents" pageNode of
    nothing -> {| EmptyPage |}
    just vr -> {| ContentStreams = PdfPageContent resources vr |}

def PdfPageContent (resources : Resources) (vr : Value) =
  block
    resources   = resources
    let content = ContentStream vr
    data        = content.data
    UNPARSED    = content.UNPARSED




