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
def PdfCatalog
  (strict : bool)
  (enc : StdEncodings)
  (pageIndex : maybe (uint 64))
  (r : Ref) =
  block
    let ?strict = strict
    let d        = ResolveValRef r is dict
    pageTree     = PdfPageTreeRoot pageIndex (LookupRef "Pages" d)
    stdEncodings = enc
    -- other fields omitted


-- ENTRY
def PdfPageCount (r : Ref) : uint 64 =
  block
    let catalog = ResolveValRef r is dict
    let pages   = ResolveValRef (LookupRef "Pages" catalog) is dict
    LookupSize "Count" pages


--------------------------------------------------------------------------------
-- Page Tree; Section 7.7.3

def PdfPageTreeRoot pageIndex (r : Ref) =
  PdfPageTree pageIndex nothing noResources r

def PdfPageTree
  (pageIndex : maybe (uint 64))
  (p : maybe Ref)
  (parentResources : Resources)
  (r : Ref) =
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

    case LookupName "Type" node of
      "Pages" ->
        block
          let kids = LookupResolve "Kids" node is array
          {| Node =
               case pageIndex of
                 nothing ->
                   map (child in kids)
                     (PdfPageTree nothing (just r) resources (child is ref))
                 just page ->
                   [ PdfPageTreeChild page (just r) resources kids 0 ]
            |}

      "Page"  ->
        block
          case pageIndex of
            nothing   -> Accept
            just page -> page == 0 is true
          {| Leaf = PdfPage resources node |}

      _       -> Fail "Unexpected `Type` in page tree"

def PdfPageTreeChild
  (pageIndex : uint 64)
  (parent : maybe Ref)
  (resources : Resources)
  (kids : [Value])
  (kidIndex : uint 64) =
  block
    let child = Index kids kidIndex is ref
    let node = ResolveValRef child is dict
    let pageCount =
      case LookupName "Type" node of
        "Pages" -> LookupSize "Count" node
        "Page"  -> 1
        _       -> Fail "Unexpected `Type` in page tree"
    if pageIndex < pageCount
      then PdfPageTree (just pageIndex) parent resources child
      else
        PdfPageTreeChild
          (pageIndex - pageCount) parent resources kids (kidIndex + 1)

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

