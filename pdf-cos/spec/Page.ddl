-- Page: a page object (Sec. 7.7.3.3)
import Stdlib
import Array
import Map

import PdfDecl
import PdfValue
import GenPdfValue
import ResourceDict
import ContentStreamLight

-- TODO: indirect deps
import FontDict
import Pair

-- PartialPage: a partial Page value
def PartialPage (t: bool) (p: bool)
  (r: maybe ResourceDict) (c: maybe stream)
  os = {
  type0 = t;
  parent0 = p;
  resources0 = r;
  contents0 = c;
  others0 = os;
}

def InitPage = PartialPage
  false
  false
  nothing
  nothing
  empty

-- TODO: precisely model other fields, as needed

-- parsers for updating fields
def PageAddType (page0 : PartialPage) = PartialPage
  (Holds (DirectOrRef (
    (NameToken "Page") |
    (NameToken "Template"))))
  page0.parent0
  page0.resources0
  page0.contents0
  page0.others0

def PageAddParent (par : Ref) (page0 : PartialPage) = PartialPage
  page0.type0
  (Holds (Guard (Token Ref == par)))
  page0.resources0
  page0.contents0
  page0.others0

def AddResources (page0 : PartialPage) = PartialPage 
  page0.type0
  page0.parent0
  (just (DirectOrRef ResourceDictP))
  page0.contents0
  page0.others0

def AddContents page0 = PartialPage 
  page0.type0
  page0.parent0
  page0.resources0

  (just (
      -- parse an array of references to streams
      { @arr = GenArray Ref;
        @strms = map (s in arr) {
          @strm = ResolveStreamRef s;
          @strmBody = strm.body is ok;
          WithStream strmBody (Many UInt8)
        };
        ^(arrayStream (concat strms))
      } <| -- parse a single reference
      { @r = Ref;
        @strm = ResolveStreamRef r;
        strm.body is ok
      }))
  page0.others0

def PageAddOther k page0 = PartialPage
  page0.type0
  page0.parent0
  page0.resources0
  page0.contents0
  (Extend k (Token Value) page0.others0)

def PageRec (par : Ref) (p : PartialPage) : PartialPage = Default p
  { @k = Token Name;
    @page0 = if k == "Type" then {
      p.type0 is false;
      PageAddType p
    }
    else if k == "Parent" then {
      p.parent0 is false;
      PageAddParent par p
    }
    else if k == "Resources" then {
      p.resources0 is nothing;
      AddResources p
    }
    else if k == "Contents" then {
      p.contents0 is nothing;
      AddContents p
    }
    else PageAddOther k p;
    PageRec par page0
  } 

def Page ancRs (page0 : PartialPage) = {
  Guard page0.type0;
  Guard page0.parent0;

  resources =
    (page0.resources0 is just) <|
    (ancRs is just);
  contents = case page0.contents0 of {
    nothing -> [ ];
    just contentStream -> WithStream contentStream (ContentStreamP resources)
  };
  others = page0.others0;
}

def PageP ancRs (par: Ref) : Page = Between "<<" ">>" {
  @initPage = InitPage;
  @pageRec = PageRec par initPage;
  Page ancRs pageRec
}
