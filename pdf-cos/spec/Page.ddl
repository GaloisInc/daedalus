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
  (rd : maybe ResourceDict) (c: maybe stream) = {
  type0 = t;
  parent0 = p;
  resources0 = rd;
  contents0 = c;
}

def InitPage = PartialPage
  false
  false
  nothing
  nothing

-- TODO: precisely model other fields, as needed

-- parsers for updating fields
def PageAddType (page0 : PartialPage) = PartialPage
  (Holds (DirectOrRef (
    (NameToken "Page") <|
    (NameToken "Template"))))
  page0.parent0
  page0.resources0
  page0.contents0

def PageAddParent (par : Ref) (page0 : PartialPage) = PartialPage
  page0.type0
-- BUG: causes page to be rejected
  (Holds (Guard (Token Ref == par)))
--  (Holds (Token Ref))
  page0.resources0
  page0.contents0

def AddResources (page0 : PartialPage) = PartialPage 
  page0.type0
  page0.parent0
  (just (DirectOrRef ResourceDictP))
  page0.contents0

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

def PageAddOther k page0 = PartialPage
  page0.type0
  page0.parent0
  page0.resources0
  page0.contents0

def ExtendPage (par : Ref) k (p : PartialPage) = 
  if k == "Type" then {
    p.type0 is false;
    just (PageAddType p)
  }
  else if k == "Parent" then {
    p.parent0 is false;
    just (PageAddParent par p)
  }
  else if k == "Resources" then {
    p.resources0 is nothing;
    just (AddResources p)
  }
  -- TEST:
  else if k == "Contents" then {
    p.contents0 is nothing;
    just (AddContents p)
  }
  else nothing

def Page ancRes (page0 : PartialPage) = {
  Guard page0.type0;
  Guard page0.parent0;

  contents = [] : [ ContentStreamObj ];
  -- contents = case page0.contents0 of {
  --   nothing -> [ ] ;
  --   just contentStream -> WithStream contentStream
  --     (ContentStreamP
  --       ((page0.resources0 is just) <|
  --        (ancRes is just) ))
  -- };
}

def PageP ancRes (par: Ref) = GenPdfDict1
  InitPage
  (ExtendPage par)
  (Page ancRes)
