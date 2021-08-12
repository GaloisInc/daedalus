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

-- partialPage: a partial Page value
def partialPage (t: bool) (p: bool)
  (rd : maybe ?resourceDict) (c: maybe stream) = {
  type0 = t;
  parent0 = p;
  resources0 = rd;
  contents0 = c;
}

def initPage = partialPage
  false
  false
  nothing
  nothing

-- TODO: precisely model other fields, as needed

-- parsers for updating fields
def PageAddType (page0 : partialPage) = partialPage
  (Holds (DirectOrRef (
    (NameToken "Page") <|
    (NameToken "Template"))))
  page0.parent0
  page0.resources0
  page0.contents0

def PageAddParent (par : Ref) (page0 : partialPage) = partialPage
  page0.type0
  (Holds (Guard (Token Ref == par)))
  page0.resources0
  page0.contents0

-- foo
def AddResources (page0 : partialPage) = partialPage 
  page0.type0
  page0.parent0
  (just (DirectOrRef ResourceDictP))
  page0.contents0

def AddContents page0 = partialPage 
  page0.type0
  page0.parent0
  page0.resources0
  (just (
      -- parse an array of references to streams
      { @arr = GenArray Ref;
        @strms = map (s in arr) {
          @strm = Resolve_Ref_ToStream s;
          @strmBody = strm.body is ok;
          WithStream strmBody (Many UInt8)
        };
        ^(arrayStream (concat strms))
      } <| -- parse a single reference
      { @r = Token Ref;
        @strm = Resolve_Ref_ToStream r;
        strm.body is ok
      }))

def ExtendPage (par : Ref) k (p : partialPage) = 
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
  else if k == "Contents" then {
    p.contents0 is nothing;
    just (AddContents p)
  }
  else nothing

def Page ancRes (page0 : partialPage) = {
  Guard page0.type0;
  Guard page0.parent0;

  contents = case page0.contents0 of {
    nothing -> [ ] ;
    just contentStream -> WithStream contentStream
      (ContentStreamP
        ((page0.resources0 is just) <|
         (ancRes is just) ))
  };
}

def PageP ancRes (par: Ref) = GenPdfDict1
  initPage
  (ExtendPage par)
  (Page ancRes)
