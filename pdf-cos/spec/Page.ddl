-- Page: a page object (Sec. 7.7.3.3)
import Stdlib
import Array
import Map

import PdfDecl
import PdfValue
import GenPdfValue
import ResourceDict
import ContentStreamLight

-- Page0: a partial Page value
def Page0 resrcs = {
  type0 = false;
  parent0 = false;
  resources0 = resrcs;
  contents0 = nothing;
  others0 = empty;
}

-- TODO: precisely model other fields, as needed

-- parsers for updating fields
def PageAddType page0 : Page0 = {
  type0 = Holds (DirectOrRef (
    (NameToken "Page") |
    (NameToken "Template")));

  parent0 = page0.parent0;
  resources0 = page0.resources0;
  contents0 = page0.contents0;
  others0 = page0.others0;
}

def PageAddParent (par : Ref) page0 : Page0 = {
  type0 = page0.type0;

  parent0 = Holds (Guard (Token Ref == par));

  resources0 = page0.resources0;
  contents0 = page0.contents0;
  others0 = page0.others0;
}

def AddResources page0 : Page0 = {
  type0 = page0.type0;
  parent0 = page0.parent0;

  resources0 = LocalDefn (DirectOrRef ResourceDictP);
    

  contents0 = page0.contents0;
  others0 = page0.others0;
}

def AddContents page0 : Page0 = {
  type0 = page0.type0;
  parent0 = page0.parent0;
  resources0 = page0.resources0;

  contents0 = just (
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
      });
      
  others0 = page0.others0;
}

def PageAddOther k page0 : Page0 = {
  type0 = page0.type0;
  parent0 = page0.parent0;
  resources0 = page0.resources0;
  contents0 = page0.contents0;

  others0 = Extend k (Token Value) page0.others0;
}

def PageRec (par : Ref) page0 = 
  { @k = Token Name;
    @page0 = if k == "Type" then {
      page0.type0 is false;
      PageAddType page0
    }
    else if k == "Parent" then {
      page0.parent0 is false;
      PageAddParent par page0
    }
    else if k == "Resources" then {
      CheckNoLocalDefn page0.resources0;
      AddResources page0
    }
    else if k == "Contents" then {
      page0.contents0 is nothing;
      AddContents page0
    }
    else PageAddOther k page0;
    PageRec par page0
  } <|
  ^page0

def CoercePage page0 = {
  Guard page0.type0;
  Guard page0.parent0;

  resources = {
    @rs = page0.resources0 is just;
    rs.snd
  };
  contents = case page0.contents0 of {
    nothing -> [ ];
    just contentStream -> WithStream contentStream
      (InterpContentStream resources)
  };
  others = page0.others0;
}

def Page (resrcs : maybe Pair) (par: Ref) = Between "<<" ">>" {
  @initPage = Page0 (Inherit resrcs);
  @pageRec = PageRec par initPage;
  CoercePage pageRec
}
