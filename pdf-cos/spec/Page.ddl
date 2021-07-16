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

def pageFields = [
  "Type",
  "Parent",
  "Resources",
  "Contents",
  "Others"
]

-- TODO: precisely model other fields, as needed

-- parsers for updating fields
def PageAddType page0 : Page0 = {
  type0 = {
    NameToken "Type";
    Guard (!page0.type0);
    DirectOrRef (
      (NameToken "Page") |
      (NameToken "Template"));
    ^true
  };

  parent0 = page0.parent0;
  resources0 = page0.resources0;
  contents0 = page0.contents0;
  others0 = page0.others0;
}

def PageAddParent (par : Ref) page0 : Page0 = {
  type0 = page0.type0;

  parent0 = {
    NameToken "Parent";
    Guard (!page0.parent0);
    @r = Token Ref;
    Guard (r == par);
    ^true
  };

  resources0 = page0.resources0;
  contents0 = page0.contents0;
  others0 = page0.others0;
}

def AddResources page0 : Page0 = {
  type0 = page0.type0;
  parent0 = page0.parent0;

  resources0 = {
    NameToken "Resources";
    CheckNoLocalDefn page0.resources0;
    LocalDefn (DirectOrRef ResourceDictP);
  };

  contents0 = page0.contents0;
  others0 = page0.others0;
}

def AddContents page0 : Page0 = {
  type0 = page0.type0;
  parent0 = page0.parent0;
  resources0 = page0.resources0;

  contents0 = {
    NameToken "Parent";
    page0.contents0 is nothing;
    just (
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
      })
  };
  others0 = page0.others0;
}

def PageAddOther page0 : Page0 = {
  type0 = page0.type0;
  parent0 = page0.parent0;
  resources0 = page0.resources0;
  contents0 = page0.contents0;

  others0 = {
    @k = Token Name; 
    Guard (!(member k pageFields));

    @v = Token Value;
    InsertFresh k v page0.others0
  };
}

def PageRec (par : Ref) page0 = 
  { @pageTree0 = Choose1 {
      PageAddType page0;
      PageAddParent par page0;
      AddResources page0;
      AddContents page0;
      PageAddOther page0;
    };
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
