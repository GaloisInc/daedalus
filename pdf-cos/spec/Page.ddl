-- Page: a page object (Sec. 7.7.3.3)
import Stdlib

import PdfDecl
import PdfValue
import GenPdfValue
import ResourceDict
import ContentStreamLight

-- Page0: a partial Page value
def Page0 resrcs = {
  type0 = false;
  parent0 = false;
  resources0 = resrcs ;
  contents0 = nothing;
  others0 = empty;
}

-- TODO: precisely model other fields, as needed

-- parsers for updating fields
def PageAddType page0 : Page0 = {
  type0 = {
    Guard (!page0.type0);
    (NameToken "Page") | (NameToken "Template");
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

  resources0 = just ResourceDict;

  contents0 = page0.contents0;
  others0 = page0.others0;
}

def AddContents page0 : Page0 = {
  type0 = page0.type0;
  parent0 = page0.parent0;
  resources0 = page0.resources0;

  contents0 = just (
    { @arr = GenArray Ref;
      @strms = map (s in arr) {
        @strm = ResolveStreamRef s;
        @strmBody = strm.body is ok;
        WithStream strmBody (Many UInt8)
      };
      ^ (arrayStream (concat strms))
    } <|
    { @r = Ref;
      @strm = ResolveStreamRef r;
      strm.body is ok
    });

  others0 = page0.others0;
}

def PageAddOther page0 : Page0 = {
  type0 = page0.type0;
  parent0 = page0.parent0;
  resources0 = page0.resources0;
  contents0 = page0.contents0;

  others0 = {
    @k = Token Name; 
    @v = Token Value;
    Insert k v page0.others0
  };
}

def PageRec (par : Ref) page0 = 
  { @pageTree0 = Choose1 {
      { NameToken "Type";
        PageAddType page0;
      };
      { NameToken "Parent";
        PageAddParent par page0;
      };
      { NameToken "Resources";
        AddResources page0;
      };
      { NameToken "Contents";
        AddContents page0;
      };
      PageAddOther page0;
    };
    PageRec par page0
  } <|
  ^page0

def CoercePage page0 = {
  Guard page0.type0;
  Guard page0.parent0;

  resources = page0.resources0 is just;
  -- TODO: allow resource dicts to be inherited
  contents = case page0.contents0 of {
    nothing -> [ ];
    just contentStream -> WithStream contentStream
      (InterpContentStream resources)
  };
  others = page0.others0;
}

def Page (resrcs : maybe ResourceDict) (par: Ref) = {
  @initPage = Page0 resrcs;
  @pageRec = PageRec par initPage;
  CoercePage pageRec
}

-- TODO: relax to allow references in dictionary values
