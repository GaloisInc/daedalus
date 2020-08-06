import PdfXRef
import PdfDecl
import PdfValue
import PdfValidate


def DOMTrailer (t : TrailerDict) = {
  @ref = t.root is just;    -- XXX: optional for linearized PDF?
  CheckRef "Catalog" PdfCatalog ref;
}



def PdfCatalog (v : Value) = {
  @d = v is dict;
  PdfType d "Catalog";

  -- Field Pages
  { @pages = Lookup "Pages" d;
    @ref   = pages is ref;
    CheckRef "PageTreeNodeRoot" (PdfPageTreeNode ref nothing) ref;
  }
}


def PdfPageTreeNode (self : Ref) (parent : maybe Ref) (v : Value) = {
  @d = v is dict;
  PdfType d "Pages";

  -- Field Count
  { @countV = Lookup "Count" d;
    @count  = PdfInteger countV;
    count >= 0;
  };

  -- Field Parent, required for non-root nodes.
  Default {} {
    @p = parent is just;
    commit;
    @val = Lookup "Parent" d;
    @ref = val is ref;
    p == ref;
  };

  -- Field Kids
  { @kidsV = Lookup "Kids" d;
    @kids  = kidsV is array;
    for (s = {}; v in kids) {
      @kid = v is ref;
      Choose1 {
        { CheckRef "PageTreeNode" (PdfPageTreeNode kid (just self)) kid };
        { CheckRef "PageObject"   (PdfPageObject             self)  kid };
      }
    }
  };

}


def PdfPageObject (parent : Ref) (v : Value) = {
  @d = v is dict;
  PdfType d "Page";

  -- Field Parent
  { @val = Lookup "Parent" d;
    @ref = val is ref;
    parent == ref;
  };
}

