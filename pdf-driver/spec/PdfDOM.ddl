import PdfXRef
import PdfDecl
import PdfValue
import PdfValidate


def PdfTrailer (t : TrailerDict) =
  CheckRef "Catalog" PdfCatalog (t.root is just)
  -- XXX: apparently in linearized pdf this could `nothing` so we
  -- should specify how that works.


def PdfCatalog (v : Value) = {
  @d = v is dict;
  PdfType d "Catalog";

  -- Field Pages
  { @ref = Lookup "Pages" d is ref;
    CheckRef "PageTreeNodeRoot" (PdfPageTreeNode ref nothing) ref;
  };
}


def PdfPageTreeNode (self : Ref) (parent : maybe Ref) (v : Value) = {
  @d = v is dict;
  PdfType d "Pages";

  -- Field Count
  { @i = PdfInteger (Lookup "Count" d);
    Guard (i >= 0)
  };

  -- Field Parent, required for non-root nodes.
  Default {} {
    @p = parent is just;
    commit;
    @ref = Lookup "Parent" d is ref;
    Guard (ref == p)
  };

  -- Field Kids
  { @kids  = Lookup "Kids" d is array;
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
  { @ref = Lookup "Parent" d is ref;
    Guard (parent == ref);
  };
}

