import PdfValue
import PdfValidate


def PdfTrailer (t : TrailerDict) =
  CheckRef "Catalog" PdfCatalog (t.root is just)
  -- XXX: apparently in linearized pdf this could `nothing` so we
  -- should specify how that works.


def PdfCatalog (v : Value) = {
  @d = v is dict;
  PdfType d "Catalog";

  -- Field "Pages"
  { @ref = Lookup "Pages" d is ref;
    CheckRef "PageTreeNodeRoot" (PdfPageTreeNode ref nothing) ref;
  };
}


-- This is used for both the root node and intermediate nodes in the
-- tree.   In the case of the root, the `parent` is `nothing`.
def PdfPageTreeNode (self : Ref) (parent : maybe Ref) (v : Value) = {
  @d = v is dict;
  PdfType d "Pages";

  -- Field "Count"
  Guard (PdfInteger (Lookup "Count" d) >= 0);

  -- Field "Parent", required for non-root nodes.
  Optional {
    @p = parent is just;
    commit;
    Guard (Lookup "Parent" d is ref == p);
  };

  -- Field "Kids"
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

  -- Field "Parent"
  Guard (Lookup "Parent" d is ref == parent);
}

