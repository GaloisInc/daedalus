-- PageTreeNode: parse a page tree node. Ensures that the page tree
-- actually forms a tree.
import Stdlib

import GenPdfValue
import PdfValue
import Page

-- ParseRef r: implement, using a primitive that returns a daedalus stream
def ParseRef P r = Void

def PageTreeNode0 = {
  type0 = false;
  parent0 = false;
  kids0 = nothing;
  count0 = nothing;
  others0 = empty;
}

-- TODO: allow for other fields

def AddType pageTree : PageTreeNode0 = {
  type0 = {
    Guard (!pageTree.type0);
    NameToken "Pages";
    ^true
  };

  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;
  count0 = pageTree.count0;
  others0 = pageTree.others0;
}

def AddParent (par: maybe Ref) pageTree : PageTreeNode0 = {
  type0 = pageTree.type0;

  parent0 = {
    Guard (!pageTree.parent0);
    @r = Token Ref;
    Guard (just r == par);
    ^true
  };

  kids0 = pageTree.kids0;
  count0 = pageTree.count0;
  others0 = pageTree.others0;
}

def PageNodeKid (cur : Ref) (r : Ref) = Choose1 {
  pageKid = Page cur;
  treeKid = ParsePageTreeNode (just cur) r;
}

def AddKids (cur : Ref) pageTree : PageTreeNode0 = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;

  kids0 = {
    pageTree.kids0 is nothing;
    just (Many {
      @r = Token Ref;
      ParseRef r (PageNodeKid cur r)
    })
  };

  count0 = pageTree.count0;
  others0 = pageTree.others0;
}

def AddCount pageTree : PageTreeNode0 = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;

  count0 = just (Token Natural);

  others0 = pageTree.others0;
}

def AddOther pageTree : PageTreeNode0 = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;
  count0 = just (Token Natural);

  others0 = {
    @k = Token Name; 
    @v = Token Value;
    Insert k v pageTree.others0
  };
}

def PageTreeNodeRec (par : maybe Ref) (cur : Ref) pageTree =
  { @pageTree0 = Choose1 {
      { NameToken "Type";
        AddType pageTree;
      };
      { NameToken "Parent";
        AddParent par pageTree;
      };
      { NameToken "Kids";
        AddKids cur pageTree;
      };
      { NameToken "Count";
        AddCount pageTree;
      };
      AddOther pageTree;
    };
    PageTreeNodeRec par cur pageTree0
  } <|
  ^pageTree

-- PageTreeNode: coerce an partial page-tree node into a page
-- tree node
def PageTreeNode (par: maybe Ref) pt0 = {
  Guard pt0.type0;
  (par is nothing) | (Guard pt0.parent0);

  kids = pt0.kids0 is just;
  count = pt0.count0 is just;
  @kidsCount = for (kidsCountAcc = 0; kid in kids)
    (kidsCountAcc + kid.count);
  Guard (count == kidsCount);

  others = pt0.others0;
}

def ParsePageTreeNode (par: maybe Ref) (cur: Ref) = {
  @initPageTree = PageTreeNode0;
  @ptRec = PageTreeNodeRec par cur initPageTree;
  PageTreeNode par ptRec
}

-- TODO: split out cases for parsing root and non-root nodes

-- TODO: extend to check for cycles in page tree
