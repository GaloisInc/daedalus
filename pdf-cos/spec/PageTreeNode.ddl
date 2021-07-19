-- PageTreeNode: parse a page tree node. Ensures that the page tree
-- actually forms a tree.
import Stdlib
import Pair
import Array
import Map

import GenPdfValue
import PdfValue
import ResourceDict
import Page

def PartialPageTreeNode (resrcs : maybe Pair) = {
  type0 = false;
  parent0 = false;
  kids0 = nothing;
  count0 = nothing;
  nodeResources0 = resrcs;
  others0 = empty;
}

def AddType pageTree : PartialPageTreeNode = {
  type0 = Holds (NameToken "Pages");

  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;
  count0 = pageTree.count0;
  nodeResources0 = pageTree.nodeResources0;
  others0 = pageTree.others0;
}

def AddParent (par: Ref) pageTree : PartialPageTreeNode = {
  type0 = pageTree.type0;

  parent0 = Holds (Guard (Token Ref == par));

  kids0 = pageTree.kids0;
  count0 = pageTree.count0;
  nodeResources0 = pageTree.nodeResources0;
  others0 = pageTree.others0;
}

def AddKids pageTree : PartialPageTreeNode = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;

  kids0 = just (GenArray Ref);

  count0 = pageTree.count0;
  nodeResources0 = pageTree.nodeResources0;
  others0 = pageTree.others0;
}

def AddCount pageTree : PartialPageTreeNode = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;

  count0 = just (DirectOrRef Natural);

  nodeResources0 = pageTree.nodeResources0;
  others0 = pageTree.others0;
}

def NodeAddResources pageTree : PartialPageTreeNode = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;
  count0 = pageTree.count0;

  nodeResources0 = LocalDefn (DirectOrRef ResourceDictP);

  others0 = pageTree.others0;
}

def NodeAddOther k pageTree : PartialPageTreeNode = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;
  count0 = pageTree.count0;
  nodeResources0 = pageTree.nodeResources0;

  others0 = Extend k (Token Value) pageTree.others0;
}

def PageTreeNodeRec (par : maybe Ref) (cur : Ref) pageTree = Default pageTree 
  { @k = Token Name;
    @pageTree0 = if k == "Type" then {
        pageTree.type0 is false;
        AddType pageTree
      }
      else if k == "Parent" then {
        pageTree.parent0 is false;
        @p = par is just;
        AddParent p pageTree
      }
      else if k == "Kids" then  {
        pageTree.kids0 is nothing;
        AddKids pageTree;
      }
      else if k == "Count" then {
        pageTree.kids0 is nothing;
        AddCount pageTree
      }
      else if k == "Resources" then {
        CheckNoLocalDefn pageTree.nodeResources0;
        NodeAddResources pageTree
      }
      else NodeAddOther k pageTree;
    PageTreeNodeRec par cur pageTree0
  }

-- pageNodeKidCount k: number of pages under kid k
def pageNodeKidCount (k : PageNodeKid) = case k of {
  pageKid _ -> 1;
  treeKid k -> k.count;
}

def PageNodeKid (resrcs : maybe Pair) (par : Ref) (r : Ref) = Choose1 {
  pageKid = Page (Bestow resrcs) r;
  treeKid = PageTreeNodeP (Bestow resrcs) (just par) r;
}

-- PageTreeNode: coerce an partial page-tree node into a page
-- tree node
def PageTreeNode (par : maybe Ref) (cur : Ref) pt0 = {
  Guard pt0.type0;
  Guard pt0.parent0;

  kids = {
    @kidRefs = pt0.kids0 is just;
    map (kidRef in kidRefs) (PageNodeKid pt0.nodeResources0 cur kidRef)
  };

  count = {
    @kidsCount = for (kidsCountAcc = 0; kid in kids)
      (kidsCountAcc + (pageNodeKidCount kid));
    $$ = pt0.count0 is just;
    Guard ($$ == kidsCount)
  };

  others = pt0.others0;
}

def PageTreeNodeP resrcs (par: maybe Ref) (cur: Ref) =
  Between "<<" ">>" (PageTreeNode par cur
    (PageTreeNodeRec par cur (PartialPageTreeNode (Inherit resrcs))) )

def PageTreeP cur = PageTreeNodeP nothing nothing cur
