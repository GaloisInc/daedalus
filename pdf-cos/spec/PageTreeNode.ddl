-- PageTreeNode: parse a page tree node. Ensures that the page tree
-- actually forms a tree.
import Stdlib
import Pair
import Array
import Map

import GenPdfValue
import PdfDecl
import PdfValue
import ResourceDict
import Page

-- TODO: indirect deps
import FontDict
import Type1Font
import CMap
import Unicode
import ContentStreamLight

def PartialPageTreeNode (t: bool) (p: bool)
  (k: maybe [ Ref ]) (c: maybe int) (rs: maybe ResourceDict)
  os = {
  type0 = t;
  parent0 = p;
  kids0 = k;
  count0 = c;
  nodeResources0 = rs;
  others0 = os;
}

def InitPageTreeNode = PartialPageTreeNode
  false
  false
  nothing
  nothing
  nothing
  empty

def AddType pageTree = PartialPageTreeNode
  (Holds (NameToken "Pages"))
  pageTree.parent0
  pageTree.kids0
  pageTree.count0
  pageTree.nodeResources0
  pageTree.others0

def AddParent (par: Ref) pageTree = PartialPageTreeNode 
  pageTree.type0
  (Holds (Guard (Token Ref == par)))
  pageTree.kids0
  pageTree.count0
  pageTree.nodeResources0
  pageTree.others0

def AddKids pageTree = PartialPageTreeNode
  pageTree.type0
  pageTree.parent0
  (just (GenArray Ref))
  pageTree.count0
  pageTree.nodeResources0
  pageTree.others0

def AddCount pageTree = PartialPageTreeNode 
  pageTree.type0
  pageTree.parent0
  pageTree.kids0
  (just (DirectOrRef Natural))
  pageTree.nodeResources0
  pageTree.others0

def NodeAddResources pageTree = PartialPageTreeNode
  pageTree.type0
  pageTree.parent0
  pageTree.kids0
  pageTree.count0
  (just (DirectOrRef ResourceDictP))
  pageTree.others0

def NodeAddOther k pageTree = PartialPageTreeNode 
  pageTree.type0
  pageTree.parent0
  pageTree.kids0
  pageTree.count0
  pageTree.nodeResources0
  (Extend k (Token Value) pageTree.others0)

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
        pageTree.nodeResources0 is nothing;
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

def PageNodeKid resrcs (par : Ref) (r : Ref) = Choose1 {
  pageKid = PageP resrcs r;
  treeKid = PageTreeNodeP resrcs (just par) r;
}

-- PageTreeNode: coerce an partial page-tree node into a page
-- tree node
def PageTreeNode ancRs (par : maybe Ref) (cur : Ref) pt0 = {
  Guard pt0.type0;
  Guard pt0.parent0;

  kids = {
    @kidRefs = pt0.kids0 is just;
    @rs = case pt0.nodeResources0 of {
      just _ -> pt0.nodeResources0
    ; nothing -> ancRs
    };
    map (kidRef in kidRefs) (PageNodeKid rs cur kidRef)
  };

  count = {
    @kidsCount = for (kidsCountAcc = 0; kid in kids)
      (kidsCountAcc + (pageNodeKidCount kid));
    $$ = pt0.count0 is just;
    Guard ($$ == kidsCount)
  };

  others = pt0.others0;
}

def PageTreeNodeP ancRs (par: maybe Ref) (cur: Ref) =
  Between "<<" ">>" (PageTreeNode ancRs par cur
    (PageTreeNodeRec par cur InitPageTreeNode))

def PageTreeP cur = PageTreeNodeP nothing nothing cur
