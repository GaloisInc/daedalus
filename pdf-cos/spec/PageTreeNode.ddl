-- PageTreeNode: parse a page tree node. Ensures that the page tree
-- actually forms a tree.
import Stdlib
import Pair
import Array
import Map
import Maybe

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
  (k: maybe [ Ref ]) (c: maybe int) (rs: maybe ResourceDict) = {
  type0 = t;
  parent0 = p;
  kids0 = k;
  count0 = c;
  nodeResources0 = rs;
}

def InitPageTreeNode = PartialPageTreeNode
  false
  false
  nothing
  nothing
  nothing

def AddType pageTree = PartialPageTreeNode
  (Holds (NameToken "Pages"))
  pageTree.parent0
  pageTree.kids0
  pageTree.count0
  pageTree.nodeResources0

def AddParent (par: Ref) pageTree = PartialPageTreeNode 
  pageTree.type0
  (Holds (Guard (Token Ref == par)))
  pageTree.kids0
  pageTree.count0
  pageTree.nodeResources0

def AddKids pageTree = PartialPageTreeNode
  pageTree.type0
  pageTree.parent0
  (just (GenArray Ref))
  pageTree.count0
  pageTree.nodeResources0

def AddCount pageTree = PartialPageTreeNode 
  pageTree.type0
  pageTree.parent0
  pageTree.kids0
  (just (DirectOrRef (Token Natural)))
  pageTree.nodeResources0

def NodeAddResources pageTree = PartialPageTreeNode
  pageTree.type0
  pageTree.parent0
  pageTree.kids0
  pageTree.count0
  (just (DirectOrRef ResourceDictP))

def ExtendPageTreeNode (par : maybe Ref) (cur : Ref) k pageTree = 
  if k == "Type" then {
    pageTree.type0 is false;
    just (AddType pageTree)
  }
  else if k == "Parent" then {
    pageTree.parent0 is false;
    @p = par is just;
    just (AddParent p pageTree)
  }
  else if k == "Kids" then  {
    pageTree.kids0 is nothing;
    just (AddKids pageTree)
  }
  else if k == "Count" then {
    pageTree.count0 is nothing;
    just (AddCount pageTree)
  }
  else if k == "Resources" then {
    pageTree.nodeResources0 is nothing;
    just (NodeAddResources pageTree)
  }
  else nothing

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
  case par of {
    just _ -> Guard pt0.parent0
  ; nothing -> {}
  };

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
}

def PageTreeNodeP ancRes (par: maybe Ref) (cur: Ref) = GenPdfDict1
  InitPageTreeNode
  (ExtendPageTreeNode par cur)
  (PageTreeNode ancRes par cur)

def PageTreeP (cur : Ref) = PageTreeNodeP nothing nothing cur

  
