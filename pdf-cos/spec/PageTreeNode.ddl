-- PageTreeNode: parse a page tree node. Ensures that the page tree
-- actually forms a tree.
import Stdlib
import Array

import GenPdfValue
import PdfValue
import ResourceDict
import Page

def PageTreeNode0 resrcs = {
  type0 = false;
  parent0 = false;
  kids0 = nothing;
  count0 = nothing;
  resources0 = resrcs;
  others0 = empty;
}

def PageNodeKid (resrcs: maybe ResourceDict) (cur : Ref) (r : Ref) = Choose1 {
  pageKid = Page resrcs cur;
  treeKid = PageTreeNodeP resrcs cur r;
}

def nodeKeys = [
  "Type",
  "Parent",
  "Kids",
  "Count",
]

def pageNodeKidCount (k : PageNodeKid) = case k of {
  pageKid -> 1;
  treeKid k -> k.count;
}

def AddType pageTree : PageTreeNode0 = {
  type0 = {
    NameToken "Type";
    Guard (!pageTree.type0);
    NameToken "Pages";
    ^true
  };

  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;
  count0 = pageTree.count0;
  resources0 = pageTree.resources0;
  others0 = pageTree.others0;
}

def AddParent (par: Ref) pageTree : PageTreeNode0 = {
  type0 = pageTree.type0;

  parent0 = {
    NameToken "Parent";
    Guard (!pageTree.parent0);
    @r = Token Ref;
    Guard (r == par);
    ^true
  };

  kids0 = pageTree.kids0;
  count0 = pageTree.count0;
  resources0 = pageTree.resources0;
  others0 = pageTree.others0;
}

def AddKids (cur : Ref) pageTree : PageTreeNode0 = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;

  kids0 = {
    NameToken "Kids";
    pageTree.kids0 is nothing;
    just (GenArray Ref)
  };

  count0 = pageTree.count0;
  resources0 = pageTree.resources0;
  others0 = pageTree.others0;
}

def AddCount pageTree : PageTreeNode0 = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;

  count0 = {
    NameToken "Count";
    pageTree.count0 is nothing;
    just (DirectOrRef Natural);
  };

  resources0 = pageTree.resources0;
  others0 = pageTree.others0;
}

def NodeAddResources pageTree : PageTreeNode0 = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;
  count0 = pageTree.count0;

  resources0 = {
    NameToken "Resources";
    pageTree.resources0 is nothing;
    just (DirectOrRef ResourceDictP)
  };

  others0 = pageTree.others0;
}

def NodeAddOther pageTree : PageTreeNode0 = {
  type0 = pageTree.type0;
  parent0 = pageTree.parent0;
  kids0 = pageTree.kids0;
  count0 = pageTree.count0;
  resources0 = pageTree.resources0;

  others0 = {
    @k = Token Name; 
    Guard (!(member k nodeKeys));
    
    -- TODO: check that key is not bound
    @v = Token Value;
    Insert k v pageTree.others0
  };
}

def PageTreeNodeRec (par : Ref) (cur : Ref) pageTree = Default pageTree 
  { @pageTree0 = Choose1 {
      AddType pageTree;
      AddParent par pageTree;
      AddKids cur pageTree;
      AddCount pageTree;
      NodeAddResources pageTree;
      NodeAddOther pageTree;
    };
    PageTreeNodeRec par cur pageTree0
  }

def CheckKidsCount kids size = {
  @kidsCount = for (kidsCountAcc = 0; kid in kids)
    (kidsCountAcc + (pageNodeKidCount kid));
  Guard (size == kidsCount)
}

def ParseKidRefs resrcs kidRefs cur = map (kidRef in kidRefs)
  (ParseAtRef kidRef (PageNodeKid resrcs cur kidRef))

-- PageTreeNode: coerce an partial page-tree node into a page
-- tree node
def PageTreeNode (cur : Ref) pt0 = {
  Guard pt0.type0;
  Guard pt0.parent0;
  kids = {
    @kidRefs = pt0.kids0 is just;
    ParseKidRefs pt0.resources0 kidRefs cur
  };
  count = {
    $$ = pt0.count0 is just;
    CheckKidsCount kids $$
  };
  others = pt0.others0;
}

def PageTreeNodeP resrcs (par: Ref) (cur: Ref) = {
  @initPageTree = PageTreeNode0 resrcs;
  @ptRec = PageTreeNodeRec par cur initPageTree;
  PageTreeNode cur ptRec
}

-- Parse the root of a page tree:

-- RootNode0: a partial page tree root value
-- TODO: extend with a field for Resources
def RootNode0 = {
  rootType0 = ^false;
  rootKids0 = ^nothing;
  rootCount0 = ^nothing;
  rootResources0 = ^nothing; 
  rootOthers0 = empty;
}

def RootAddType partialRoot : RootNode0 = {
  rootType0 = {
    NameToken "Type";
    Guard (!partialRoot.rootType0);
    NameToken "Pages";
    ^true
  };

  rootKids0 = partialRoot.rootKids0;
  rootCount0 = partialRoot.rootCount0;
  rootResources0 = partialRoot.rootResources0;
  rootOthers0 = partialRoot.rootOthers0;
}

def RootAddKids partialRoot : RootNode0 = {
  rootType0 = partialRoot.rootType0;

  rootKids0 = {
    NameToken "Kids";
    partialRoot.rootKids0 is nothing;
    just (GenArray Ref)
  };

  rootCount0 = partialRoot.rootCount0;
  rootResources0 = partialRoot.rootResources0;
  rootOthers0 = partialRoot.rootOthers0;
}

def RootAddCount partialRoot : RootNode0 = {
  rootType0 = partialRoot.rootType0;
  rootKids0 = partialRoot.rootKids0;

  rootCount0 = {
    NameToken "Count";
    partialRoot.rootCount0 is nothing;
    just (DirectOrRef Natural);
  };

  rootResources0 = partialRoot.rootResources0;
  rootOthers0 = partialRoot.rootOthers0;
}

def RootAddResources partialRoot : RootNode0 = {
  rootType0 = partialRoot.rootType0;
  rootKids0 = partialRoot.rootKids0;
  rootCount0 = partialRoot.rootCount0;

  rootResources0 = {
    NameToken "Resources";
    partialRoot.rootResources0 is nothing;
    just (DirectOrRef ResourceDictP)
  };

  rootOthers0 = partialRoot.rootOthers0;
}

def RootAddOther partialRoot : RootNode0 = {
  rootType0 = partialRoot.rootType0;
  rootKids0 = partialRoot.rootKids0;
  rootCount0 = partialRoot.rootCount0;
  rootResources0 = partialRoot.rootResources0;

  rootOthers0 = {
    @k = Token Name; 
    Guard (!(member k (cons "Parent" nodeKeys)));
    -- "Parent" key is not permitted in the root node.
    
    -- TODO: check that key isn't already bound
    @v = Token Value;
    Insert k v partialRoot.rootOthers0
  };
}

def PageTreeRootRec partialRoot = Default partialRoot {
  @partialRoot0 = Choose1 {
    RootAddType partialRoot;
    RootAddKids partialRoot;
    RootAddCount partialRoot;
    RootAddResources partialRoot;
    RootAddOther partialRoot
  };
  PageTreeRootRec partialRoot0
}

-- RootNode: coerce a partial root node into a root node
def RootNode (cur : Ref) partialRoot = {
  Guard partialRoot.rootType0;
  rootResources = partialRoot.rootResources0;
  rootKids = {
    @kidRefs = partialRoot.rootKids0 is just;
    ParseKidRefs rootResources kidRefs cur
  };
  rootCount = {
    $$ = partialRoot.rootCount0 is just;
    CheckKidsCount rootKids $$
  };
  rootOthers = partialRoot.rootOthers0;
}

def PageTreeP (cur : Ref) = {
  @initRoot = RootNode0;
  @partialRoot = PageTreeRootRec initRoot;
  RootNode cur partialRoot
}

-- TODO: check for cycles when building page tree
