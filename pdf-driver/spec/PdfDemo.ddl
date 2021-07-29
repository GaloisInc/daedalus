import Stdlib
import PdfValue
import PdfDecl
import PdfXRef

-- import ContentStreamLight
-- TODO: gradually replace this with a more heavyweight parser/validator
import PageTreeNode

def IsRootPages r = Default false {
--  IsPageOrPages nothing r;
  ^ true
}

def IsPageOrPages p c =
  (IsPage p c) |
  (IsPages p c)

def IsPage (p : maybe Ref) (r : Ref) =
{
    @v = ResolveValRef r;
    @dict = v is dict;
    CheckType "Page" dict;
    CheckParent p dict;
}

def IsPages (p : maybe Ref) (r : Ref) =
{
    @v = ResolveValRef r;
    @dict = v is dict;
    CheckType "Pages" dict;
    CheckParent p dict;
    -- Ignore count for now
    @kidsv = Lookup "Kids" dict;    -- XXX: can this be a ref?
    @kids  = kidsv is array;
    for (acc = { }; refv in kids) {
        @ref = refv is ref;
        IsPageOrPages (just r) ref;
    };
}

def CheckParent (p : maybe Ref) (dict : [[uint 8] -> Value]) =
    { p is nothing;
      @v = Optional (LookupRef "Parent" dict);
      v is nothing;
    }
    <|
    { @pref = p is just;
      @dpref = LookupRef "Parent" dict;
      Guard (dpref == pref);
    }

--------------------------------------------------------------------------------
-- Catalogue objects

-- The JS stuff will be take care of by the global safety check as
-- Names->JavaScript maps to JavaScript actions.

def CatalogIsOK r = {
  @catv = ResolveValRef r;
  @cat   = catv is dict;
  CheckType "Catalog" cat;
  @pages = LookupRef "Pages" cat;
  Holds (PageTreeP pages)
  --IsRootPages pages;
}

--------------------------------------------------------------------------------
-- Checked

def CheckDecl expectId expectGen (decl : TopDecl) = {
  Guard (decl.id == expectId);
  Guard (decl.gen == expectGen);
  obj    = ^ decl.obj;
  isSafe = { @v = decl.obj is value;  ValueIsSafe v }
         | {      decl.obj is stream; ^ safeSafetyInfo };
}

def TopDeclCheck expectId expectGen = {
  @decl = TopDecl;
  CheckDecl expectId expectGen decl;
}

def ResolveObjectStreamEntryCheck
      expectId expectGen (oid : Nat) (gen : Nat) (idx : Nat) = {
  @decl = ResolveObjectStreamEntry oid gen idx;
  CheckDecl expectId expectGen decl;
}

--------------------------------------------------------------------------------
-- JavaScript predicate

def safetyInfo (js : bool) (uri : bool) = { hasJS = js; hasURI = uri }

def band (b1 : bool) (b2 : bool) : bool = if b1 then b2 else false
def bor  (b1 : bool) (b2 : bool) : bool = if b1 then true else b2

def mergeSafetyInfo si1 si2 =
    safetyInfo (bor si1.hasJS si2.hasJS) (bor si1.hasURI si2.hasURI)

def safeSafetyInfo = safetyInfo false false

def ValueIsSafe (v : Value) =
    Choose {
        When (@(v is null) | @(v is bool) | @(v is ref) | @(v is name)
              | @(v is string) | @(v is number)) safeSafetyInfo;
        { @dict = v is dict;
          -- Check to see whether it contains a JS action
          Choose1 {
             When  (DictIsAction "JavaScript" dict) (safetyInfo true false);
             When  (DictIsAction "URI" dict) (safetyInfo false true);
             (for (acc = safeSafetyInfo; v in dict) {
                 @r = ValueIsSafe v;
                 ^ mergeSafetyInfo acc r;
                 }
             )
          }
        };
        { @arr = v is array;
          for (acc = safeSafetyInfo; v in arr) {
              @r = ValueIsSafe v;
              ^ mergeSafetyInfo acc r;
          }
        }
    }

def DictIsAction a d = {
  -- Type is optional, so we ignore it and just look for the field "S"
  @n = LookupName "S" d; 
  Guard (a == n);
}


