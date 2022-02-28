-- Map.ddl: library for maps
import Stdlib
import Pair

def defaultEmpty (mopt : maybe [ ?k -> ?v ]) : [ ?k -> ?v ] = case mopt of {
  just m -> m;
  nothing -> empty;
}

def mapEntry k v = {
  key = k;
  value = v;
}

def PairMapEntry p = mapEntry p.fst p.snd

def mapToList m = for (l = [ ]; k, v in m) (snoc (pair k v) l)

def MapDomain m = {
  @ents = mapToList m;
  map (ent in ents) ent.fst
}

def mapLength m = length (mapToList m)

-- ListToMap l: collect list of entries l into a map:
def ListToMap l = for (acc = empty; e in l) (insert e.key e.value acc)

def ListOfPairsToMap l = for (acc = empty; e in l) insert e.fst e.snd acc

-- MapDisjUnion m0 m1: union of disjoint maps m0 and m1
def MapDisjUnion m0 m1 = for (acc = m0; k1, v1 in m1)
  insert k1 v1 acc

def MapTo d v = for (acc = empty; k in d) insert k v acc

def MapUnion m0 m1 = for (acc = m1; k0, v0 in m0)
  (if IsBound k0 m1 then acc
  else insert k0 v0 acc)

def UnionMapArray ms = for (acc = empty; m in ms) MapUnion acc m

def ComposeMaps m0 m1 = for (res = empty; k0, v0 in m0)
  (insert k0 (Lookup v0 m1) res)

def ComposePartialMaps dflt m0 m1 = for (res = empty; k0, v0 in m0)
  (insert k0 (Default dflt (Lookup v0 m1)) res)

def TryLookup d k dflt = Default dflt (Lookup d k)

def IsBound k m = Default false (Holds (Lookup k m))

def Extend k v m = {
  @b = IsBound k m;
  Guard (!b);
  insert k v m
}
