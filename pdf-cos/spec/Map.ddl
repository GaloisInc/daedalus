-- Map.ddl: library for maps
import Stdlib
import Pair

def defaultEmpty (mopt : maybe [k -> v ]) : [k -> v] = case mopt of {
  just m -> m;
  nothing -> empty;
}

def MapEntry k v = {
  key = k;
  value = v;
}

def PairMapEntry p = MapEntry p.fst p.snd

def MapToList m = for (l = [ ]; k, v in m) (snoc (Pair k v) l)

def MapDomain m = {
  @ents = MapToList m;
  map (ent in ents) ent.fst
}

def MapLength m = length (MapToList m)

-- ListToMap l: collect list of entries l into a map:
def ListToMap l = for (acc = empty; e in l) Insert e.key e.value acc

def ListOfPairsToMap l = for (acc = empty; e in l) Insert e.fst e.snd acc

def MapTo d v = for (acc = empty; k in d) Insert k v acc

def MapUnion m0 m1 = for (acc = m1; k0, v0 in m0)
  (if IsBound k0 m1 then acc
  else Insert k0 v0 acc)

def UnionMapArray ms = for (acc = empty; m in ms) MapUnion acc m

def ComposeMaps m0 m1 = for (res = empty; k0, v0 in m0) 
  (Insert k0 (Lookup v0 m1) res)

def ComposePartialMaps dflt m0 m1 = for (res = empty; k0, v0 in m0)
  (Insert k0 (Default dflt (Lookup v0 m1)) res)

def TryLookup d k dflt = Default dflt (Lookup d k)

def IsBound k m = Default false (Holds (Lookup k m))

def Extend k v m = {
  @b = IsBound k m;
  Guard (!b);
  Insert k v m
}
