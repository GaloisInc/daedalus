-- Map.ddl: library for maps
import Stdlib
import Pair

def mapDomain m = for (acc = [ ]; k, v in m) snoc k acc

-- ListToMap l: collect list of entries l into a map:
def ListToMap l = for (acc = empty; e in l) Insert e.fst e.snd acc

def MapTo d v = for (acc = empty; k in d) Insert k v acc

def UnionMaps m0 m1 = for (acc = m1; k0, v0 in m0) Insert k0 v0 acc

def UnionMapArray ms = for (acc = empty; m in ms) UnionMaps acc m

def TryLookup d k dflt = Default dflt {
  @v = Lookup d k ;
  ^ v
} 
