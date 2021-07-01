-- Map.ddl: library for maps
import Stdlib

def MapTo d v = for (acc = empty; k in d) Insert k v acc

def UnionMaps m0 m1 = for (acc = m1; k0, v0 in m0) Insert k0 v0 acc

def TryLookup d k default = Default default {
  @v = Lookup d k ;
  ^ v
} 
