-- Test: empty collections through iteration (zero-element edge case).
import Daedalus

def Main = {
  @n = UInt8;
  arr = Many (n as uint 64) UInt8;
  mp  = ^ (empty : [uint 8 : uint 8]);
}
