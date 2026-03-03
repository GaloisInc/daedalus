-- Test: nested container types (array of arrays).
import Daedalus

def LenArray = {
  @n = UInt8;
  $$ = Many (n as uint 64) UInt8;
}

def Main = {
  @n = UInt8;
  data = Many (n as uint 64) LenArray;
}
