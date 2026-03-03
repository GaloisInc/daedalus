-- Test: implicit default exporter resolution.
import Daedalus

def Pair = {
  x = LEUInt32;
  y = LEUInt32;
}

def Main = {
  p = Pair;
}
