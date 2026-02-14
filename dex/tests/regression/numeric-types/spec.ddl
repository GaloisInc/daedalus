-- Test: all numeric DDL types.
import Daedalus

def Main = {
  a = UInt8;
  b = LEUInt16;
  c = LEUInt32;
  d = LEUInt64;
  e = LESInt32;
  f = LESInt64;
  g = UInt8 as! sint 8;
  h = LESInt16;
}
