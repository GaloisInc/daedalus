-- Test: polymorphic exporters and parameterized type aliases.
import Daedalus

def Main = {
  @len   = UInt8;
  first  = Chunk (len as uint 64) (Many UInt8);
  second = Many UInt8;
}
