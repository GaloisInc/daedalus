-- Test: shared references to boxed (recursive) union.
-- Both fields a and b point to the same IntList, so refcount > 1.

def IntList = First
  nil  = { @b = UInt8; b == 0 is true }
  cons = { val = UInt8; rest = IntList }

def Main = {
  let list = IntList;
  a = ^ list;
  b = ^ list;
}
