-- Test: non-boxed (non-recursive) union case dispatch (bug 3 regression).

def Main = First
  tag_a = { @b = UInt8; b == 1 is true }
  tag_b = { @b = UInt8; b == 2 is true }
