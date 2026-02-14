-- Test: map iteration.

def Main = {
  mp = block
    let m0 = empty : [uint 8 : uint 8];
    let k1 = UInt8;
    let v1 = UInt8;
    let m1 = insert k1 v1 m0;
    let k2 = UInt8;
    let v2 = UInt8;
    insert k2 v2 m1;
}
