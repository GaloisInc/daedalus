
def Main = {
  SetStream (arrayStream [2]);
  @x = UInt8;
  ^ case x is {
      1 -> 17 : uint 8;
      2 -> 18;
  }
}


