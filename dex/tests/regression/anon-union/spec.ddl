-- Test: anonymous union names (bug 2 regression).
-- The case expression inside a struct creates an anonymous union type (Main0).

def Main = {
  @disc = UInt8;
  val = case disc of
          1 -> {| num  = UInt8 |}
          2 -> {| pair = { a = UInt8; b = UInt8 } |}
}
