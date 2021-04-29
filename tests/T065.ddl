
bitdata Bar where
  b1 = 63 : uint 8 -- decimal values have to have a type if it isn't inferrable from the context
  b2 = 64

bitdata Foo where
  A = 0x42         -- unless otherwise noted, hex and binary literals have their natural width (i.e. 8 here)
  B = { 0b11; x : uint 6 }
  C = { b : Bar }  -- other bitdata can be referenced, but currently have to be named.
  D = { 0b10; _ : uint 6 } -- '_' is a wildcard
