def Main = {
  a1 = 0xF0 .|. 0x03  : uint 8;
  a2 = a1 # a1;
  a3 = a2 <# (0x00 : uint 3);
  a4 = a3 .&. ~a2;
  a5 = a4 << 64;
  a6 = a4 << 3;
}
