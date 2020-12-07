def Block n P =
  { @cur = GetStream;
    SetStream (Take n cur);
    $$ = P;
    SetStream (Drop n cur);
  }

def Main = {
  SetStream (arrayStream [1,2,3,4,5,6,7,8]);
  a = Block 2 UInt8;
  b = Many UInt8;
}

