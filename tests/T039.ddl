
def Main = {
  Block 5 (Match "hi");
  Match "there"
}

def Block n P = {
  @s  = GetStream;
  @s2 = Take n s;
  SetStream s2;
  $$  = P;
  @s3 = Drop n s;
  SetStream s3;
}

