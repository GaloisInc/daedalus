-- test handle completeness in biased Many

def B = Many (1..) A

def A =
  Choose1
    block $$ = UInt8 0xFF ; UInt8 0x00
    UInt8 (!0xFF)


def Main =
  block
    B
