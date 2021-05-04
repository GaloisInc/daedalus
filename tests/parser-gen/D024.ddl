-- test complement of class character

def A =
  Choose1
    block $$ = UInt8 0xFF ; UInt8 0x00
    UInt8 (!0xFF)


def Main =
  block
    A
