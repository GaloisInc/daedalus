-- test complement of class character

def A =
  First
    block $$ = $[0xFF] ; $[0x00]
    $[!0xFF]


def Main =
  block
    A
