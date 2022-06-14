-- testing avoiding state explosion

def Main =
  First
    block $$ = $[0xFE]; $[0x01]
    $[ !0xFE ]