def Main : {} = Loop $['a']

def Loop P : {} =
  block
    P
    Loop
      block
        P
        $['a']
