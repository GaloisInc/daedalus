bitdata Group where
  more:  uint 1
  value: uint 7

def SizedNum =
  block
    $$ = many (loop = { value = 0 : uint 64, bits = 0, done = false })
      if loop.done
      then Fail "" -- exit loop
      else
        block
          let group = UInt8 as Group
          let chunk = (group.value as ?auto) << loop.bits
          value = chunk .|. loop.value
          bits  = loop.bits + 7
          done  = group.more == 0
    $$.done is true -- loop stopped normally

def U64 = SizedNum.value

def S64 =
  block
    let n = SizedNum
    let sign = 1 << (n.bits - 1)
    (n.value .^. sign) - sign as! sint 64

def Main =
  block
    $$ = many (s = 0) (s + U64)
    END
