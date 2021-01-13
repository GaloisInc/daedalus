-- Testing ambiguity detecting at the earliest

def Main =
{ a = Choose1
      { x1 = Match1 'a'
      , x2 = Match1 'a'
      , x3 = Many (Match1 'a')
      }
, b = Choose1
      { y1 = Match1 'b'
      , y2 = Match1 'b'
      }
, END
}