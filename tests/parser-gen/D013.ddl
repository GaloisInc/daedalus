-- Testing ambiguity detecting at the earliest

def Main =
{ a = First
        x1 = $['a']
        x2 = $['a']
        x3 = Many $['a']

, b = First
        y1 = $['b']
        y2 = $['b']

, END
}
