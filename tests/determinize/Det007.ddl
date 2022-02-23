-- testing correctness of reconstruction of biased/unbiased alternation

def Main = { Test1; Test2; Test3; Test4; END }

def A = Match1 'a'
def B = Match1 'b'
def C = Match1 'c'
def D = Match1 'd'

def Test1 = ({A ; B} | B) <| A

def Test2 = ({A ; B} <| B) | A

def Test3 = ({A; B} <| {A ; B; C}) | {A ; D}

def Test4 = (@A <| ^ {})

