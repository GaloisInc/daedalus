-- testing correctness of reconstruction of biased/unbiased alternation

def Main = { Test1; Test2; Test3; Test4; END }

def A = $['a']
def B = $['b']
def C = $['c']
def D = $['d']

def Test1 = ({A ; B} | B) <| A

def Test2 = ({A ; B} <| B) | A

def Test3 = ({A; B} <| {A ; B; C}) | {A ; D}

def Test4 = (@A <| ^ {})

