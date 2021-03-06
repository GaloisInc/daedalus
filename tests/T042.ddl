
def Main = Ex6

def Ex1 = Choose { A = {}; B = {} }

def Ex2 = T 'a'
def T x = Choose { A = ^ x; B = {} }

def Ex3 = { @x = Ex2; x is A }

def Ex5 = { @x = Ex1; x is B }

def Ex6 = { x = UInt8; y = ^ true }


