
def Main = Ex6

def Ex1 =
  Choose
    A = Accept
    B = Accept

def Ex2 = T 'a'
def T x =
  Choose
    A = ^ x
    B = Accept

def Ex3 =
  block
    let x = Ex2
    x is A

def Ex5 =
  block
    let x = Ex1
    x is B

def Ex6 =
  block
    x = UInt8
    y = ^ true


