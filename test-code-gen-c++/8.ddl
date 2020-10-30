
def F P = Choose {
  x = Choose { a = P; y = {} };
}

def Main = F (Match1 'a')
