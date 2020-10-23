
def Node P =
  Choose1 {
    cons = { head = P; tail = Node P };
    nil  = {}
  }

def Main = Node (Match1 'a')

