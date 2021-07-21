-- Sum.ddl: sum types
def Sum P0 P1 = Choose {
  injl = P0;
  injr = P1;
}

def getLeft (s: Sum) = case s of
  injl sl -> just sl
  injr _ -> nothing

def getRight (s: Sum) = case s of 
  injr sr -> just sr
  injl _ -> nothing
