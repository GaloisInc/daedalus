-- Sum.ddl: sum types
def Sum P0 P1 = Choose {
  injl = P0;
  injr = P1;
}

def Sum1 P0 P1 : Sum = Choose1 {
  injl = P0;
  injr = P1;
}

def Injl x : Sum = {| injl = x |}
def Injr x : Sum = {| injr = x |}


def getLeft (s: Sum) = case s of
  injl sl -> just sl
  injr _ -> nothing

def getRight (s: Sum) = case s of 
  injr sr -> just sr
  injl _ -> nothing
