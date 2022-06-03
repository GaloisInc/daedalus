-- Unrolled version of T011.ddl 

def Big = $['A' .. 'Z'] 
def Little = $['a' .. 'z'] 

def T = { 
    $$ = Big | Little;
    $$ >= 'J' is true; 
}

def Main = {
  T;
  T; 
  T; 
  T; 
  T; 
  T; 
  T; 
  T;
  T;
  T; 
}
