-- Unrolled version of T011.ddl 

def Big = Match1 ('A' .. 'Z') 
def Little = Match1 ('a' .. 'z') 

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