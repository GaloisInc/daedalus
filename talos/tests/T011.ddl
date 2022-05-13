
def Big = $['A' .. 'Z'] 
def Little = $['a' .. 'z']

def T = { 
    $$ = Big | Little;
    $$ >= 'J' is true; 
}

def Main = Many 10 T
