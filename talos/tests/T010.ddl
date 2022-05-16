
def Big = $['A' .. 'Z']
def Little = $['a' .. 'z']

def Main = { 
    @x = Big | Little;
    x >= 'J' is true; 
}
