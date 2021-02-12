
def Big = Match1 ('A' .. 'Z')
def Little = Match1 ('a' .. 'z')

def Main = { 
    @x = Big | Little;
    x >= 'J' is true; 
}
