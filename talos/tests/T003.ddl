

def $c   = ('A' | 'a' | '0')
def Main = { @v1 = Match1 ('0' .. '9');
             @v2 = Match1 ('a' .. 'z');
             v3 = Match1 $c;
           }
