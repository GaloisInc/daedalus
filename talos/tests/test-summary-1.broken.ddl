
def Foo = { @v = UInt8; ^ v as int }

def Main = { x = Foo; Test2; (x < 10) is true }



def Test = { x = UInt8
           ; y = Choose {
             { x < 10 is true; ^ true };
             { x > 30 is true; ^ true };             
             { x > 20 is true; x < 25 is true; ^ false }
            }}
  
def Test2 = { r = Test
            ; r.y is true
            }



                
