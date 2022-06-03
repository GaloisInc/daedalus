-- Zoo9 from notes.org. Choice order is important

def Main = {
    a = $['0'..'9'];
    b = $['0'..'9'];
    c = Choose { 
        { b < '5' is true; ^ '5' }; 
        { ^ '1' }; 
    }; 
    c == '5' is true;
}
