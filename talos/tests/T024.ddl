-- Zoo8 from notes.org. Many example. 

def Main = {
  @a = Match1 ('a'..'z');
  r = Many (3..) { 
    @b = Match1 ('a'..'z'); 
    b < a is true;  
  }; 
}
