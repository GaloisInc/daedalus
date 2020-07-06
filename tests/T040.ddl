def Main =
  { a = ^ true;
    b = ^ false;
    c = ^ nothing : maybe bool;
    d = ^ just true;
    e = d is nothing <| ^ false;
    f = d is just <| ^ true;
  }

