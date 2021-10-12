def Main =
  block
    a = ^ true
    b = ^ false
    c = ^ nothing : maybe bool
    d = ^ just true
    e = d is nothing <| ^ {}
    f = d is just <| ^ true

