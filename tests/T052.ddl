
def Main = {
  a = ^ rangeUp 10 : [int];
  b = ^ rangeDown 10 : [int];
  c = ^ rangeUp 10 20 : [int];
  d = ^ rangeDown 20 10 : [int];
  e = ^ rangeUp 10 20 2 : [int];
  f = ^ rangeDown 20 10 2 : [int];
  g = ^ rangeUp 3 : [uint 2];
  h = ^ rangeUp 0 : [uint 1];
  i = ^ rangeUp 20 10 : [int];
  j = ^ rangeDown 10 20 : [int];
  k = ^ rangeUp 250 255 50 : [uint 8];
  l = ^ rangeUp 0 0 1 : [uint 8];
}
