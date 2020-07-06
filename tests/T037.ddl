
def Main = {
  @one = ^ (1 : uint 8);
  t_and = ^ (one & 2);
  t_or = ^ (one | 2);
  t_xor = ^ (one ^ 0);
  t_comp = ^ ~ t_or
}
