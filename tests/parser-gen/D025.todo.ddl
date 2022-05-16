-- TODO: test handle correctness in biased choice
-- The trick is this should not match "ab"

def A =
  First
    block $$ = $['a'] ; $['b']
    $[!'a']


def Main =
  block
    Many A
    $['a']
    $['b']
    END
