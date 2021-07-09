-- TODO: test handle correctness in biased choice
-- The trick is this should not match "ab"

def A =
  Choose1
    block $$ = UInt8 'a' ; UInt8 'b'
    UInt8 (!'a')


def Main =
  block
    Many A
    UInt8 'a'
    UInt8 'b'
    END
