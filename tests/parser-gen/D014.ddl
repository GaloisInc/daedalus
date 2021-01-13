-- Test this fragment from pdf-simple.ddl
-- NOTE: problems are:
-- * an exponential behavior with the size of AString
-- * not determinized because of overflow
-- * ... more problems

def SkipTo P = P  <| { @UInt8; SkipTo P }

def AString = Match "a"

def Main = SkipTo AString