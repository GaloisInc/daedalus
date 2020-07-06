{- Tests with Many:
   - SetUnion construct
   - Calling Class function NCCall
-}

def $letter = 'A'..'K' | 'a'..'k' | 'r'..'z'

def Main = { x = Many $letter ; END }
