def NT1 = @$['0'..'7']
def NT2 = { $['('] ; $['{'] ; $['}'] ; $[')'] }
def Main = { x = Many (4..5) NT1 ; y = Many 2 NT2 }
