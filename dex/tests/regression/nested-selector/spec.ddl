-- Test: nested struct with exporter composition (borrow chain).

def Inner = { a = UInt8; b = UInt8; }

def Main = { tag = UInt8; inner = Inner; }
