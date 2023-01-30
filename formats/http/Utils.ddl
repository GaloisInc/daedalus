

def ManyStart P Q = build (many (buf = emit builder P) (emit buf Q))
