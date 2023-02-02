

def ManyStart P Q = build (many (buf = emit builder P) (emit buf Q))

def When c P = if c then @P else Accept

def Count P : uint 64 = many (count = 0) { P; count + 1 }
