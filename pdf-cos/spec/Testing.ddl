-- Testing: definitions for testing

def DirectOrRef P = P

def ResolveStream v = {
  body = Choose {
    ok = GetStream;
  }
}
