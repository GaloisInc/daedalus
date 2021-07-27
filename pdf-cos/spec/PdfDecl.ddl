-- Testing: definitions for testing

-- ParseAtRef P r: parse the input at r, using P
def ParseAtRef r P = P

def DirectOrRef P = P

def ResolveStream v = {
  body = Choose {
    ok = GetStream;
  }
}

def WithReffedStreamBody P = P

def InputAtRef (r : Ref) = just GetStream
