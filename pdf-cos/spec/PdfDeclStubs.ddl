-- Testing: definitions for testing
import Stdlib

-- ParseAtRef P r: parse the input at r, using P
def ParseAtRef r P = P

def DirectOrRef P = P

def StreamBody = Choose {
  ok = GetStream;
}

def ResolveStream v = {
  body = StreamBody;
}

def WithReffedStreamBody P = P

def InputAtRef (r : Ref) = just GetStream

-- stub defn:
def Stream (hdr : [ [ uint 8 ] -> int ]) = {
  header = hdr;
  body = StreamBody;
}

def ResolveStreamRef (r : Ref) : Stream = Stream empty
