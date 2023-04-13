import common

def Main =
  block
    BytesThenSuffix
    END

def BytesThenSuffix =
  First
    block
      i = GetStream                 -- i = stream
      x = "SUFFIX"                  -- x = "SUFFIX"
      b = isPrefix x i              -- b = i == h ++ t /\ x == h
      if b then                     --  b ==> i == h' ++ t' /\ len(h') == len(x) && stream' == t'
        SetStream (Drop len(x) i)   -- !b ==> false
      else
        fail "expected 'SUFFIX'"
    block
      -- UInt8
      let _ = Head GetStream
      SetStream (Drop 1 GetStream)  -- stream == h ++ t && len(h) == 1 && stream' == t
      BytesThenSuffix

-- BytesThenSuffix in graph form:
--     ---->--> (start) - "SUFFIX" -> (end)
--         \       /
--           UInt8

-- symbolic state:
--  - stream
--    - always a fresh variable pointing to the current head
--    - Drop and Take introduce new variables + constraints
--  - variables: map varname -> constraints
--
-- need some stream axioms, e.g.
-- str = a ++ b /\ str = c ++ d /\ len(a) = len(b) ==> a = c /\ b = d
--

-- the stream constraints will form a chain h ++ t1 ++ ... ++ tn.  individual
-- variables will have length and character constraints.  we should be able to
-- stitch them together to find cavities?
-- 
-- Maybe stream state consists of regions, where each unbounded stream movement
-- yields a new region?  (We can do this by assuming adversarial control over the
-- input, where the adversary always wants to create a cavity when specifying a
-- stream location.)
--
-- Each stream region is then a stack of intervals + constraints over intervals.

-- Thinking about loop/recursion summaries: several kinds of loops, explicitly bounded (Many (1..2) P), implicitly bounded (Many P), explicitly bounded by unknown (Many (x..y) P where x, y are parsed).  Maybe also different ways loops are implicitly bounded, e.g. 


-- what to do about loops we don't fully explore that have side effects on the stream?
def Main = LoopForN 10
def LoopForN n =
  block
    if n <= 0
      ^ "Done"
    else
      UInt8
      LoopForN (n - 1)

-- ideally want 0 < n <= 10 in the else body, but also to have a 10-byte cavity
