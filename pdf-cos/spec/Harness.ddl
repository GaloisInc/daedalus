-- Harness: testing harness
import ResourceDict
import TextEffect
import FontDict
import Type0Font
import Type1Font
import FontDesc
import TextObj

def TestFont : FontDict = MkType0Font (Type0Font (PartialType0Font
  true
  true
  (just Helvetica)
  (just {| preDef = "TestEnc"|})
  nothing
  nothing))

def TestResrcs : ResourceDict = ResourceDict (PartialResourceDict
  nothing
  nothing
  nothing
  nothing
  nothing
  (just (Insert "F13" TestFont empty))
  nothing
  nothing)

def TestSizedFont = SizedFont TestFont 12

-- Main: the entry point
def Main = Type1FontP

-- TODO: 
-- pages
-- page tree nodes

