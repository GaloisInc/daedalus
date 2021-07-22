-- Harness: testing harness
import ResourceDict
import Type1Font
import FontDesc
import TextObj

def Main0 = Type1FontP

def Main = {
  $$ = TextObj (ResourceDict InitResourceDict) nothing;
}

-- TODO: unit test these parsers
-- content stream
-- pages
-- page tree nodes
