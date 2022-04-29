-- wrapper around the list of Glyph encodings
import Map
import Stdlib

import GenPdfValue
import Glyph
import GlyphList
import Unicode

def GlyphEncoding : [ glyph -> [ UTF8 ] ] = ListToMap
  (map (ent in glyphEncs)
    (mapEntry (glyph ent.key) (map (pt in ent.value) (unicodePoint pt))))
