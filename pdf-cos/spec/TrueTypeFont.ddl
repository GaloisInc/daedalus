-- TrueTypeFont.ddl: definition of True Type fonts
import Stdlib

import GenPdfValue
import FontCommon
import PdfValue
import Type1Font

-- TypeType fonts (Sec. 9.6.3)
def TrueTypeFont = GenPdfDict1
  InitType1Font
  (ExtendType1Font "TrueTypeFont" Void)
  Type1Font
