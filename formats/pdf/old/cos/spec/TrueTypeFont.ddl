-- TrueTypeFont.ddl: definition of True Type fonts
import Stdlib

import GenPdfValue
import FontCommon
import FontDesc
import PdfValue
import Type1Font

-- TypeType fonts (Sec. 9.6.3)
def TrueTypeFont = GenPdfDict1
  initType1Font
  (ExtendType1Font trueTypeSym Void)
  (Type1Font trueTypeSym)
