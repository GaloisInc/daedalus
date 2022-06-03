-- Graphic state operations:
import GenPdfValue
import PdfValue

-- Intent: intents (Table 69) (nice)
def Intent = First
  absoluteColorimetric = @(Match "AbsoluteColorimetric")
  relativeColorimetric = @(Match "RelativeColorimetric")
  saturation = @(Match "Saturation")
  perceptual = @(Match "Perceptual")


def LineStyle = $['0' .. '2']

-- General Graphics State operators (Tables 51, 57)
def GraphicsStateOp = First
  pushState = @(KW "q")
  restoreState = @(KW "Q")
  concatMatrix = {
    a = Token Number ;
    b = Token Number ;
    c = Token Number ;
    d = Token Number ;
    e = Token Number ;
    f = Token Number ;
    KW "cm" ;
    }
  setLineWidth = {
    lineWidth = Token Number ;
    KW "w" ;
    }
  setLineCapStyle = {
    lineCap = Token LineStyle ;
    KW "J" ;
    }
  setLineJoinStyle = {
    lineJoin = Token LineStyle ;
    KW "j" ;
    }
  setMiterLimit = {
    miterLimit = Token Number ;
    KW "M" ;
    }
  setLineDash = {
    dashArray = GenArray (Token Natural); -- TODO: check that some array elt is non-zero
    dashPhase = Token Number ;
    KW "d" ;
    }
  setColorRenderingIntent = {
    intent = Token (GenName Intent);
    KW "ri" 
    }
  setFlat = {
    flatness = Token Number; -- TODO: check lower/upper bounds
    KW "i" ;
    }
  setGraphicsStateParams = {
    dictName = Token Name ;
    KW "gs" ;
    }
