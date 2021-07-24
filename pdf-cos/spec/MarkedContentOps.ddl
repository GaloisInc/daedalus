-- Marked Content
import PdfValue

-- ContentProps: properties for a content stream object
def ContentProps = Choose1 {
  inline = Dict
; nm = Name  -- TODO: lookup in resource dict
}

-- Marked-content operators (Table 320)
def MarkedContentOp = Choose1 {
  defineMarkedContent = {
    tag = Token Name ;
    KW "MP";
  }
; defMarkedContentPoint = {
    tag = Token Name ;
    props = Token ContentProps ;
    KW "DP" ;
  }
; beginMarked = @(KW "BMC")
; beginMarkedProp = {
    tag = Token Name ;
    props = Token ContentProps ;
    KW "BDC" ;
  } 
; endMarked = @(KW "EMC")
-- TODO: refine to check well-nesting property between operators
}
