-- Marked Content
import PdfValue

-- ContentProps: properties for a content stream object
def ContentProps = Choose1 {
  inline = Dict
; nm = Name  -- TODO: lookup in resource dict
}

-- Marked-content operators (Table 320)
def MarkContentPoint = Choose1 {
  defineMarkedContent = {
    $$ = Token Name;
    KW "MP";
  }
; defMarkedContentPoint = {
    tag = Token Name;
    props = Token ContentProps;
    KW "DP";
  }
}

def BeginMarkedSeq = Choose1 {
  bmc = {
    $$ = Token Name;
    KW "BMC"
  }
; bdc = {
    tag = Token Name;
    props = Token ContentProps ;
    KW "BDC" ;
  }
}

def EndMarkedSeq = @(KW "EMC")

def MarkedContentSeq P = {
  begin = BeginMarkedSeq;
  markedContent = P;
  EndMarkedSeq;
}

def MarkedContentSeqOp = Choose1 {
  beginSeq = BeginMarkedSeq
; endSeq = EndMarkedSeq
}
