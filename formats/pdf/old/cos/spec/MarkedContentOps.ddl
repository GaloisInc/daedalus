-- Marked Content
import PdfValue

-- ContentProps: properties for a content stream object
def ContentProps = First
  inline = Dict
  nm = Name  -- TODO: lookup in resource dict


-- Marked-content operators (Table 320)
def MarkContentPoint = First
  defineMarkedContent = {
    $$ = Token Name;
    KW "MP";
    }
  defMarkedContentPoint = {
    tag = Token Name;
    props = Token ContentProps;
    KW "DP";
    }


def BeginMarkedSeq = First
  bmc = {
    $$ = Token Name;
    KW "BMC"
    }
  bdc = {
    tag = Token Name;
    props = Token ContentProps ;
    KW "BDC" ;
    }


def EndMarkedSeq = @(KW "EMC")

def MarkedContentSeq P = {
  begin = BeginMarkedSeq;
  markedContent = P;
  EndMarkedSeq;
}

def MarkedContentSeqOp = First
  beginSeq = BeginMarkedSeq
  endSeq = EndMarkedSeq

