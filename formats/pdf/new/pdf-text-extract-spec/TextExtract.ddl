import PdfValue
import PdfDecl
import StandardEncodings
import Catalog
import CMap
import ContentStream
import Fonts

-- ENTRY
def TextInCatalogPage (c : PdfCatalog) : {} =
  block
    let ?stdEncodings = c.stdEncodings
    ResetPage
    TextInPage c.page

def TextInPage (p : PdfPage) =
  case p of
    EmptyPage -> Accept
    ContentStreams content ->
      block
        let ?resources = content.resources
        TextInPageContnet content

def TextInPageContnet (p : PdfPageContent) =
  block
    let ?instrs = p.data
    FindTextOnPage false 0 0

def GetOperand i = (Index ?instrs i : ContentStreamEntry) is value

def SelectFont mbValue =
  case mbValue of
    nothing -> SetFont nothing
    just value ->
      case value of
        ref r -> SetFont (just (LoadFontByRef ?stdEncodings r))
        _ -> SetFont (just (Font value))

def FindTextOnPage
  (inText : bool)
  (operandCount : uint 64)
  (i : uint 64) : {} =
  case Optional (Index ?instrs i) of
    nothing -> Accept
    just instr ->
      case instr of

        operator op ->
          case op of

            BT ->
              block
                BeginText
                FindTextOnPage true 0 (i+1)

            ET ->
              block
                EndText
                FindTextOnPage false 0 (i+1)

            Tj ->
              block
                if inText && operandCount == 1
                  then DecodeText (GetOperand (i - 1) is string)
                  else Accept
                FindTextOnPage inText 0 (i+1)


            quote ->
              block
                if inText && operandCount == 1
                  then
                    block
                      EmitUtf16 [ '\n' as uint 16 ]
                      DecodeText (GetOperand (i - 1) is string)
                  else Accept
                FindTextOnPage inText 0 (i+1)

            dquote ->
              block
                if inText && operandCount == 3
                  then
                    block
                      EmitUtf16 [ '\n' as uint 16 ]
                      DecodeText (GetOperand (i - 1) is string)
                  else Accept
                FindTextOnPage inText 0 (i+1)

            Td, TD ->
              block
                if inText && operandCount == 2
                  then EmitUtf16 [ '\n' as uint 16 ]
                  else Accept
                FindTextOnPage inText 0 (i+1)

            T_star ->
              block
                if inText && operandCount == 0
                  then EmitUtf16 [ '\n' as uint 16 ]
                  else Accept
                FindTextOnPage inText 0 (i+1)

            TJ ->
              block
                if inText && operandCount == 1
                  then
                    for (done = {}; x in (GetOperand (i - 1) is array))
                      block
                        case x of
                          string s -> DecodeText s
                          _        -> Accept
                        {}
                  else Accept
                FindTextOnPage inText 0 (i+1)

            Tf ->
              block
                if operandCount == 2
                  then
                    block
                      let fontName = GetOperand (i - 2) is name
                      let fontValue = Optional (Lookup fontName ?resources.fonts)
                      SelectFont fontValue
                  else Accept
                FindTextOnPage inText 0 (i+1)

            _  -> FindTextOnPage inText 0 (i+1)

        value _ ->
          FindTextOnPage inText (operandCount + 1) (i+1)

def DecodeText str =

  case CurrentFont of
    nothing -> Raw str

    just f ->
      case (f : Font).toUnicode of
        nothing ->
          if f.subType == "Type1" || f.subType == "Type3"
            then DecodeTextWithEncoding f str
            else Raw str

        just cmap ->
          case cmap of
            cmap c ->
              block
                let s = GetStream
                SetStream (arrayStream str)
                let units =
                  many (output = builder)
                    ParseUnicode output c
                EmitUtf16 (build units)
                SetStream s


def DecodeTextWithEncoding (f : Font) str =
  block
    let enc = case f.encoding of
                nothing  -> ?stdEncodings.std
                just enc -> enc
    for (done = {}; x in str)
      block
        case lookup x enc of
          just us -> EmitUtf16 us
          nothing -> EmitUtf16 [ '.' as uint 16 ]
        {}



def Raw (str : [uint 8]) =
  EmitUtf16 (map (x in str) (x as uint 16))

def ResetPage : {}
def BeginText : {}
def EndText : {}
def CurrentFont : maybe Font
def LoadFontByRef (encodings : StdEncodings) (r : Ref) : Font
def SetFont (font : maybe Font) : {}
def EmitUtf16 (text : [uint 16]) : {}
