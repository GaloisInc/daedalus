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

def SelectFont fontSize mbValue =
  case mbValue of
    nothing -> SetFont fontSize nothing
    just value ->
      case value of
        ref r -> SetFont fontSize (just (LoadFontByRef ?stdEncodings r))
        _ -> SetFont fontSize (just (Font value))

def GetMatrixOperands i =
  block
    a = GetOperand (i - 6) is number
    b = GetOperand (i - 5) is number
    c = GetOperand (i - 4) is number
    d = GetOperand (i - 3) is number
    e = GetOperand (i - 2) is number
    f = GetOperand (i - 1) is number

def FindTextOnPage
  (inText : bool)
  (operandCount : uint 64)
  (i : uint 64) : {} =
  case Optional (Index ?instrs i) of
    nothing -> if inText then NoteMalformedOperator else Accept
    just instr ->
      case instr of

        operator op ->
          case op of

            -- Save the current graphics state.
            q ->
              block
                Operator
                  block
                    operandCount == 0 is true
                    SaveGraphicsState
                FindTextOnPage inText 0 (i+1)

            -- Restore the most recently saved graphics state.
            Q ->
              block
                Operator
                  block
                    operandCount == 0 is true
                    RestoreGraphicsState
                FindTextOnPage inText 0 (i+1)

            -- Concatenate a matrix with the current transformation matrix.
            cm ->
              block
                Operator
                  block
                    operandCount == 6 is true
                    let m = GetMatrixOperands i
                    ConcatMatrix m.a m.b m.c m.d m.e m.f
                FindTextOnPage inText 0 (i+1)

            -- Begin a text object and initialize its text matrices.
            BT ->
              block
                First
                  block
                    inText is false
                    operandCount == 0 is true
                  NoteMalformedOperator
                BeginText
                FindTextOnPage true 0 (i+1)

            -- End the current text object.
            ET ->
              block
                Operator
                  block
                    inText is true
                    operandCount == 0 is true
                FindTextOnPage false 0 (i+1)

            -- Set the spacing added after each character.
            Tc ->
              block
                Operator
                  block
                    operandCount == 1 is true
                    SetCharacterSpacing (GetOperand (i - 1) is number)
                FindTextOnPage inText 0 (i+1)

            -- Set the additional spacing applied to word spaces.
            Tw ->
              block
                Operator
                  block
                    operandCount == 1 is true
                    SetWordSpacing (GetOperand (i - 1) is number)
                FindTextOnPage inText 0 (i+1)

            -- Set horizontal text scaling as a percentage.
            Tz ->
              block
                Operator
                  block
                    operandCount == 1 is true
                    SetHorizontalScaling
                      (GetOperand (i - 1) is number)
                FindTextOnPage inText 0 (i+1)

            -- Set the vertical distance used to move to the next text line.
            TL ->
              block
                Operator
                  block
                    operandCount == 1 is true
                    SetLeading (GetOperand (i - 1) is number)
                FindTextOnPage inText 0 (i+1)

            -- Set the text rendering mode.
            Tr ->
              block
                Operator
                  block
                    operandCount == 1 is true
                    let mode =
                      NumberAsNat
                        (GetOperand (i - 1) is number) as? uint 8
                    mode <= 7 is true
                    SetRenderingMode mode
                FindTextOnPage inText 0 (i+1)

            -- Set the vertical displacement of text from the baseline.
            Ts ->
              block
                Operator
                  block
                    operandCount == 1 is true
                    SetTextRise (GetOperand (i - 1) is number)
                FindTextOnPage inText 0 (i+1)

            -- Replace the text matrix and text-line matrix.
            Tm ->
              block
                Operator
                  block
                    inText is true
                    operandCount == 6 is true
                    let m = GetMatrixOperands i
                    SetTextMatrix m.a m.b m.c m.d m.e m.f
                FindTextOnPage inText 0 (i+1)

            -- Show a text string at the current text position.
            Tj ->
              block
                Operator
                  block
                    inText is true
                    operandCount == 1 is true
                    DecodeText (GetOperand (i - 1) is string)
                FindTextOnPage inText 0 (i+1)

            -- Move to the next line and show a text string.
            quote ->
              block
                Operator
                  block
                    inText is true
                    operandCount == 1 is true
                    DecodeText (GetOperand (i - 1) is string)
                FindTextOnPage inText 0 (i+1)

            -- Set word and character spacing, move to the next line, and
            -- show a text string.
            dquote ->
              block
                Operator
                  block
                    inText is true
                    operandCount == 3 is true
                    DecodeText (GetOperand (i - 1) is string)
                FindTextOnPage inText 0 (i+1)

            -- Move the text position to the start of another line.
            Td, TD ->
              block
                Operator
                  block
                    inText is true
                    operandCount == 2 is true
                FindTextOnPage inText 0 (i+1)

            -- Move to the start of the next text line.
            T_star ->
              block
                Operator
                  block
                    inText is true
                    operandCount == 0 is true
                FindTextOnPage inText 0 (i+1)

            -- Show strings with interspersed positioning adjustments.
            TJ ->
              block
                Operator
                  block
                    inText is true
                    operandCount == 1 is true
                    let xs = GetOperand (i - 1) is array
                    for (done = {}; x in xs)
                      case x of
                        string text -> DecodeText text
                        number adjustment -> AdjustTextPosition adjustment
                        _ -> NoteMalformedOperator
                FindTextOnPage inText 0 (i+1)

            -- Select the text font and font size.
            Tf ->
              block
                Operator
                  block
                    operandCount == 2 is true
                    let fontName = GetOperand (i - 2) is name
                    let fontSize = GetOperand (i - 1) is number
                    let fontValue = Lookup fontName ?resources.fonts
                    SelectFont fontSize (just fontValue)
                FindTextOnPage inText 0 (i+1)

            _  -> FindTextOnPage inText 0 (i+1)

        value _ ->
          FindTextOnPage inText (operandCount + 1) (i+1)

def Operator P =
  First
    P
    NoteMalformedOperator

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
      case lookup x enc of
        just us -> EmitUtf16 us
        nothing -> EmitUtf16 [ '.' as uint 16 ]



def Raw (str : [uint 8]) =
  EmitUtf16 (map (x in str) (x as uint 16))

def ResetPage : {}
def SaveGraphicsState : {}
def RestoreGraphicsState : {}
def ConcatMatrix
  (a : Number) (b : Number) (c : Number)
  (d : Number) (e : Number) (f : Number) : {}
def BeginText : {}
def SetCharacterSpacing (value : Number) : {}
def SetWordSpacing (value : Number) : {}
def SetHorizontalScaling (value : Number) : {}
def SetLeading (value : Number) : {}
def SetRenderingMode (value : uint 8) : {}
def SetTextRise (value : Number) : {}
def SetTextMatrix
  (a : Number) (b : Number) (c : Number)
  (d : Number) (e : Number) (f : Number) : {}
def AdjustTextPosition (value : Number) : {}
def NoteMalformedOperator : {}
def CurrentFont : maybe Font
def LoadFontByRef (encodings : StdEncodings) (r : Ref) : Font
def SetFont (fontSize : Number) (font : maybe Font) : {}
def EmitUtf16 (text : [uint 16]) : {}
