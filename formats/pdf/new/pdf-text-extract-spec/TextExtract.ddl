import PdfValue
import PdfDecl
import StandardEncodings
import Catalog
import CMap
import ContentStream
import Fonts

def ExtractState =
  struct
    font      : maybe Font
    fontCache : [ Ref -> Font ]

def GetCharCode (cmap : cmap) : sint 32

def LookupCMap cmap =
  block
    let start = Offset
    let c = GetCharCode cmap
    let width = Offset - start
    let key = width <# (c as? uint 32)
    case Optional (Lookup key cmap.charMap) of
      just us ->
        if isInvalidUnicodeDestination us
          then Fail "Invalid Unicode destination in ToUnicode CMap"
          else @map (u in us) (EmitChar u)
      nothing -> EmitChar ('?' as ?auto)

def LookupCMapLoop (w : uint 64) (prevIx : uint 32) =
  block
    let c = prevIx <# UInt8
    let key = (w / 8 + 1) <# c
    case Optional (Lookup key ?cmap) of
      just us ->
        if isInvalidUnicodeDestination us
          then Fail "Invalid Unicode destination in ToUnicode CMap"
          else @map (u in us) (EmitChar u)
      nothing -> if w < 16 then LookupCMapLoop (w + 8) c
                           else Fail "Unknown character code"

-- ENTRY
def TextInCatalogPage
  (state : ExtractState)
  (c : PdfCatalog) : ExtractState =
  block
    let ?stdEncodings = c.stdEncodings
    TextInPageTree
      { font = nothing, fontCache = state.fontCache }
      c.pageTree

def TextInPageTree (state : ExtractState) (t : PdfPageTree) =
  case t of
    Node kids -> for (s = state; x in kids) (TextInPageTree s x)
    Leaf p    -> TextInPage state p

def TextInPage (state : ExtractState) (p : PdfPage) =
  case p of
    EmptyPage -> state
    ContentStreams content ->
      block
        let ?resources = content.resources
        TextInPageContnet state content

def TextInPageContnet (state : ExtractState) (p : PdfPageContent) =
  block
    let ?instrs = p.data
    FindTextOnPage state 0

def GetOperand i = (Index ?instrs i : ContentStreamEntry) is value

def SelectFont (state : ExtractState) mbValue : ExtractState =
  case mbValue of
    nothing -> { font = nothing, fontCache = state.fontCache }
    just value ->
      case value of
        ref r ->
          case lookup r state.fontCache of
            just font ->
              { font = just font, fontCache = state.fontCache }
            nothing ->
              block
                let f = Font value
                font = just f
                fontCache = insert r f state.fontCache
                
        _ ->
          { font = just (Font value), fontCache = state.fontCache }

def FindTextOnPage (state : ExtractState) i =
  case Optional (Index ?instrs i) of
    nothing -> state
    just instr ->
      case instr of

        operator op ->
          case op of

            Tj ->
              block
                DecodeText state.font (GetOperand (i - 1) is string)
                FindTextOnPage state (i+1)


            quote, dquote ->
              block
                EmitChar ('\n' as ?auto)
                DecodeText state.font (GetOperand (i - 1) is string)
                FindTextOnPage state (i+1)

            Td, TD, T_star ->
              block
                EmitChar ('\n' as ?auto)
                FindTextOnPage state (i+1)

            TJ ->
              block
                map (x in (GetOperand (i - 1) is array))  
                    case x of
                      string s -> DecodeText state.font s
                      _        -> Accept -- EmitChar (' ' as ?auto)

                FindTextOnPage state (i+1)

            Tf ->
              block
                let fontName = GetOperand (i - 2) is name
                let fontValue = Optional (Lookup fontName ?resources.fonts)
                let nextState = SelectFont state fontValue
                FindTextOnPage nextState (i+1)

            _  -> FindTextOnPage state (i+1)

        _  -> FindTextOnPage state (i+1)

def DecodeText mbFont str =

  case mbFont of
    nothing -> Raw str

    just f ->
      if f.subType == "Type1" || f.subType == "Type3"
        then DecodeTextWithEncoding f str
        else
          case (f : Font).toUnicode of
            nothing -> Raw str
            just cmap ->
              case cmap of
                named x -> Raw str

                cmap c ->
                  block
                    let s = GetStream
                    SetStream (arrayStream str)
                    Many (LookupCMap c)
                    SetStream s


def DecodeTextWithEncoding (f : Font) str =
  block
    let enc = case f.encoding of
                nothing  -> ?stdEncodings.std
                just enc -> enc
    @map (x in str)
       case lookup x enc of
         just us -> @map (u in us) (EmitChar (u as ?auto))
         nothing -> EmitChar ('.' as ?auto)



def Raw (str : [uint 8]) = @map (x in str) (EmitChar (x as ?auto))

-- Emit a character
def EmitChar (c : uint 32) : {}
