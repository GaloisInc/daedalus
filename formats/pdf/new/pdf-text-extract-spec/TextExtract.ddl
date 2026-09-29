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
    output    : builder (uint 16)

-- ENTRY
def TextInCatalogPage
  (state : ExtractState)
  (c : PdfCatalog) : ExtractState =
  block
    let ?stdEncodings = c.stdEncodings
    TextInPageTree
      { font = nothing, fontCache = state.fontCache, output = state.output }
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
    nothing ->
      { font = nothing, fontCache = state.fontCache, output = state.output }
    just value ->
      case value of
        ref r ->
          case lookup r state.fontCache of
            just font ->
              { font = just font
              , fontCache = state.fontCache
              , output = state.output
              }
            nothing ->
              block
                let f = Font value
                font = just f
                fontCache = insert r f state.fontCache
                output = state.output
                
        _ ->
          { font = just (Font value)
          , fontCache = state.fontCache
          , output = state.output
          }

def FindTextOnPage (state : ExtractState) i =
  case Optional (Index ?instrs i) of
    nothing -> state
    just instr ->
      case instr of

        operator op ->
          case op of

            Tj ->
              block
                let next = DecodeText state (GetOperand (i - 1) is string)
                FindTextOnPage next (i+1)


            quote, dquote ->
              block
                let next = EmitUtf16 state [ '\n' as uint 16 ]
                let next = DecodeText next (GetOperand (i - 1) is string)
                FindTextOnPage next (i+1)

            Td, TD, T_star ->
              block
                let next = EmitUtf16 state [ '\n' as uint 16 ]
                FindTextOnPage next (i+1)

            TJ ->
              block
                let next =
                  for (next = state; x in (GetOperand (i - 1) is array))
                    case x of
                      string s -> DecodeText next s
                      _        -> next
                FindTextOnPage next (i+1)

            Tf ->
              block
                let fontName = GetOperand (i - 2) is name
                let fontValue = Optional (Lookup fontName ?resources.fonts)
                let nextState = SelectFont state fontValue
                FindTextOnPage nextState (i+1)

            _  -> FindTextOnPage state (i+1)

        _  -> FindTextOnPage state (i+1)

def DecodeText (state : ExtractState) str =

  case state.font of
    nothing -> Raw state str

    just f ->
      if f.subType == "Type1" || f.subType == "Type3"
        then DecodeTextWithEncoding state f str
        else
          case (f : Font).toUnicode of
            nothing -> Raw state str
            just cmap ->
              case cmap of
                cmap c ->
                  block
                    let s = GetStream
                    SetStream (arrayStream str)
                    let units =
                      many (output = builder)
                        ParseUnicode output c
                    let next = EmitUtf16 state (build units)
                    SetStream s
                    next


def DecodeTextWithEncoding (state : ExtractState) (f : Font) str =
  block
    let enc = case f.encoding of
                nothing  -> ?stdEncodings.std
                just enc -> enc
    for (next = state; x in str)
      case lookup x enc of
        just us -> EmitUtf16 next us
        nothing -> EmitUtf16 next [ '.' as uint 16 ]



def Raw (state : ExtractState) (str : [uint 8]) =
  EmitUtf16 state (map (x in str) (x as uint 16))

def EmitUtf16 (state : ExtractState) (text : [uint 16]) : ExtractState =
  { font = state.font
  , fontCache = state.fontCache
  , output = emitArray state.output text
  }
