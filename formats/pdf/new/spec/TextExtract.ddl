import Debug
import PdfValue
import PdfDecl
import StandardEncodings
import Catalog
import CMap
import ContentStream
import Fonts

def GetCharCode (cmap : cmap) : sint 32

def LookupCMap cmap =
  block
    let c = GetCharCode cmap
    EmitChar (Lookup (c as? uint 32) cmap.charMap <| ('?' as ?auto))

def LookupCMapLoop (w : uint 64) (prevIx : uint 32) =
  block
    let c = prevIx <# UInt8
    case Optional (Lookup c ?cmap) of
      just v  -> EmitChar v
      nothing -> if w < 16 then LookupCMapLoop (w + 8) c
                           else Fail "Unknown character code"

def TextInCatalog (c : PdfCatalog) =
  block
    let ?stdEncodings = c.stdEncodings
    @(TextInPageTree nothing c.pageTree)

def TextInPageTree acc (t : PdfPageTree) =
  case t of
    Node kids -> for (s = acc; x in kids) (TextInPageTree s x)
    Leaf p    -> TextInPage acc p

def TextInPage acc (p : PdfPage) =
  case p of
    EmptyPage -> acc
    ContentStreams content ->
      block
        let ?resources = content.resources
        TextInPageContnet acc content

def TextInPageContnet acc (p : PdfPageContent) =
  block
    let ?instrs = p.data
    FindTextOnPage acc 0

def GetOperand i = (Index ?instrs i : ContentStreamEntry) is value

def FindTextOnPage acc i =
  case Optional (Index ?instrs i) of
    nothing -> acc
    just instr ->
      case instr of

        operator op ->
          case op of

            Tj ->
              block
                DecodeText acc (GetOperand (i - 1) is string)
                FindTextOnPage acc (i+1)


            quote, dquote ->
              block
                EmitChar ('\n' as ?auto)
                DecodeText acc (GetOperand (i - 1) is string)
                FindTextOnPage acc (i+1)

            Td, TD, T_star ->
              block
                EmitChar ('\n' as ?auto)
                FindTextOnPage acc (i+1)

            TJ ->
              block
                map (x in (GetOperand (i - 1) is array))  
                    case x of
                      string s -> DecodeText acc s
                      _        -> Accept -- EmitChar (' ' as ?auto)

                FindTextOnPage acc (i+1)

            Tf ->
              block
                let fontName = GetOperand (i - 2) is name
                let font     = Optional (Lookup fontName ?resources.fonts)
                FindTextOnPage font (i+1)

            _  -> FindTextOnPage acc (i+1)

        _  -> FindTextOnPage acc (i+1)

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


--------------------------------------------------------------------------------
-- Just used for the type (lists/builders should be a standard type)
def List P =
  First
    cons = Node P
    nil  = Accept

def Node P =
  block
    head = P
    tail = List P

def nil : List = {| nil |}
def push x xs : List = {| cons = { head = x, tail = xs } |}

def reverse acc (xs : List) =
  case xs of
    nil    -> acc
    cons n -> reverse (push n.head acc) n.tail

def DumpStream (s : Stream) =
  block
    SetStream (s.body is ok)
    Many UInt8


