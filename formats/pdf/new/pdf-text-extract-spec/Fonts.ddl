import PdfValue
import PdfDecl
import StandardEncodings
import CMap

def GetFonts (r : Dict) : [ [uint 8] -> Value ] =
  case Optional (Lookup "Font" r) of
    nothing -> empty
    just v  -> ResolveVal v is dict

def Font (v : Value) =
  block
    let dict  = ResolveVal v is dict
    let descriptor = GetFontDescriptor dict
    subType   = LookupName "Subtype" dict
    encoding  = GetEncoding dict
    toUnicode = case lookup "ToUnicode" dict of
                  nothing -> nothing
                  just v  -> just (UnicodeCMap v)
    firstChar = GetFirstChar dict
    widths = GetWidths dict
    missingWidth = GetDescriptorNumber "MissingWidth" descriptor
    ascent = GetDescriptorNumber "Ascent" descriptor
    descent = GetDescriptorNumber "Descent" descriptor
    fontBBox = GetFontBBox dict descriptor

def FontByRef (r : Ref) = Font {| ref = r |}

def GetFirstChar (dict : Dict) : maybe (uint 8) =
  case lookup "FirstChar" dict of
    nothing -> nothing
    just v ->
      just (NumberAsNat (ResolveVal v is number) as? uint 8)

def GetWidths (dict : Dict) : maybe [Number] =
  case lookup "Widths" dict of
    nothing -> nothing
    just v ->
      just
        (map (width in (ResolveVal v is array))
          (ResolveVal width is number))

def GetFontDescriptor (dict : Dict) : maybe Dict =
  Optional (ResolveVal (Lookup "FontDescriptor" dict) is dict)

def GetDescriptorNumber
  (key : [uint 8])
  (descriptor : maybe Dict) : maybe Number =
  Optional
    (ResolveVal (Lookup key (descriptor is just)) is number)

def GetFontBBox
  (font : Dict)
  (descriptor : maybe Dict) : maybe [Number] =
  Optional
    First
      GetBBox (Lookup "FontBBox" font)
      GetBBox (Lookup "FontBBox" (descriptor is just))

def GetBBox value : [Number] =
  block
    let values = ResolveVal value is array
    length values == 4 is true
    map (coordinate in values) (ResolveVal coordinate is number)

def namedEncoding encName =
  case encName of
    "WinAnsiEncoding"  -> just stdEncodings.win
    "MacRomanEncoding" -> just stdEncodings.mac
    "StandardEncoding" -> just stdEncodings.std
    _                  -> nothing


def GetEncoding (d : Dict) : maybe [uint 8 -> [uint 16]] =
  case lookup "Encoding" d of
    nothing -> nothing
    just v ->
     block
      case ResolveVal v of
        name encName -> namedEncoding encName
        dict encD ->
          block
            let base = case lookup "BaseEncoding" encD of
                         just ev -> namedEncoding (ResolveVal ev is name)
                         nothing -> nothing
            case lookup "Differences" encD of
              nothing -> base
              just d  -> just (EncodingDifferences base (ResolveVal d is array))

        _ -> nothing


def EncodingDifferences base (ds : [Value]) : [ uint 8 -> [uint 16] ]=
  block
    let start = case base of
                  nothing -> stdEncodings.std
                  just e  -> e
    let s = for (s = { enc = start, code = 0 : uint 16 }; x in ds)
             case ResolveVal x of
               number n ->
                 block
                   enc  = s.enc
                   code = (NumberAsNat n as? uint 8) as uint 16
               name x ->
                 block
                   let code = s.code as? uint 8
                   let enc = case lookup x stdEncodings.uni of
                               just u  -> insert code u s.enc
                               nothing ->
                                 block
                                   -- Trace (concat ["Missing: ", x])
                                   insert code
                                     (map (c in concat ["[",x,"]"])
                                          (c as uint 16)) s.enc
                   { enc = enc, code = s.code + 1 }

    s.enc
