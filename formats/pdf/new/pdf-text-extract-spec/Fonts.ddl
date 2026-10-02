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
    let subType = LookupName "Subtype" dict
    subType   = subType
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
    cidFont = if subType == "Type0"
                then just (GetCIDFont dict)
                else nothing

def FontByRef (r : Ref) = Font {| ref = r |}

def GetCIDFont (dict : Dict) =
  block
    let descendants = ResolveVal (Lookup "DescendantFonts" dict) is array
    length descendants == 1 is true
    let descendant = ResolveVal (Index descendants 0) is dict
    let descriptor = GetFontDescriptor descendant
    subType = LookupName "Subtype" descendant
    encoding = GetCIDEncoding dict
    defaultWidth =
      case lookup "DW" descendant of
        nothing -> 1000
        just v  -> NumberAsDouble (ResolveVal v is number)
    widths = GetCIDWidths descendant
    ascent = GetDescriptorNumber "Ascent" descriptor
    descent = GetDescriptorNumber "Descent" descriptor
    fontBBox = GetFontBBox descendant descriptor

def GetCIDEncoding (dict : Dict) =
  Optional
    block
      let encodingName = ResolveVal (Lookup "Encoding" dict) is name
      encodingName == "Identity-H" is true
      {| identityH = {} |}

def cidWidth =
  union
    Consecutive: (uint 32, [double])
    Range:       (uint 32, double)

def GetCIDWidths (dict : Dict) : [uint 32 -> cidWidth] =
  case lookup "W" dict of
    nothing -> empty
    just v  ->
      block
        let values = ResolveVal v is array
        let result =
          many
            (state =
              { index = 0
              , widths = empty : [uint 32 -> cidWidth]
              })
            block
              state.index < length values is true
              ParseCIDWidth values state
        result.index == length values is true
        result.widths

def ParseCIDWidth values state =
  block
    let first =
      NumberAsNat
        (ResolveVal (Index values state.index) is number) as? uint 32
    case ResolveVal (Index values (state.index + 1)) of
      array ws ->
        block
          length ws > 0 is true
          let widths =
            map (w in ws) (NumberAsDouble (ResolveVal w is number))
          let last =
            ((first as uint 64) + length widths - 1) as? uint 32
          index = state.index + 2
          -- The specification says that a CID should not be specified more
          -- than once. We do not specially handle malformed overlapping
          -- entries.
          widths =
            insert first
              ({| Consecutive = (last, widths) |} : cidWidth)
              state.widths

      number lastNumber ->
        block
          let last  = NumberAsNat lastNumber as? uint 32
          first <= last is true
          let width =
            NumberAsDouble
              (ResolveVal (Index values (state.index + 2)) is number)
          index  = state.index + 3
          -- See the overlap note above.
          widths =
            insert first
              ({| Range = (last, width) |} : cidWidth)
              state.widths

      _ -> Fail "Invalid CID font W entry"

def GetFirstChar (dict : Dict) : maybe (uint 8) =
  case lookup "FirstChar" dict of
    nothing -> nothing
    just v  -> just (NumberAsNat (ResolveVal v is number) as? uint 8)

def GetWidths (dict : Dict) : maybe [double] =
  case lookup "Widths" dict of
    nothing -> nothing
    just v ->
      just
        (map (width in (ResolveVal v is array))
          (NumberAsDouble (ResolveVal width is number)))

def GetFontDescriptor (dict : Dict) : maybe Dict =
  Optional (ResolveVal (Lookup "FontDescriptor" dict) is dict)

def GetDescriptorNumber
  (key : [uint 8])
  (descriptor : maybe Dict) : maybe double =
  Optional
    (NumberAsDouble
      (ResolveVal (Lookup key (descriptor is just)) is number))

def GetFontBBox
  (font : Dict)
  (descriptor : maybe Dict) : maybe [double] =
  Optional
    First
      GetBBox (Lookup "FontBBox" font)
      GetBBox (Lookup "FontBBox" (descriptor is just))

def GetBBox value : [double] =
  block
    let values = ResolveVal value is array
    length values == 4 is true
    map (coordinate in values)
      (NumberAsDouble (ResolveVal coordinate is number))

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
