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
    subType   = LookupName "Subtype" dict
    encoding  = GetEncoding dict
    toUnicode = case lookup "ToUnicode" dict of
                  nothing -> nothing
                  just v  -> just (UnicodeCMap v)

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
