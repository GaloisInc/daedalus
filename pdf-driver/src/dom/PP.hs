{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}
{-# Language DataKinds #-}
module PP where

import GHC.Records(getField)
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.ByteString.Short(ShortByteString)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import Text.PrettyPrint
import Data.Char

import RTS.Vector(Vector)
import qualified RTS.Vector as Vector
import RTS.Numeric(UInt)
import RTS.Input
import PdfMonad
import PdfDecl
import PdfValue(Value(..), Ref(..), Number(..))
import PdfXRef(TrailerDict)

class PP t where
  pp :: t -> Doc

instance PP Value where
  pp val =
    case val of
      Value_bool b    -> if b then "true" else "false"
      Value_null {}   -> "null"
      Value_number n  -> pp n
      Value_ref r     -> pp r
      Value_array vs  -> ppArray vs
      Value_dict ds   -> ppDict ds
      Value_name xs   -> ppName True xs
      Value_string xs -> pp (Vector.vecToRep xs)

ppBlock :: Doc -> Doc -> [Doc] -> Doc
ppBlock open close ds =
  case ds of
    []     -> hcat [ open, close ]
    x : xs -> vcat ( (open <+> x)
                   : ["," <+> y | y <- xs ]
                  ++ [close]
                   )

ppTagged :: Bool -> Doc -> Doc -> Doc
ppTagged small t v =
  if small then hsep [ "{",  tag, v, "}" ]
           else vcat [ "{" <+> tag, nest 2 v, "}" ]
  where
  tag = hcat [ "$",t,colon]

ppHDict :: [Doc] -> Doc
ppHDict ds = "{" <+> hsep (punctuate comma ds) <+> "}"

ppArray :: Vector Value -> Doc
ppArray vs = ppBlock "[" "]" (map pp (Vector.toList vs))

ppDict :: Map (Vector (UInt 8)) Value -> Doc
ppDict mp = ppTagged False "dict" (ppBlock "{" "}" (map entry (Map.toList mp)))
  where
  entry (x,y) = hang (hcat [ ppName False x, colon ]) 2 (pp y)

ppName :: Bool -> Vector (UInt 8) -> Doc
ppName asVal xs = if asVal then ppTagged True "name" val else val
  where
  rep      = Vector.vecToRep xs
  normal c = isAscii c && (isAlphaNum c || c == '_')
  quote    = if not asVal && BS8.all normal rep then id else doubleQuotes
  val      = quote (text (BS8.unpack rep))


ppRef :: (PP a, PP b) => a -> b -> Doc
ppRef o g = ppTagged True "ref" (ppHDict [ "obj:" <+> pp o, "gen:" <+> pp g ])

ppXRef :: (R,ObjLoc) -> Doc
ppXRef (r,ol) =
  ppHDict $ [ "id:" <+> pp r
            ] ++ loc
  where
  loc = case ol of
          InFileAt x -> [ "offset:" <+> pp x ]
          InObj r i  -> [ "container:" <+> pp r <+> "ix:" <+> pp i ]

instance PP R where
  pp r = ppRef (refObj r) (refGen r)

instance PP Ref where
  pp ref = ppRef (getField @"obj" ref) (getField @"gen" ref)

instance PP Number where
  pp num
    | e >= 0    = pp (n * 10 ^ e)
    | n == 0    = "0"
    | otherwise =
      let str = show n
      in case splitAt (length str - fromInteger e) str of
           (as,bs) -> hcat [ text as, ".", text bs ]
    where
    e = getField @"exp" num
    n = getField @"num" num




instance PP TopDecl where
  pp td = ppBlock "{" "}"
            [ "obj:" <+> pp (getField @"id" td)
            , "gen:" <+> pp (getField @"gen" td)
            , payload ]
      where
      payload = case getField @"obj" td of
                  TopDeclDef_value v -> hang "value:" 2 (pp v)
                  TopDeclDef_stream s ->
                    hang "stream:" 2 (pp s)

instance PP Stream where
  pp str = ppBlock "{" "}"
              [ hang "header:" 2 (ppDict (getField @"header" str))
              , hang "body:" 2 body
              ]
    where
    body = case getField @"body" str of
             ApplyFilter_ok i           -> pp i
             ApplyFilter_unsupported {} -> "null"

instance PP Input where
  pp inp = pp (Base64.encode (inputBytes inp))

instance PP Int where
  pp = int

instance PP Integer where
  pp = integer

instance PP ShortByteString where
  pp xs = text (show xs)

instance PP ByteString where
  pp xs = text (show xs)

instance PP TrailerDict where
  pp td = ppDict (getField @"all" td)

instance PP ParseError where
  pp err = ppTagged False "error"
        $ ppBlock "{" "}"
          [ "input:" <+> pp (inputName inp)
          , "offset:" <+> pp (inputOffset inp)
          , "message:" <+> ppStr (peMsg err)
          , "grammar:" <+> "[" <+>
                  hsep (punctuate comma (map ppStr (peGrammar err))) <+> "]"
          , "context:" $$ nest 2 (ppBlock "[" "]" (map ppStr (peStack err)))
          ]
    where
    inp = peInput err
    ppStr = text . show
