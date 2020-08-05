{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}
{-# Language DataKinds #-}
module PP where

import GHC.Records(getField)
import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Text.PrettyPrint
import Data.Char

import RTS.Vector(Vector)
import qualified RTS.Vector as Vector
import RTS.Numeric(UInt)
import PdfValue(Value(..), Ref(..), Number(..))

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
      Value_name xs   -> ppName False xs
      Value_string xs -> text (show (Vector.vecToRep xs))

ppArray :: Vector Value -> Doc
ppArray vs =
  case map pp (Vector.toList vs) of
    []      -> "[]"
    x : xs -> vcat ( ("[" <+> x)
                   : ["," <+> y | y <- xs ]
                  ++ ["]"]
                   )

ppDict :: Map (Vector (UInt 8)) Value -> Doc
ppDict mp =
  case map entry (Map.toList mp) of
    []      -> "{}"
    x : xs  -> vcat ( ("{" <+> x)
                    : ["," <+> y | y <- xs ]
                   ++ ["}"]
                    )
  where
  entry (x,y) = hang (hcat [ ppName True x, colon ]) 2 (pp y)



ppName :: Bool -> Vector (UInt 8) -> Doc
ppName skipQuotes xs
  | skipQuotes && BS8.all normal rep = text ('$' : BS8.unpack rep)
  | otherwise                        = quotes (text ('$' : BS8.unpack rep))
  where
  rep      = Vector.vecToRep xs
  normal c = isAscii c && (isAlphaNum c || c == '_')



instance PP Ref where
  pp ref = braces
         $ hsep
         $ punctuate comma
           [ "obj:" <+> integer (getField @"obj" ref)
           , "gen:" <+> integer (getField @"gen" ref)
           ]

instance PP Number where
  pp num
    | e >= 0    = integer (n * 10 ^ e)
    | n == 0    = "0"
    | otherwise =
      let str = show n
      in case splitAt (length str - fromInteger e) str of
           (as,bs) -> hcat [ text as, ".", text bs ]
    where
    e = getField @"exp" num
    n = getField @"num" num


-- instance PP TopDeclDef where

