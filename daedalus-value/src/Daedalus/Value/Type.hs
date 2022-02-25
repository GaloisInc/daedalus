{-# Language OverloadedStrings, DataKinds #-}
module Daedalus.Value.Type where

import GHC.Float

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Vector(Vector)
import qualified Data.Vector as Vector
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Word(Word8)
import Data.Char (isAscii, isPrint, chr)
import Numeric(showHex)

import Daedalus.PP hiding (empty)
import Daedalus.Range
import Daedalus.Panic(panic)
import RTS.Input(Input(..),inputName,inputOffset,inputLength)

-- | Value universe
data Value =
    VUInt !Int {- nbits -} !Integer
  | VSInt !Int {- nbits -} !Integer
  | VInteger               !Integer
  | VBool                  !Bool
  | VFloat                 !Float
  | VDouble                !Double
  | VUnionElem             !Label !Value
  | VStruct                ![(Label,Value)]
  | VBDStruct !BDStruct    !Integer
  | VBDUnion  !BDUnion     !Integer
  | VArray                 !(Vector Value)
  | VMaybe                 !(Maybe Value)
  | VMap                   !(Map Value Value)
  | VStream                !Input
  | VBuilder               ![Value]   -- array builder
  | VIterator              ![(Value,Value)]
    deriving Show

data BDStruct = BDStruct
   { bdName       :: !Text
   , bdWidth      :: !Int
   , bdGetField   :: !(Label -> Integer -> Value)
   , bdStruct     :: !([(Label,Value)] -> Integer)
   , bdValid      :: !(Integer -> Bool)
   , bdFields     :: ![Label]
   }

data BDUnion = BDUnion
  { bduName       :: !Text
  , bduWidth      :: !Int
  , bduValid      :: !(Integer -> Bool)
  , bduGet        :: !(Label   -> Integer -> Value)
  , bduMatches    :: !(Label   -> Integer -> Bool)
  , bduCases      :: ![Label]
  }

instance Show BDStruct where
  show _ = "BDStruct"

instance Show BDUnion where
  show _ = "BDUnion"

type Label = Text
type Partial = Either String    -- ^ values with errors

vErr :: String -> Partial a     -- ^ create an error value
vErr = Left

-- | Values of types
data TValue =
    TVInteger
  | TVUInt !Int
  | TVSInt !Int
  | TVNum  !Int  -- of kind number, needed to pass as param
  | TVFloat
  | TVDouble
  | TVArray
  | TVMap
  | TVBDStruct BDStruct
  | TVBDUnion  BDUnion
  | TVOther
    deriving Show


vUnit :: Value
vUnit = VStruct []

vByte :: Word8 -> Value
vByte x = VUInt 8 (fromIntegral x)

vByteString :: ByteString -> Value
vByteString = VArray . Vector.fromList . map vByte . BS.unpack

vFloat :: Float -> Value
vFloat = VFloat

vDouble :: Double -> Value
vDouble = VDouble

vUInt :: Int -> Integer -> Value
vUInt w i = VUInt w (i `mod` (snd (uintRange w) + 1))

vSInt :: Int -> Integer -> Partial Value
vSInt w x
  | inRange (sintRange w) x = pure (VSInt w x)
  | otherwise               = vErr (show x ++ " does not fit in sint" ++ show w)

-- | Used for coercion.  Make a value only using the lowest `n` bits.
vSInt' :: Int -> Integer -> Value
vSInt' w x = VSInt w (mod (x - lb) (ub - lb + 1) + lb)
  where
  (lb,ub) = sintRange w

vSize :: Integer -> Value
vSize = vUInt 64

vFromBits :: TValue -> Integer -> Value
vFromBits t i =
  case t of
    TVUInt n      -> vUInt n i
    TVSInt n      -> vSInt' n i
    TVBDStruct bd -> VBDStruct bd (clampTo (bdWidth bd) i)
    TVBDUnion bd  -> VBDUnion bd (clampTo (bduWidth bd) i)
    TVFloat       -> VFloat  (castWord32ToFloat  (fromInteger i))
    TVDouble      -> VDouble (castWord64ToDouble (fromInteger i))
    _             -> panic "vFromBits" [ "Value cannot be made from bits"
                                       , showPP t
                                       , show i ]

  where
  clampTo w j = mod j (snd (uintRange w) + 1)

vToBits :: Value -> Integer
vToBits v =
  case v of
    VUInt _ i     -> i
    VSInt _ i     -> i
    VBDStruct _ i -> i
    VBDUnion  _ i -> i
    VFloat f      -> toInteger (castFloatToWord32 f)
    VDouble f     -> toInteger (castDoubleToWord64 f)
    _             -> panic "vFromBits" [ "Value cannot be converted to bits"
                                       , showPP v ]

--------------------------------------------------------------------------------
-- Dynamic checks on values

-- | Extract the value out of a numeric type
valueToIntegral :: Value -> Integer
valueToIntegral val =
  case val of
    VInteger i -> i
    VUInt _ i  -> i
    VSInt _ i  -> i
    _          -> panic "valueToIntegral" [ "Not an integral value", show val ]

valueToBool :: Value -> Bool
valueToBool val =
  case val of
    VBool b -> b
    _       -> panic "valueToBool" [ "Not a boolean", show val ]

valueToMaybe :: Value -> Maybe Value
valueToMaybe val =
  case val of
    VMaybe mb -> mb
    _         -> panic "ValueToMaybe" [ "Not a maybe", show val ]

valueToByte :: Value -> Word8
valueToByte val =
  case val of
    VUInt 8 b -> fromInteger b
    _         -> panic "valueToByte" [ "Not a byte", show val ]

valueToVector :: Value -> Vector Value
valueToVector v =
 case v of
   VArray vs -> vs
   _         -> panic "valueToVector" [ "Not a vector", show v ]

valueToStruct :: Value -> [(Label,Value)]
valueToStruct v =
  case v of
    VStruct fs -> fs
    _          -> panic "valueToStruct" [ "Not a struct", show v ]

valueToUnion :: Value -> (Label,Value)
valueToUnion v =
  case v of
    VUnionElem l e -> (l,e)
    _              -> panic "valueToUnion" [ "Not a union", show v ]

valueToByteString :: Value -> ByteString
valueToByteString v =
  case v of
    VArray vs -> BS.pack (map valueToByte (Vector.toList vs))
    _         -> panic "valueToByteString" [ "Not a bytesring", show v ]

valueToMap :: Value -> Map Value Value
valueToMap v =
  case v of
    VMap m -> m
    _      -> panic "valueToMap" [ "Not a map", show v ]

valueToStream :: Value -> Input
valueToStream v =
  case v of
    VStream i -> i
    _         -> panic "valueToStream" [ "Not a stream", show v ]

valueToBuilder :: Value -> [Value]
valueToBuilder v =
  case v of
    VBuilder xs -> xs
    _           -> panic "valueToBuilder" [ "Not a builder", show v ]

valueToIterator :: Value -> [(Value,Value)]
valueToIterator v =
  case v of
    VIterator xs -> xs
    _            -> panic "valueToIterator" [ "Not an iterator", show v ]

valueToSize :: Value -> Integer
valueToSize v =
  case v of
    VUInt 64 i -> i
    _          -> panic "valueToSize" [ "Not a size", show v ]

valueToIntSize :: Value -> Maybe Int
valueToIntSize = integerToInt . valueToSize

--------------------------------------------------------------------------------

-- NOTE: this does not do a little bit of type-checking anymore. It is
-- useful to keep the comparison total for LL(*) which uses
-- hashconsing with maps on values
vCompare :: Value -> Value -> Ordering
vCompare a b =
  case (a,b) of
    (VUInt n x,      VUInt n' y)            -> compare (n,x) (n',y)
    (VUInt _ _,      _)                     -> LT
    (_,              VUInt _ _)             -> GT

    (VSInt n x,      VSInt n' y)            -> compare (n,x) (n',y)
    (VSInt _ _,      _)                     -> LT
    (_ ,             VSInt _ _)             -> GT

    (VInteger x,     VInteger y)            -> compare x y
    (VInteger _,     _)                     -> LT
    (_,              VInteger _)            -> GT

    (VBool x,        VBool y)               -> compare x y
    (VBool _,        _)                     -> LT
    (_,              VBool _)               -> GT

    (VFloat x,        VFloat y)               -> compare x y
    (VFloat _,        _)                     -> LT
    (_,              VFloat _)               -> GT

    (VDouble x,        VDouble y)               -> compare x y
    (VDouble _,        _)                     -> LT
    (_,              VDouble _)               -> GT

    (VUnionElem p x, VUnionElem q y)        -> compare (p,x) (q,y)
    (VUnionElem _ _, _)                     -> LT
    (_,              VUnionElem _ _)        -> GT

    (VStruct xs,     VStruct ys)            -> compare (Map.fromList xs)
                                                       (Map.fromList ys)
    (VStruct _,      _)                     -> LT
    (_,              VStruct _)             -> GT

    (VBDStruct _ x,  VBDStruct _ y)         -> compare x y
    (VBDStruct {},   _)                     -> LT
    (_,              VBDStruct {})          -> GT

    (VBDUnion _ x,   VBDUnion _ y)          -> compare x y
    (VBDUnion {},    _)                     -> LT
    (_,              VBDUnion {})           -> GT

    (VArray xs,      VArray ys)             -> compare xs ys
    (VArray _,       _)                     -> LT
    (_,              VArray _)              -> GT

    (VMaybe x,       VMaybe y)              -> compare x y
    (VMaybe _,       _)                     -> LT
    (_,              VMaybe _)              -> GT

    (VMap x,         VMap y)                -> compare x y
    (VMap _,         _)                     -> LT
    (_,              VMap _)                -> GT

    (VStream x,      VStream y)             -> compare x y
      -- WARNING: Only by name, see Input!
    (VStream _,      _)                     -> LT
    (_,              VStream _)             -> GT

    (VBuilder xs,    VBuilder ys)           -> compare xs ys
    (VBuilder {},    _)                     -> LT
    (_,             VBuilder {})            -> GT

    (VIterator xs,    VIterator ys)         -> compare xs ys
    -- (VIterator {},    _)                     -> LT
    -- (_,             VIterator {})            -> GT


instance Eq Value where
  x == y = vCompare x y == EQ

instance Ord Value where
  compare = vCompare

--------------------------------------------------------------------------------
instance PP TValue where
  ppPrec _ tv =
    case tv of
      TVInteger     -> "integer"
      TVUInt n      -> "uint" <.> int n
      TVSInt n      -> "sint" <.> int n
      TVFloat       -> "float"
      TVDouble      -> "double"
      TVNum n       -> int n
      TVArray       -> "array"
      TVMap         -> "map"
      TVBDStruct bd -> pp (bdName bd)
      TVBDUnion bd  -> pp (bduName bd)
      TVOther       -> "other"

ppRawBD :: Int -> Integer -> Doc
ppRawBD w i = "0x" <.> padding <.> text txt <.> brackets (int w)
  where
  txt     = showHex i ""
  padding = let p = div (w - 4 * length txt + 1) 4
            in text (replicate p '0')

instance PP Value where
  ppPrec n val =
    case val of
      VUInt 8 x
        | isAscii c && isPrint c -> quotes (char (chr (fromInteger x)))
          where c = toEnum (fromInteger x)
      VUInt nb x -> pp x <> "[" <> pp nb <> "]"
      VSInt nb x -> pp x <> "[S" <> pp nb <> "]"
      VInteger x -> pp x
      VFloat x   -> pp x
      VDouble x  -> pp x
      VBool b    -> if b then "T" else "F"
      VUnionElem lbl v -> braces (pp lbl <.> colon <+> pp v)
      VStruct xs      -> block "{" "," "}" (map ppF xs)
        where ppF (x,t) = pp x <.> colon <+> pp t

      VBDStruct t x ->
        hang (ppRawBD (bdWidth t) x) 2
        $ block "{" "," "}"
         [ pp l <.> colon <+> pp (bdGetField t l x) | l <- bdFields t ]

      VBDUnion t x ->
        hang (ppRawBD (bduWidth t) x) 2 (braces (pp lbl <.> colon <+> pp v))
        where
        views = [ (l, bduGet t l x)
                | l <- bduCases t, bduMatches t l x ]
        (lbl,v) = case views of
                    a : _ -> a
                    []    -> panic "pp@Value"
                               [ "Invalid bitdata value"
                               , "type: " ++ Text.unpack (bduName t)
                               , "value: " ++ show x
                               ]


      VArray v -> case vs of
                    VUInt 8 _ : _ ->
                      text (show (BS.pack [ fromInteger x | VUInt _ x <- vs ]))
                    _ -> block "[" "," "]" (map pp vs)
        where vs = Vector.toList v

      VMaybe v   -> case v of
                      Nothing -> "Nothing"
                      Just v' -> wrapIf (n > 0) ("Just" <+> ppPrec 1 v')
      VMap m -> block "{|" ", " "|}"
                [ ppPrec 1 k <+> "->" <+> ppPrec 1 v | (k,v) <- Map.toList m ]

      VStream i -> text (show i)

      VBuilder vs -> block "[builder|" ",       " "|]" (map pp vs)
      VIterator vs -> block "[iterator|" ",        " "|]"
                             [ pp x <+> "->" <+> pp y | (x,y) <- vs ]


-- | Render a value as JSON
valueToJS :: Value -> Doc
valueToJS val =
  case val of
    VUInt _ n  -> integer n
    VSInt _ n  -> integer n
    VInteger n -> integer n

    VBool b    -> if b then "true" else "false"

    VFloat f   -> float f
    VDouble f  -> double f

    VUnionElem l v -> tagged (pp l) (valueToJS v)

    VStruct vs ->
      jsBlock "{" "," "}"
       [ text (show (show (pp l))) <.> colon <+> valueToJS v | (l,v) <- vs ]

    VBDStruct t x -> jsBlock "{" "," "}"
                     [ lab l <+> valueToJS (bdGetField t l x)
                     | l <- bdFields t ]
      where
      lab l = text (show (show (pp l))) <.> colon

    VBDUnion t x -> tagged tag (valueToJS v)
      where
      views = [ (pp l, bduGet t l x)
              | l <- bduCases t, bduMatches t l x ]
      (tag,v) = case views of
                  g : _ -> g
                  []    -> panic "valueToJS"
                            [ "Invalid value"
                            , "bitdata: " ++ Text.unpack (bduName t)
                            , "value: " ++ show x
                            ]

    VArray vs -> jsBlock "[" "," "]" (map valueToJS (Vector.toList vs))

    VMap mp -> tagged "$map" $ jsBlock "[" "," "]" (map pair (Map.toList mp))

    VMaybe mb ->
      case mb of
        Nothing -> "null"
        Just v  -> tagged "$just" (valueToJS v)

    VStream inp -> tagged "$input"
                 $ text $ show
                 $ toStr (inputName inp) ++ ":" ++ rng
      where
      rng = "0x" ++ showHex (inputOffset inp) (
          "--0x" ++ showHex (inputOffset inp + inputLength inp) "")
      toStr = map (toEnum . fromEnum) . SBS.unpack

    VBuilder xs -> tagged "$$builder" $ jsBlock "[" "," "]" (map valueToJS xs)
    VIterator xs -> tagged "$$iterator" $ jsBlock "[" "," "]" (map pair xs)

  where
  pair (a,b) = "[" <+> valueToJS a <.> "," <+> valueToJS b <+> "]"

  tagged t v = "{ \"$" <.> t <.> "\":" <+> v <+> "}"

  jsBlock open separ close ds =
    case ds of
      []  -> open <.> close
      [d] -> open <+> d <+> close
      _   -> vcat $ [ s <+> d | (s,d) <- zip (open : repeat separ) ds ] ++
                    [ close ]
