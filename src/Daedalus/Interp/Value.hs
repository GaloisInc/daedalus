{-# Language OverloadedStrings, ViewPatterns #-}
module Daedalus.Interp.Value where

import Data.Vector(Vector)
import qualified Data.Vector as Vector
import Data.Map(Map)
import qualified Data.Map as Map
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.Word(Word8)
import Data.Char (isAscii, isPrint, chr)
import Data.Bits(shiftL, (.&.))

import Daedalus.PP hiding (empty)
import Daedalus.Type.AST hiding (Value)
import RTS.ParserAPI(Input(..))

data Value =
    VUInt !Int {- nbits -} !Integer
  | VSInt !Int {- nbits -} !Integer
  | VInteger !Integer
  | VBool !Bool
  | VUnionElem !Label !Value
  | VStruct ![ (Label,Value) ]
  | VArray !(Vector Value)
  | VMaybe !(Maybe Value)
  | VMap   !(Map Value Value)
  | VStream !Input
  deriving (Show, Eq, Ord)

data TVal = TVInteger
          | TVUInt !Int
          | TVSInt !Int
          | TVNum  !Int     -- of kind number, needed to pass as param
          | TVArray
          | TVMap
          | TVOther
            deriving Show



mkUInt :: Int -> Integer -> Value
mkUInt n v = VUInt n (v .&. ((1 `shiftL` n ) - 1))

-- FIXME: correct?  maybe we need to sign extend.
mkSInt :: Int -> Integer -> Value
mkSInt n v = VSInt n (v .&. ((1 `shiftL` n ) - 1))



--------------------------------------------------------------------------------
-- Dynamic checks on values

valueToInteger :: Value -> Integer
valueToInteger v =
  case v of
    VUInt _ n -> n
    VSInt _ n -> n
    VInteger n -> n
    _ -> error ("BUG: valueToInteger: expecting an Integer, got " ++ show v)
{-# INLINE valueToInteger #-}

valueToBool :: Value -> Bool
valueToBool val =
  case val of
    VBool b -> b
    _       -> error "BUG: valueToBool expecting a bool"
{-# INLINE valueToBool #-}

valueToMaybe :: Value -> Maybe Value
valueToMaybe val =
  case val of
    VMaybe mb -> mb
    _ -> error "BUG: valueToMaybe expecting a maybe"
{-# INLINE valueToMaybe #-}


valueToByte :: Value -> Word8
valueToByte (VUInt 8 b) = fromInteger b
valueToByte v = error ("BUG: expecting a UInt8, got " ++ show v)
{-# INLINE valueToByte #-}

valueToVector :: Value -> Vector Value
valueToVector v =
 case v of
   VArray vs -> vs
   _ -> error ("BUG: `concat` applied to non-array")
{-# INLINE valueToVector #-}

valueToStruct :: Value -> [(Label,Value)]
valueToStruct v =
  case v of
    VStruct fs -> fs
    _ -> error "BUG: expected a struct"
{-# INLINE valueToStruct #-}

valueToUnion :: Value -> (Label,Value)
valueToUnion v =
  case v of
    VUnionElem l e -> (l,e)
    _ -> error "BUG: expected a union element"
{-# INLINE valueToUnion #-}

valueToByteString :: Value -> ByteString
valueToByteString v =
  case v of
    VArray vs -> BS.pack (map valueToByte (Vector.toList vs))
    _         -> error "BUG: expected a bytestring"
{-# INLINE valueToByteString #-}

valueToMap :: Value -> Map Value Value
valueToMap v =
  case v of
    VMap m -> m
    _ -> error "BUG: expected a map"
{-# INLINE valueToMap #-}

valueToStream :: Value -> Input
valueToStream v =
  case v of
    VStream i -> i
    _ -> error "BUG: expected a stream"

--------------------------------------------------------------------------------

doCoerceTo :: TVal -> Value -> (Value,Lossy)
doCoerceTo tgt v =
  case v of

    VUInt _ n ->
      case tgt of
        TVInteger -> (VInteger n,  NotLossy)
        TVUInt st -> retNum n (mkUInt st n)
        TVSInt st -> retNum n (mkSInt st n)
        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VSInt _ n ->
      case tgt of
        TVInteger -> (VInteger n,  NotLossy)
        TVUInt st -> retNum n (mkUInt st n)
        TVSInt st -> retNum n (mkSInt st n)
        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VInteger n ->
      case tgt of
        TVInteger -> (VInteger n, NotLossy)
        TVUInt st -> retNum n (mkUInt st n)
        TVSInt st -> retNum n (mkSInt st n)
        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VBool {}      -> (v, NotLossy)
    VUnionElem {} -> (v, NotLossy)
    VStruct {}    -> (v, NotLossy)
    VMap {}       -> (v, NotLossy)
    VStream {}    -> (v, NotLossy)
    VArray {}     -> (v, NotLossy)
    VMaybe {}     -> (v, NotLossy)



  where
  lossy orig val = if orig == val then NotLossy else Lossy

  retNum orig val = (val, case val of
                            VInteger _  -> NotLossy
                            VUInt _ va  -> lossy orig va
                            VSInt _ va  -> lossy orig va
                            _           -> error "BUG: not a number")

  bug = error "BUG: cannot coerce"

--------------------------------------------------------------------------------
instance PP TVal where
  ppPrec _ tv =
    case tv of
      TVInteger -> "integer"
      TVUInt n  -> "uint" <.> int n
      TVSInt n  -> "sint" <.> int n
      TVNum n   -> int n
      TVArray   -> "array"
      TVMap     -> "map"
      TVOther   -> "other"

instance PP Value where
  ppPrec n val =
    case val of
      VUInt 8 x
        | isAscii c && isPrint c -> quotes (char (chr (fromInteger x)))
          where c = toEnum (fromInteger x)
      VUInt nb x -> pp x <> "[" <> pp nb <> "]"
      VSInt nb x -> pp x <> "[S" <> pp nb <> "]"
      VInteger x -> pp x
      VBool b    -> if b then "T" else "F"
      VUnionElem lbl v -> braces (pp lbl <+> colon <+> pp v)
      VStruct xs      -> block "{" "," "}" (map ppF xs)
        where ppF (x,t) = pp x <.> colon <+> pp t

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

      VStream i -> brackets $
                  "stream|" <+>
                  "off:" <+> pp (inputOffset i) <+>
                  "len:" <+> pp (BS.length (inputBytes i))

