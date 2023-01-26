{-# Language RecordWildCards, DataKinds, RankNTypes, OverloadedStrings #-}
{-# Language TypeFamilies #-}
module RTS.ParserAPI (module RTS.ParserAPI, Input) where

import Control.Monad(when, unless, replicateM_)
import Data.Word
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(isJust)
import Data.Char(isPrint)
import Data.ByteString(ByteString)
import Numeric(showHex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Daedalus.RTS.Numeric
import Daedalus.RTS.Vector(Vector,VecElem)
import Daedalus.RTS.Input
import qualified Daedalus.RTS.Vector as Vector
import Daedalus.RTS.InputTrace
import RTS.ParseError

import Debug.Trace(traceM)


data ResultG e a =
    NoResults (ParseErrorG e)
  | Results (NonEmpty (a,InputTrace))
    deriving Show

instance Functor (ResultG e) where
  fmap f r =
    case r of
      NoResults e -> NoResults e
      Results xs  -> Results ((\(x,a) -> (f x,a)) <$> xs)


data ErrorMode = Abort | Fail
  deriving Show



--------------------------------------------------------------------------------
-- | Representation of character classes.
data ClassVal = ClassVal (Word8 -> Bool) String

bcNone :: ClassVal
bcNone = ClassVal (const False) "no byte"

bcAny :: ClassVal
bcAny = ClassVal (const True) "a byte"

bcSingle :: UInt 8 -> ClassVal
bcSingle c = ClassVal (fromUInt c ==) ("byte " ++ showByte (fromUInt c))

bcComplement :: ClassVal -> ClassVal
bcComplement (ClassVal p x) = ClassVal (not . p) ("not (" ++ x ++ ")")

bcUnion :: ClassVal -> ClassVal -> ClassVal
bcUnion (ClassVal p x) (ClassVal q y) =
  ClassVal (\c -> p c || q c) ("(" ++ x ++ ")" ++ ", or (" ++ y ++ ")")

-- XXX: we could sort the BS and do binary search
bcByteString :: ByteString -> ClassVal
bcByteString bs = ClassVal (`BS.elem` bs) ("one of " ++ show bs)

bcDiff :: ClassVal -> ClassVal -> ClassVal
bcDiff (ClassVal p x) (ClassVal q y) =
  ClassVal (\c -> p c && not (q c)) ("in (" ++ x ++ "), but not (" ++ y ++ ")")

bcRange :: UInt 8 -> UInt 8 -> ClassVal
bcRange x' y' = ClassVal (\c -> x <= c && c <= y)
              ("between " ++ showByte x ++ " and " ++ showByte y)
  where x = fromUInt x'
        y = fromUInt y'

showByte :: Word8 -> String
showByte x
  | isPrint c = show c
  | otherwise =
    case showHex x "" of
      [d] -> "0x0" ++ [d]
      d   -> "0x" ++ d
  where c = toEnum (fromEnum x)

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

class Monad p => BasicParser p where
  type Annot p
  (|||)     :: p a -> p a -> p a
  (<||)     :: p a -> p a -> p a
  pFail     :: ParseErrorG (Annot p) -> p a
  pByte     :: SourceRange -> p Word8
  pEnter    :: Annot p -> p a -> p a
  pStack    :: p [Annot p]
  pITrace   :: p InputTrace
  pSetITrace :: InputTrace -> p ()
  pPeek     :: p Input
  pSetInput :: Input -> p ()
  pErrorMode :: ErrorMode -> p a -> p a

-- | Are we at the end
pEnd :: BasicParser p => SourceRange -> p ()
pEnd r =
  do inp <- pPeek
     case inputByte inp of
       Nothing -> pure ()
       Just (_,_) -> pErrorAt FromSystem [r] inp "unexpected left over input"
{-# INLINE pEnd #-}


-- | Get the current location in the input
pOffset :: BasicParser p => p (UInt 64)
pOffset = intToSize . inputOffset <$> pPeek
{-# INLINE pOffset #-}

-- | Check if a byte satisfies a predicate
pMatch1 :: BasicParser p => SourceRange -> ClassVal -> p Word8
pMatch1 erng (ClassVal p str) =
  do inp <- pPeek
     b   <- pByte erng
     unless (p b)
       $ pErrorAt FromSystem [erng] inp
       $ unwords ["byte", showByte b, "does not match", str]
     pure b
{-# INLINE pMatch1 #-}



pTrace :: BasicParser p => Vector (UInt 8) -> p ()
pTrace msg =
  do off <- pOffset
     traceM (show off ++ ": " ++ BS8.unpack (Vector.vecToRep msg))

traceScope :: BasicParser p => p v -> p (v,InputTrace)
traceScope p =
  do i   <- pITrace
     pSetITrace emptyInputTrace
     a <- p
     j <- pITrace
     pSetITrace (unionInputTrace i j)
     pure (a,j)



pMatch :: BasicParser p => SourceRange -> Vector (UInt 8) -> p (Vector (UInt 8))
pMatch = \r bs ->
  do let check _i b =
           do loc <- pPeek
              b1 <- pByte r
              let byte = showByte (fromUInt b)
                  msg = "expected " ++ byte ++ " while matching " ++ show bs
              unless (b == uint8 b1) (pErrorAt FromSystem [r] loc msg)
     -- XXX: instad of matching one at a time we should check for prefix
     -- and advance the stream?
     Vector.imapM_ check bs
     pure bs
{-# INLINE pMatch #-}



pError' :: BasicParser p => ParseErrorSource -> [SourceRange] -> String -> p a
pError' src rs m =
  do i <- pPeek
     s <- pStack
     t <- pITrace
     pFail PE { peInput   = i
              , peStack   = s
              , peGrammar = rs
              , peMsg     = m
              , peSource  = src
              , peMore    = Nothing
              , peNumber  = -1
              , peITrace  = t
              }
{-# INLINE pError' #-}

pError :: BasicParser p => ParseErrorSource -> SourceRange -> String -> p a
pError src r m = pError' src [r] m
{-# INLINE pError #-}


pErrorAt ::
  BasicParser p => ParseErrorSource -> [SourceRange] -> Input -> String -> p a
pErrorAt src r inp m =
  do s <- pStack
     t <- pITrace
     pFail PE { peInput   = inp
              , peStack   = s
              , peGrammar = r
              , peMsg     = m
              , peSource  = src
              , peMore    = Nothing
              , peNumber  = -1
              , peITrace  = t
              }
{-# INLINE pErrorAt #-}

-- | Check that the vector has at least that many elements.
pMinLength :: (VecElem a, BasicParser p) =>
              SourceRange -> UInt 64 -> p (Vector a) -> p (Vector a)
pMinLength rng need p =
  do as <- p
     let have = Vector.length as
     unless (have >= need) $
       pError FromSystem rng
         $ "Not enough entries, found " ++ show have ++ ", but need at least " ++ show need
     pure as

type Commit p = forall a. p a -> p a -> p a

pOptional :: BasicParser p => Commit p -> (a -> Maybe b) -> p a -> p (Maybe b)
pOptional orElse mk e = orElse (mk <$> e) (pure Nothing)
{-# INLINE pOptional #-}


pLoopMany :: BasicParser p => Commit p -> (a -> p a) -> a -> p a
pLoopMany orElse p s =
  do mb <- pOptional orElse Just (p s)
     case mb of
       Nothing -> pure s
       Just s1 -> pLoopMany orElse p s1

pMany :: (VecElem a, BasicParser p) => Commit p -> p a -> p (Vector a)
pMany orElse = \p ->
  let step _ = pOptional orElse ok p
      ok x   = Just (x,())
  in Vector.unfoldrM step ()
{-# INLINE pMany #-}

pManyUpTo :: (VecElem a, BasicParser p) =>
            Commit p -> UInt 64 -> p a -> p (Vector a)
pManyUpTo orElse = \limI p ->
  do let lim = sizeToInt limI
         step n = if n < lim then pOptional orElse (ok n) p else pure Nothing
         ok n x = Just (x,n+1)

     Vector.unfoldrM step 0
{-# INLINE pManyUpTo #-}


pSkipMany :: BasicParser p => Commit p -> p () -> p ()
pSkipMany orElse = \p -> let go = orElse (p >> go) (pure ())
                         in go
{-# INLINE pSkipMany #-}

pSkipManyUpTo :: BasicParser p => Commit p -> UInt 64 -> p () -> p ()
pSkipManyUpTo orElse limI p = go 0
  where
  lim  = sizeToInt limI
  go n = when (n < lim) (orElse (p >> go (n+1)) (pure ()))


pSkipExact :: BasicParser p => UInt 64 -> p () -> p ()
pSkipExact limI p =
  do let lim = sizeToInt limI
     replicateM_ lim p
{-# INLINE pSkipExact #-}


pSkipAtLeast :: BasicParser p => Commit p -> UInt 64 -> p () -> p ()
pSkipAtLeast orElse = \limI p ->
  do pSkipExact limI p
     pSkipMany orElse p
{-# INLINE pSkipAtLeast #-}

pSkipWithBounds :: BasicParser p =>
  SourceRange -> Commit p -> UInt 64 -> UInt 64 -> p () -> p ()
pSkipWithBounds erng orElse lb ub p
  | lb > ub = pError FromSystem erng "Inconsitent bounds"
  | otherwise = pSkipExact lb p >> pSkipManyUpTo orElse (ub `sub` lb) p
{-# INLINE pSkipWithBounds #-}


pIsJust :: BasicParser p => SourceRange -> String -> Maybe a -> p a
pIsJust erng msg = \mb -> case mb of
                            Nothing -> pError FromSystem erng msg
                            Just a  -> pure a
{-# INLINE pIsJust #-}

pIsJust_ :: BasicParser p => SourceRange -> String -> Maybe a -> p ()
pIsJust_ erg msg = pGuard erg msg . isJust
{-# INLINE pIsJust_ #-}

pGuard :: BasicParser p => SourceRange -> String -> Bool -> p ()
pGuard erng msg = \b -> unless b (pError FromSystem erng msg)
{-# INLINE pGuard #-}

