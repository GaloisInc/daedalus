{-# Language RecordWildCards, DataKinds, RankNTypes, OverloadedStrings #-}
module RTS.ParserAPI where

import Control.Exception
import Control.Monad(when, unless, guard, replicateM_)
import Data.Word
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(isJust)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Text.PrettyPrint hiding ((<>))

import RTS.Numeric
import RTS.Vector(Vector,VecElem)
import qualified RTS.Vector as Vector

data Result a = NoResults ParseError
              | Results (NonEmpty a)

instance Functor Result where
  fmap f r =
    case r of
      NoResults e -> NoResults e
      Results xs  -> Results (f <$> xs)


--------------------------------------------------------------------------------
type SourceRange = String
--------------------------------------------------------------------------------


data ErrorMode = Abort | Fail
  deriving Show

--------------------------------------------------------------------------------
-- | This is the representation of a stream.
-- XXX: Add more info about where the stram came from.
data Input = Input
  { inputBytes  :: {-# UNPACK #-} !ByteString
  , inputOffset :: !Int
    -- ^ Index of next character
  }

instance Eq Input where
  x == y = view x == view y
    where view i = (inputOffset i, BS.length (inputBytes i))

instance Ord Input where
  compare x y = compare (view x) (view y)
    where view i = (inputOffset i, BS.length (inputBytes i))

instance Show Input where
  show Input { .. } =
    "Stream { off = " ++ show inputOffset ++
           ", len = " ++ show (BS.length inputBytes) ++
           "}"

-- | Limit the input to the given number of bytes.
-- Fails if there aren't enough bytes.
limitLen :: Integer -> Input -> Maybe Input
limitLen n' i =
  do n <- toInt n'
     let bs = inputBytes i
     guard (0 <= n && n <= BS.length bs)
     pure i { inputBytes = BS.take n bs }
{-# INLINE limitLen #-}


-- | Advance the input by the ginve number of bytes.
-- Fails if we don't have enough bytes, although it is ok to
-- get to the very end of the input.
advanceBy :: Integer -> Input -> Maybe Input
advanceBy n' i =
  do n <- toInt n'
     let bs = inputBytes i
     guard (0 <= n && n <= BS.length bs)
     pure Input { inputBytes  = BS.drop n bs
                , inputOffset = inputOffset i + n
                }
{-# INLINE advanceBy #-}


arrayStream :: Vector (UInt 8) -> Input
arrayStream v = Input { inputOffset = 0, inputBytes = Vector.vecToRep v }
{-# INLINE arrayStream #-}

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Representation of character classes.
data ClassVal = ClassVal (Word8 -> Bool) String

bcNone :: ClassVal
bcNone = ClassVal (const False) "no byte"

bcAny :: ClassVal
bcAny = ClassVal (const True) "a byte"

bcSingle :: UInt 8 -> ClassVal
bcSingle c = ClassVal (fromUInt c ==) ("byte " ++ show c)

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
              ("between " ++ show x ++ " and " ++ show y)
  where x = fromUInt x'
        y = fromUInt y'

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
data ParseError = PE { peOffset  :: !Int
                     , peStack   :: ![String]
                     , peGrammar :: ![SourceRange]
                     , peMsg     :: !String
                     , peSource  :: !ParseErrorSource
                     , peMore    :: !(Maybe ParseError)
                     } deriving Show

data ParseErrorSource = FromUser | FromSystem
  deriving Show

instance Exception ParseError

instance Semigroup ParseError where
  p1 <> p2
    | peOffset p1 < peOffset p2 = p2
    | peOffset p2 < peOffset p1 = p1
    | otherwise = case peMore p1 of
                    Nothing -> p1 { peMore = Just p2 }
                    Just p1' ->
                      case peMore p2 of
                        Nothing -> p2 { peMore = Just p1 }
                        Just _  -> p1 { peMore = Just $! joinErr p1' p2 }

joinErr :: ParseError -> ParseError -> ParseError
joinErr = (<>)

ppParseError :: ParseError -> Doc
ppParseError PE { .. } =
  brackets ("offset:" <+> int peOffset) $$
  nest 2 (bullets
           [ text peMsg, gram
           , "context:" $$ nest 2 (bullets (reverse (map text peStack)))
           ]
         )
    $$ more
  where
  gram = case peGrammar of
           [] -> empty
           _  -> "see grammar at:" <+> commaSep (map text peGrammar)
  more = case peMore of
           Nothing -> empty
           Just err -> ppParseError err

  bullet      = if True then "•" else "*"
  buletItem d = bullet <+> d
  bullets ds  = vcat (map buletItem ds)
  commaSep ds = hsep (punctuate comma ds)



--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

class Monad p => BasicParser p where
  (|||)     :: p a -> p a -> p a
  (<||)     :: p a -> p a -> p a
  pFail     :: ParseError -> p a
  pByte     :: SourceRange -> p Word8
  pEnter    :: String -> p a -> p a
  pStack    :: p [String]
  pPeek     :: p Input
  pSetInput :: Input -> p ()
  pErrorMode :: ErrorMode -> p a -> p a

  pOffset   :: p Int    -- more generally, describes current loc. in input
  pEnd      :: SourceRange -> p ()     -- are we at the end
  pMatch1   :: SourceRange -> ClassVal -> p Word8


pMatch :: BasicParser p => SourceRange -> Vector (UInt 8) -> p (Vector (UInt 8))
pMatch = \r bs ->
  do Input { .. } <- pPeek
     let check i b =
           do b1 <- pByte r
              unless (b == uint8 b1) (pErrorAt FromSystem [r] (inputOffset + i)
                                                    ("expected " ++ show bs) )
     Vector.imapM_ check bs
     pure bs
{-# INLINE pMatch #-}



pError' :: BasicParser p => ParseErrorSource -> [SourceRange] -> String -> p a
pError' src rs m =
  do off <- pOffset
     s   <- pStack
     pFail PE { peOffset  = off - 1
              , peStack   = s
              , peGrammar = rs
              , peMsg     = m
              , peSource  = src
              , peMore    = Nothing
              }
{-# INLINE pError' #-}

pError :: BasicParser p => ParseErrorSource -> SourceRange -> String -> p a
pError src r m = pError' src [r] m
{-# INLINE pError #-}


pErrorAt ::
  BasicParser p => ParseErrorSource -> [SourceRange] -> Int -> String -> p a
pErrorAt src r off m =
  do s <- pStack
     pFail PE { peOffset  = off
              , peStack   = s
              , peGrammar = r
              , peMsg     = m
              , peSource  = src
              , peMore    = Nothing
              }
{-# INLINE pErrorAt #-}

-- | Check that the vector has at least that many elements.
pMinLength :: (VecElem a, BasicParser p) =>
              SourceRange -> Integer -> p (Vector a) -> p (Vector a)
pMinLength rng n p =
  do as <- p
     unless (toInteger (Vector.length as) >= n) $
       pError FromSystem rng "Not enough entries"
     pure as

type Commit p = forall a. p a -> p a -> p a

pOptional :: BasicParser p => Commit p -> (a -> Maybe b) -> p a -> p (Maybe b)
pOptional orElse mk e = orElse (mk <$> e) (pure Nothing)
{-# INLINE pOptional #-}



pMany :: (VecElem a, BasicParser p) => Commit p -> p a -> p (Vector a)
pMany orElse = \p ->
  let step _ = pOptional orElse ok p
      ok x   = Just (x,())
  in Vector.unfoldrM step ()
{-# INLINE pMany #-}

pManyUpTo :: (VecElem a, BasicParser p) =>
            Commit p -> Integer -> p a -> p (Vector a)
pManyUpTo orElse = \limI p ->
  do let (short,lim) = case toInt limI of
                         Just i  -> (False,i)
                         Nothing -> (True,maxBound)

         step n = if n < lim then pOptional orElse (ok n) p else pure Nothing
         ok n x = Just (x,n+1)

     v <- Vector.unfoldrM step 0

     -- we probably already run out of memory...
     when (short && Vector.length v == maxBound)
          $ error ("pManyUpTo: Vector length exceeded maximum")
     pure v
{-# INLINE pManyUpTo #-}


pSkipMany :: BasicParser p => Commit p -> p () -> p ()
pSkipMany orElse = \p -> let go = orElse (p >> go) (pure ())
                         in go
{-# INLINE pSkipMany #-}

pSkipManyUpTo :: BasicParser p => Commit p -> Integer -> p () -> p ()
pSkipManyUpTo orElse limI p = go 0
  where
  (short,lim) = case toInt limI of
                  Just i  -> (False, i)
                  Nothing -> (True, maxBound)

  go n = if n < lim then orElse (p >> go (n+1)) done else done
  done = if short then pSkipManyUpTo orElse (limI - toInteger lim) p
                  else pure ()


pSkipExact :: BasicParser p => Integer -> p () -> p ()
pSkipExact limI p =
  do let (short,lim) = case toInt limI of
                         Just i  -> (False,i)
                         Nothing -> (True,maxBound)
     replicateM_ lim p
     when short (pSkipExact (limI - toInteger lim) p)
{-# INLINE pSkipExact #-}


pSkipAtLeast :: BasicParser p => Commit p -> Integer -> p () -> p ()
pSkipAtLeast orElse = \limI p ->
  do pSkipExact limI p
     pSkipMany orElse p
{-# INLINE pSkipAtLeast #-}

pSkipWithBounds :: BasicParser p =>
  SourceRange -> Commit p -> Integer -> Integer -> p () -> p ()
pSkipWithBounds erng orElse lb ub p
  | lb > ub = pError FromSystem erng "Inconsitent bounds"
  | otherwise = pSkipExact lb p >> pSkipManyUpTo orElse (ub - lb) p
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

