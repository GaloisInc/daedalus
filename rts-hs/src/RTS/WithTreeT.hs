-- | A parser that builds a form of parse tree.
{-# Language GeneralizedNewtypeDeriving #-}
module RTS.WithTreeT
  ( WithTreeT
  , runWithTreeT
  , liftWithTree
  , Trie (..)
  , showTrie
  , emptyTrie
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(fromMaybe)

import RTS.ParserAPI
import RTS.Numeric(sizeToInt)
import RTS.StateT
import RTS.ByteRanges


newtype WithTreeT m a = P (StateT Trie m a)
  deriving (Functor,Applicative,Monad)

runWithTreeT :: Trie -> WithTreeT m a -> m (a, Trie)
runWithTreeT t (P p) = runStateT t p

instance BasicParser m => BasicParser (WithTreeT m) where
  P x ||| P y         = P (x ||| y)
  P x <|| P y         = P (x <|| y)
  pFail e             = P (pFail e)
  pByte r             = do record
                           P (pByte r)
  pEnter e (P m)      = P (pEnter e m)
  pStack              = P pStack
  pPeek               = P pPeek
  pSetInput i         = P (pSetInput i)
  pErrorMode e (P m)  = P (pErrorMode e m)

  pOffset             = P pOffset
  pEnd r              = P (pEnd r)
  pMatch1 r x         = do record
                           P (pMatch1 r x)

liftWithTree :: Monad m => m a -> WithTreeT m a
liftWithTree m = P $ liftS m

record :: BasicParser m => WithTreeT m ()
record =
  do x <- pOffset
     s <- pStack
     P $ sets_ $ insert (sizeToInt x) (reverse s) -- XXX: lots of reversing

----------------------------------------------------------------
data Trie = Trie Ranges (Map String Trie)

showTrie :: Trie -> [String]
showTrie (Trie xs' mp) =
  map (".." ++) $ if null xs then rest
                             else unwords (map show xs) : rest
  where
  rest = [ l | (x,y) <- Map.toList mp , l <- x : showTrie y ]
  xs = rangesToList xs'

emptyTrie :: Trie
emptyTrie = Trie noRanges Map.empty

insert :: Int -> [String] -> Trie -> Trie
insert v ks (Trie xs mp) =
  case ks of
    [] -> Trie (insertRange v v xs) mp
    k : more -> Trie xs (Map.alter add k mp)
        where add = Just . insert v more . fromMaybe emptyTrie


