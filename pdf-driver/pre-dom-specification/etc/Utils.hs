{-# LANGUAGE EmptyDataDecls, TypeOperators, LambdaCase #-}

module Utils where

type a :+: b = Either a b


mMapLeft :: Monad m => (a-> m c) -> a :+: b -> m (c :+: b)
mMapLeft f = (\a-> Left <$> f a) `either` (return . Right)

mMapRight :: Monad m => (b-> m c) -> a :+: b -> m (a :+: c)
mMapRight f = (return . Left) `either` (\b-> Right <$> f b) 

-- sub - a safe version of (!!)
sub :: [a] -> Int -> Maybe a
sub (x:_)  0       = Just x
sub (_:xs) n | n>0 = sub xs (n-1)
sub _      _       = Nothing

safeIndex = sub

stub = error "stub"

notImplementedYet = error "not implemented"

warn :: [Char] -> IO ()
warn s = error s
