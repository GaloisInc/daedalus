module Utils where


zipSafe :: [a] -> [b] -> [(a,b)]
zipSafe as bs = case zip' as bs of
                  Nothing -> error "zipSafe"
                  Just xs -> xs
                  
-- zipWith' - ensures lists are of same length
zipWith'                  :: (a->b->c) -> [a]->[b]->Maybe [c]
zipWith' z (a:as) (b:bs)   = fmap (z a b :) (zipWith' z as bs)
zipWith' _ []     []       = Just []
zipWith' _ _      _        = Nothing

zip' :: [a] -> [b] -> Maybe [(a,b)]
zip' = zipWith' (,)

