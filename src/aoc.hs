module AOC 
    ( solving
    , mapFst
    , mapSnd
    ) where

solving :: (Show b, Show c) => (a -> b) -> (a -> c) -> (String -> a) -> IO ()
solving s1 s2 input = interact $ solve . input
    where solve x = unlines [ "Part A", show (s1 x), "Part B", show (s2 x) ]

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)