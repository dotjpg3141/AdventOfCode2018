module AOC 
    ( solving
    ) where

solving :: (Show b, Show c) => (a -> b) -> (a -> c) -> (String -> a) -> IO ()
solving s1 s2 input = interact $ solve . input
    where solve x = unlines [ "Part A", show (s1 x), "Part B", show (s2 x) ]