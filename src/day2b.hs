main :: IO()
main = interact $ (\xs -> show $ head [ common x y | x <- xs, y <- xs, differBy1 x y ] ) . words

differBy1 :: (Eq a) => [a] -> [a] -> Bool
differBy1 [] [] = False
differBy1 [] _  = False
differBy1 _  [] = False
differBy1 (x:xs) (y:ys) = 
    if x == y
        then differBy1 xs ys
        else xs == ys

common :: (Eq a) => [a] -> [a] -> [a]
common xs [] = []
common [] ys = []
common (x:xs) (y:ys) =
    if x == y
        then x:(common xs ys)
        else (common xs ys)
