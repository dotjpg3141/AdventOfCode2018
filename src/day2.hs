import AOC
import qualified Data.List as List

main :: IO ()
main = solving strat1 strat2 id

strat1 :: String -> Int
strat1 = checksum . words

hasDuplicate :: (Eq a, Ord a) => Int -> [a] -> Bool
hasDuplicate n = any (\xs -> length xs == n) . List.group . List.sort

checksumPart :: Int -> [String] -> Int
checksumPart n = length . filter ( hasDuplicate n )

checksum :: [String] -> Int
checksum xs = ( checksumPart 2 xs ) * ( checksumPart 3 xs )

strat2 :: String -> String
strat2 = (\xs -> head [ common x y | x <- xs, y <- xs, differBy1 x y ] ) . words

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