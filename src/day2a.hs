import qualified Data.List as List

main :: IO ()
main = interact $ show . checksum . words

hasDuplicate :: (Eq a, Ord a) => Int -> [a] -> Bool
hasDuplicate n = any (\xs -> length xs == n) . List.group . List.sort

checksumPart :: Int -> [String] -> Int
checksumPart n = length . filter ( hasDuplicate n )

checksum :: [String] -> Int
checksum xs = ( checksumPart 2 xs ) * ( checksumPart 3 xs )