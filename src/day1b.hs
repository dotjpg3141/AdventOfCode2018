import qualified Data.Set as Set

main :: IO ()
main = interact $ show . firstDuplicate . scanl (+) 0 . cycle . map parseInt . words

parseInt :: String -> Int
parseInt ('+':xs) = read xs
parseInt xs       = read xs

firstDuplicate :: (Ord a) => [a] -> Maybe a
firstDuplicate = firstDuplicateWith Set.empty

firstDuplicateWith :: (Ord a) => Set.Set a -> [a] -> Maybe a
firstDuplicateWith seen []     = Nothing
firstDuplicateWith seen (x:xs) = 
    if x `Set.member` seen
        then Just x
        else firstDuplicateWith (x `Set.insert` seen) xs