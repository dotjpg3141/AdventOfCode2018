main :: IO ()
main = interact $ show . foldl1 (+) . map parseInt . words

parseInt :: String -> Int
parseInt ('+':xs) = read xs
parseInt xs       = read xs