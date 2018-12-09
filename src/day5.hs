import AOC
import Data.Char
import Data.Maybe
import Control.Applicative
import qualified Data.Set as Set

main :: IO ()
main = solving 5 strat1 strat2 parseInput

parseInput :: String -> String
parseInput = filter (not . isSpace)

strat1 :: String -> Int
strat1 = length . react

react :: String -> String
react = reverse . flip react' []

react' :: String -> String -> String
react' []     ys = ys
react' (x:xs) [] = (react' xs [x])
react' (x:xs) (y:ys) = if canReact x y
    then react' xs ys
    else react' xs (x:y:ys)

canReact :: Char -> Char -> Bool
canReact a b = toUpper a == toUpper b && isUpper a /= isUpper b

strat2 :: String -> Int
strat2 xs = minimum
          . map (\c -> length . react $ filter ( (/= c) . toLower ) xs )
          . Set.toList
          . Set.fromList
          . map toLower
          $ xs