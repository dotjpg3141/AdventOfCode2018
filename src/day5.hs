import AOC
import Data.Char
import Data.Maybe
import Control.Applicative

main :: IO ()
main = solving 5 strat1 strat2 parseInput

parseInput :: String -> String
parseInput = filter (not . isSpace)

strat1 :: String -> Int
strat1 = length . react

react :: String -> String
react xs = fromJust
    . last
    . takeWhile isJust
    $ iterate (>>= reactSingle) (Just xs)

reactSingle :: String -> Maybe String
reactSingle (a:b:xs) = if canReact a b
    then reactSingle xs <|> Just xs
    else (a:) <$> ( reactSingle $ b:xs )
reactSingle xs = Nothing

canReact :: Char -> Char -> Bool
canReact a b = toUpper a == toUpper b && isUpper a /= isUpper b

strat2 :: String -> String
strat2 = const ""