module Day3Common
    ( Claim (..)
    , parseClaim
    , module AOC
    ) where
import AOC
import Data.List.Split
import Data.Char

data Claim = Claim {
    indet :: Int,
    x :: Int,
    y :: Int,
    w :: Int,
    h :: Int
} deriving (Show, Ord, Eq)

parseClaim :: String -> Claim
parseClaim xs = case parseNumbers xs of
    [ ident, x, y, w, h ]  -> Claim ident x y w h
    _                      -> error $ "Invalid claim: " ++ show xs

parseNumbers :: String -> [Int]
parseNumbers = map read . split (dropDelims . dropBlanks . condense $ whenElt $ not . isDigit)
