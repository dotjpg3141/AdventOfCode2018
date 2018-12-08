import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe

main = interact 
    $ show
    . filter (\(claim, count) -> size claim == count)
    . Map.assocs
    . Map.fromListWith (+)
    . filter ((==1) . snd)
    . Map.elems
    . insertPositions Map.empty 
    . concatMap getPositions 
    . map parseClaim 
    . lines

data Claim = Claim {
    indet :: Int,
    x :: Int,
    y :: Int,
    w :: Int,
    h :: Int
} deriving (Show, Ord, Eq)

size :: Claim -> Int
size c = w c * h c

type Position = ((Int, Int), Claim)
type Fabric = Map.Map (Int,Int) (Claim, Int)

insertClaim :: Fabric -> Claim -> Fabric
insertClaim fabric claim = insertPositions fabric $ getPositions claim

getPositions :: Claim -> [Position]
getPositions claim = [ ((x,y), claim) 
        | x <- [ x claim .. x claim + w claim - 1 ]
        , y <- [ y claim .. y claim + h claim - 1 ]
    ]

insertPositions :: Fabric -> [Position] -> Fabric
insertPositions fabric = foldl insertPosition fabric

insertPosition :: Fabric -> Position -> Fabric
insertPosition fabric (coord, claim) = (Map.alter $ Just . mapSnd (+1) . fromMaybe (claim, 0)) coord fabric

parseClaim :: String -> Claim
parseClaim xs = case parseNumbers xs of
    [ ident, x, y, w, h ]  -> Claim ident x y w h
    _                      -> error $ "Invalid claim: " ++ show xs

parseNumbers :: String -> [Int]
parseNumbers = map read . split (dropDelims . dropBlanks . condense $ whenElt $ not . isDigit)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)
