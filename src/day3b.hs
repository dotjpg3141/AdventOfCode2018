module Day3b ( strat2 ) where
import Day3Common
import qualified Data.Map.Strict as Map
import Data.Maybe

strat2 :: String -> Int
strat2
    = indet
    . fst
    . head
    . filter (\(claim, count) -> size claim == count)
    . Map.assocs
    . Map.fromListWith (+)
    . filter ((==1) . snd)
    . Map.elems
    . insertPositions Map.empty 
    . concatMap getPositions 
    . map parseClaim 
    . lines

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
