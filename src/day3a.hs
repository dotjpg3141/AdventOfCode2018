module Day3a ( strat1 ) where
import Day3Common
import qualified Data.Map.Strict as Map
import Data.Maybe

strat1 :: String -> Int
strat1 
    = Map.size 
    . Map.filter (>1) 
    . insertPositions Map.empty 
    . concatMap getPositions 
    . map parseClaim 
    . lines

type Fabric = Map.Map (Int,Int) Int

insertClaim :: Fabric -> Claim -> Fabric
insertClaim fabric claim = insertPositions fabric $ getPositions claim

getPositions :: Claim -> [(Int, Int)]
getPositions claim = [ (x,y) 
        | x <- [ x claim .. x claim + w claim - 1 ]
        , y <- [ y claim .. y claim + h claim - 1 ]
    ]

insertPositions :: Fabric -> [(Int, Int)] -> Fabric
insertPositions fabric = foldl insertPosition fabric

insertPosition :: Fabric -> (Int, Int) -> Fabric
insertPosition = flip $ Map.alter $ Just . (+1) . fromMaybe 0