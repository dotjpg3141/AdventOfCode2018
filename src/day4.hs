import Data.List
import Data.List.Split
import Control.Applicative
import Data.Maybe
import Data.Function
import qualified Data.Map.Strict as Map
import AOC

main :: IO ()
main = solving 4 strat1 strat2 parseInput

parseInput :: String -> [SleepInstance]
parseInput = sleepInstances . parseRecords

strat1 :: [SleepInstance] -> Int
strat1 instances = sleepiestGuard * sleepiestMinute
    where
        sleepiestGuard  = sleepiest sleepGuard instances
        sleepiestMinute = sleepiest sleepMinute
                        . filter ( (==sleepiestGuard) . sleepGuard )
                        $ instances

strat2 :: [SleepInstance] -> Int
strat2 instances = sleepGuard inst * sleepMinute inst
    where
        inst = sleepiest id instances
                        
data Record = Record {
        year   :: Int,
        month  :: Int,
        day    :: Int,
        hour   :: Int,
        minute :: Int,
        guard  :: Maybe Int,
        action :: GuardAction
    } deriving (Show, Ord, Eq)

data GuardAction = BeginShift | FallAsleep | WakeUp deriving (Show, Ord, Eq)

data SleepInstance = SleepInstance {
        sleepMinute :: Int,
        sleepGuard  :: Int
    } deriving (Show, Ord, Eq)

defaultRecord :: Record
defaultRecord = Record 0 0 0 0 0 Nothing BeginShift

splitRecord :: String -> [String]
splitRecord = split $ dropDelims $ dropBlanks $ oneOf "[]-: #"

parseRecord :: [String] -> Record
parseRecord (y:m:d:h:mn:w1:w2:_) = Record {
        year   = read y,
        month  = read m,
        day    = read d,
        hour   = read h,
        minute = read mn,
        guard  = if w1 == "Guard"
            then Just . read $ w2
            else Nothing,
        action = case w1 of
            "Guard"     -> BeginShift
            "falls"     -> FallAsleep
            "wakes"     -> WakeUp
    }

parseRecords :: String -> [Record]
parseRecords 
    = tail
    . ( scanl ( curry (\(prev, curr) -> curr { guard = guard curr <|> guard prev }) ) defaultRecord )
    . sort
    . map ( parseRecord . splitRecord )
    . lines

sleepiest :: (Ord a) => (SleepInstance -> a) -> [SleepInstance] -> a
sleepiest f
    = fst
    . maximumBy ( compare `on` snd )
    . Map.assocs
    . Map.fromListWith (+)
    . map (\x -> (f x, 1))

sleepInstances :: [Record] -> [SleepInstance]
sleepInstances [] = []
sleepInstances (Record{action=BeginShift}:xs) = sleepInstances xs
sleepInstances (Record{action=FallAsleep,minute=start,guard=Just g}:Record{action=WakeUp,minute=end}:xs)
    = map (\m -> SleepInstance { sleepMinute = m, sleepGuard = g } ) [ start .. end -1 ]
    ++ sleepInstances xs