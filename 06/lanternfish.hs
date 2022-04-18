import Data.List ( groupBy )
import Data.Function ( on )
import Data.Char ( isDigit )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )

type Day = Int
type Pop = Map.Map Int Int

main :: IO ()
main = do
    input <- readFile "./input.txt"

    let
        parsedInput = map (\x -> read x :: Int) . filter (/= ",") . groupBy ((==) `on` isDigit) $ init input
        pop = popInit parsedInput

        -- -- Small remark:
        -- -- Part 1 using brute force approach, actually took way longer to simulate than part 2
        -- -- despite being a significantly shorter. (N=80)
        -- solve1 = length $ simulateN simulation parsedInput 80
        (solve2, solutionMap) = Map.mapAccum (\acc v -> (acc + v, v)) 0 $ simulateN lanternFishSim pop 256

    print solve2

-- Part 1
newborns :: [Day] -> [Day]
newborns = foldr (\x acc -> if x == 0 then 8 : acc  else acc) []

step :: Int -> Int
step n = if n == 0 then 6 else pred n

nextGen :: [Day] -> [Day]
nextGen = map step

-- Sim algorithm: "brute force" approach
-- 1 - Scan current state for 0
-- 2 - For each 0 generate a newborn fish
-- 3 - Decrement days of the CURRENT generation
-- 4 - Append newborn list to the end of the next day cycle 
simulation :: [Day] -> [Day]
simulation xs = nextDayCycle ++ newFish
    where
        newFish = newborns xs
        nextDayCycle = nextGen xs


simulateN :: (a -> a) -> a -> Int -> a
simulateN sim pop n = iterate sim pop !! n

-- Part 2
popInit :: [Int] -> Pop
popInit = foldr (Map.update (Just . succ)) init
    where init = Map.fromList [(i, 0) | i <- [0..8]]

newborns' :: Pop -> Int
newborns' = simLookup 0

simLookup :: Int -> Pop -> Int
simLookup k m = fromMaybe 0 (Map.lookup k m)

-- prevents overrwriting a 'transitioned' value
safeInsert :: Int -> Int -> Pop -> Pop
safeInsert = Map.insertWith (+)

nextGen' :: Pop -> Pop
nextGen' currentPop = nextGenPatch
    where
        (next, _) = Map.mapAccumWithKey (\acc k v -> (safeInsert (step k) v acc, v)) (Map.fromList [(0, 0)]) currentPop
        -- Using this method will always drop the last key, thus 
        -- it needs to be patched back in the next generation map
        nextGenPatch = safeInsert 8 0 next

lanternFishSim :: Pop -> Pop 
lanternFishSim pop = nextDay
    where
        newFishCount = newborns' pop
        transitionedCurrent = nextGen' pop
        nextDay = safeInsert 8 newFishCount transitionedCurrent