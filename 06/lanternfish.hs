import Data.List ( groupBy )
import Data.Function ( on )
import Data.Char ( isDigit )

type Day = Int

main :: IO ()
main = do 
    input <- readFile "./input.txt"

    let 
        initialPopulation = map (\x -> read x :: Int) . filter (/= ",") . groupBy ((==) `on` isDigit) $ init input
        solve1 = length $ simulateN simulation initialPopulation 80 
    print solve1


-- "Could this be stork" 
newborns :: [Day] -> [Day]
newborns = foldr (\x acc -> if x == 0 then 8 : acc  else acc) []

step :: Day -> Day 
step n = if n == 0 then 6 else pred n

nextGen :: [Day] -> [Day]
nextGen = map step

-- Sim algorithm:
-- 1 - Scan current state for 0, for each 0
-- 2 - Generate a list of newborn fishes
-- 3 - Decrement days of the CURRENT generation
-- 4 - Append newborn list to the end of the next day cycle 
simulation :: [Day] -> [Day]
simulation xs = nextDayCycle ++ newFish
    where 
        newFish = newborns xs
        nextDayCycle = nextGen xs


simulateN :: ([Day] -> [Day]) -> [Day] -> Int -> [Day]
simulateN sim pop n = iterate sim pop !! n
