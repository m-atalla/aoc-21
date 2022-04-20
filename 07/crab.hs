import Data.List ( group, groupBy, sort, sortBy )
import Data.Function ( on )
import Data.Char ( isDigit )

type Bin = (Int, Int)

main :: IO ()
main = do
    file <- readFile "./input.txt"
    let
        testInput = [16,1,2,0,4,2,7,1,2,14]
        input = map toInt . 
                filter (/= ",") . 
                init . 
                groupBy ((==) `on` isDigit) $ file

    putStrLn $ "Part 1: " ++ show (lowestCost  input)
    putStrLn $ "Part 2: " ++ show (lowestCostSeries input)

toInt x = read x :: Int

positionHistogram :: [Int] -> [Bin]
positionHistogram = sortBy (\(a, _) (b, _) -> compare b a) .
                    map (\l@(n:ns) -> (length l, n)) .
                    group .
                    sort

alignCost :: Int -> Int -> Int
alignCost to from = abs (to - from)

lowestCost :: [Int] -> Int
lowestCost iniPos = minimum allAlignments
    where
        allAlignments = map (sum . (\pos -> map (alignCost pos) iniPos)) likelyPos
        likelyPos = map snd . positionHistogram $ iniPos

-- part 2
alignCostSeries :: Int -> Int -> Int
alignCostSeries to from = let
                            n = alignCost to from 
                          in
                            n * (n + 1) `div` 2

lowestCostSeries :: [Int] -> Int
lowestCostSeries iniPos = minimum allAlignments
    where
        allAlignments = map (sum . (\pos -> map (alignCostSeries pos) iniPos)) likelyPos
        likelyPos = [0 .. maximum iniPos]
