import Data.List ( group, groupBy, sort, sortBy )
import Data.Function ( on )
import Data.Char ( isDigit )

type Bin = (Int, Int)

main :: IO ()
main = do
    file <- readFile "./input.txt"
    let
        input = map toInt . filter (/= ","). init . groupBy ( (==) `on` isDigit ) $ file

    print $ lowestCost input

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
