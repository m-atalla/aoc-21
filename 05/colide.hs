import Data.List ( (\\), groupBy, sort )
import Data.Char ( isDigit )
import Data.Function ( on )

type Point = (Int, Int)

data Line = Line {
    src :: Point,
    dest :: Point
} deriving (Show)


main :: IO ()
main = do
    c <- readFile "./input.txt"
    let
        input = lines c
        parsed = parseLine input
        parsed2 = map flattenPointStr parsed
        parsed3 = map convertToLine parsed2
        vents = concatMap (\x -> genPoints (src x) (dest x)) $ filter isHorV parsed3
        sol1 = part1 vents

    print sol1
    return ()

toInt x = read x :: Int

parseLine :: [String] -> [[String]]
parseLine = map removeArrows

-- Applies the following string transformation:
-- "x1,y1 -> x2,y2" -> ["x1,y1","x2,y2"]
removeArrows :: [Char] -> [String]
removeArrows x = words $ x \\ "->"

-- Applies the following transformation:
-- ["x1,y1","x2,y2"] -> [x1,y1,x2,y2]
flattenPointStr :: [String] -> [String]
flattenPointStr = concatMap (deleteByIdx 1 . groupBy ((==) `on` isDigit))

convertToLine :: [String] -> Line
convertToLine l = Line { src = (x1 l, y1 l), dest = (x2 l,y2 l)}
    where x1 = (#!! 0)
          y1 = (#!! 1)
          x2 = (#!! 2)
          y2 = (#!! 3)
          (#!!) xs idx = toInt (xs !! idx) -- Get by index and convert to Int

deleteByIdx :: Int -> [a] -> [a]
deleteByIdx idx xs = before ++ after
    where (before, _:after) = splitAt idx xs


-- Generates intermediate points from source to destination
genPoints (x1 , y1) (x2, y2)
    | x1 == x2 && y1 < y2 = zip (repeat x1) [y1..y2]
    | x1 == x2 && y1 > y2 = zip (repeat x1) [y2..y1]
    | x1 > x2 && y1 == y2 = zip [x2..x1] (repeat y1)
    | x1 < x2 && y1 == y2 = zip [x1..x2] (repeat y1)
    | otherwise = [(x1,y1)] -- both xs and ys are equal, meaning it is only a single point

part1 vs = length $ filter ((>= 2) . length) . groupBy (\(x1, y1) (x2, y2) -> x1 == x2 && y1 == y2 ) $ sort vs

isHorV line = x1 == x2 || y1 == y2
    where
        (x1, y1) = src line
        (x2, y2) = dest line
