import Data.List ( (\\), groupBy )
import Data.Char ( isDigit )
import Data.Function ( on )

data Point = Point (Int, Int) deriving (Show)

data Line = Line {
    src :: Point,
    dest :: Point
} deriving (Show)


main :: IO ()
main = do
    c <- readFile "./input.txt"
    let
        input = lines c
        diagram = [[0 | _ <- [0..999]] | _ <- [0..999]]
        parsed = parseLine input
        parsed2 = map flattenPointStr parsed
        parsed3 = map convertToLine parsed2

    print parsed3
    return ()

toInt x = read x :: Int

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
convertToLine l = Line { src = Point (x1 l, y1 l), dest = Point (x2 l,y2 l)}
    where x1 = (#!! 0)
          y1 = (#!! 1)
          x2 = (#!! 2)
          y2 = (#!! 3)
          (#!!) xs idx = toInt (xs !! idx)

deleteByIdx :: Int -> [a] -> [a]
deleteByIdx idx xs = before ++ after
    where (before, _:after) = splitAt idx xs
