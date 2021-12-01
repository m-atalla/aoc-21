import System.IO

main :: IO ()
main = do
        contents <- readFile "input.txt"
        print . depthSpike . slidingWindow . map readInt . words $ contents


readInt :: String -> Int
readInt = read
    

-- Part 1
depthSpike :: [Int] -> Int
depthSpike xs = sum [1 | (x,i) <- zip xs [0..], i /= 0 && xs !! i > xs !! (i-1) ]


-- Part 2 
slidingWindow :: [Int] -> [Int]
slidingWindow [] = []
slidingWindow (x:xs) | length xs < 2 = []
                     | otherwise     = sum (x : take 2 xs) : slidingWindow xs
