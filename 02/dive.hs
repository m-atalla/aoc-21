main = do 
        contents <- readFile "input.txt"
        print . finalDepth' $ diveSum' $ zipper . words $ contents


readInt :: String -> Int
readInt = read


zipper :: [String] -> [(String, Int)]
zipper []       = []
zipper (x:y:ys) = (x, readInt y) : zipper ys


-- Part 1
dive :: [(String ,Int)] -> [(Int, Int)]
dive [] = [(0, 0)]
dive ((a, m):xs) 
           | a == "up"       = (-m, 0) : dive xs
           | a == "down"     = (m, 0) : dive xs
           | otherwise       = (0, m) : dive xs

diveSum :: [(Int, Int)] -> (Int,Int)
diveSum = foldr (\(v,h) (av, ah) -> (av + v, ah + h)) (0, 0)

finalDepth :: (Int, Int) -> Int
finalDepth (v, h) = v * h

-- Part 2 
diveSum' :: [(String, Int)] -> (Int, Int, Int)                                        
diveSum' = foldl accu (0, 0, 0)
        where
            accu (v, h, a) (command, x) = case command of
                            "forward" -> (v + (a * x), h + x, a)
                            "up" -> (v , h, a - x)
                            "down" -> (v, h, a + x)

finalDepth' :: (Int, Int, Int) -> Int
finalDepth' (v, h, _) = v * h
