import Data.List ( transpose )
import Data.Char ( digitToInt )

main = do
    binaryInput <- readFile "./input.txt"

    let bitRead = map countBits . groupByColumns $ lines binaryInput

    putStrLn $ "Part 1: " ++ show (part1 bitRead)

    putStrLn $ "Part 2: " ++ show (part2 $ lines binaryInput)

part1 :: [(Int, Int)] -> Int
part1 bitRead = gamma * epsilon
    where
        gamma   = bin2dec $ map assignBitGamma bitRead
        epsilon = bin2dec $ map assignBitEpsilon bitRead


countBits :: [Char] -> (Int, Int)
countBits = foldr zipSumBits (0, 0)
    where
        zipSumBits bit (zeros, ones) = if bit == '0' then(succ zeros, ones) else (zeros, succ ones)

groupByColumns :: [[Char]] -> [[Char]]
groupByColumns = transpose

assignBit :: (Int -> Int -> Bool) -> (Int, Int) -> Char
assignBit pred (x, y) = if pred x y then '0' else '1'

assignBitGamma :: (Int, Int) -> Char
assignBitGamma = assignBit (>)

assignBitEpsilon :: (Int, Int) -> Char
assignBitEpsilon = assignBit (<)

bin2dec :: [Char] -> Int
bin2dec bs = sum [digitToInt b * 2 ^ i | (b, i) <- zip (reverse bs) [0..]]



-- part 2
assignBitO :: (Int, Int) -> Char
assignBitO (x, y) = if x == y then '1' else assignBit (>) (x, y)

assignBitCO2 :: (Int, Int) -> Char
assignBitCO2 (x, y) = if x == y then '0' else assignBit (<) (x, y)


filterDiagnostic :: [[Char]] -> Int -> ((Int, Int) -> Char) -> Int
filterDiagnostic [x] _  _ = bin2dec x
filterDiagnostic xs idx pred = filterDiagnostic newDiagnostic (idx + 1) pred
    where 
        countPairs = map countBits . groupByColumns
        newMapping = countPairs xs !! idx
        filterBit = pred newMapping
        newDiagnostic = foldByNthBit idx filterBit xs

part2 :: [[Char]] -> Int
part2 binInput = oLevel * co2Level
    where 
        oLevel   = filterDiagnostic binInput 0 assignBitO
        co2Level = filterDiagnostic binInput 0 assignBitCO2


foldByNthBit :: (Foldable t, Eq a) => Int -> a -> t [a] -> [[a]]
foldByNthBit nth bit = foldr (\xs acc ->
        if xs !! nth == bit then
            xs : acc
        else
            acc
    )
    []

testInput = [
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"]