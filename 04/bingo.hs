import Data.List ( delete, groupBy, transpose )

type Cell = (Int, Bool)

type Board = [[Cell]]

main :: IO ()
main = do
    file <- readFile "./input.txt"
    let
        input = lines file
        draw = parseDraw $ head input
        boardSrc = delete "" (tail input)
        boards = parseBoards boardSrc []

    putStrLn "Part 1:"
    print $ solve $ bingo draw boards
    putStrLn "Part 2:"
    print $ solve $ bingoLast draw boards
    return ()

parseBoards :: [String] -> [Board] -> [Board]
parseBoards [] acc = acc
parseBoards src acc = parsedBoard : parseBoards nextInput acc
    where
        parsedBoard = toBoard $ takeWhile (/= "") src
        nextInput = safeTail $ dropWhile (/= "") src

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

markCell :: Cell -> Int -> Cell
markCell (n, b) m = if n == m then (n, True) else (n, b)

markBoard :: Int -> Board -> Board
markBoard m = map $ map (`markCell` m)

gameRound :: Int -> [Board] -> [Board]
gameRound m = map (m `markBoard`)

bingo :: [Int] -> [Board] -> (Board, Int)
bingo [] boards = error "No winning boards"
bingo (d:ds) boards = if winnerFound then (head winningBoard, d) else bingo ds nextBoards
    where
        nextBoards = gameRound d boards
        winningBoard = filter isWinningBoard nextBoards
        winnerFound = length winningBoard == 1

bingoLast :: [Int] -> [Board] -> (Board, Int)
bingoLast [] boards = error "No winning boards"
bingoLast (d:ds) boards = if lastTurn then (head winningBoard, d) else bingoLast ds newBoards
    where
        nextBoards = gameRound d boards
        newBoards = filter (not . isWinningBoard) nextBoards
        winningBoard = filter isWinningBoard nextBoards
        lastTurn = null newBoards

isWinningBoard :: Board -> Bool
isWinningBoard board = hWin || vWin
    where
        flippedBoard = transpose board
        hasWinRow= all snd
        hWin = any hasWinRow board
        vWin = any hasWinRow flippedBoard


solve :: (Board, Int) -> Int
solve (b, w) = unmarkedSum * w
    where
        unmarkedSum = foldr (\(n, b) acc -> if not b then n + acc else acc) 0 $ concat b

toBoard :: [String] -> Board
toBoard = map (\x -> [toCell n | n <- words x])

toInt x = read x :: Int

toCell :: String -> Cell
toCell str = (toInt str, False)

parseDraw :: [Char] -> [Int]
parseDraw x = map toInt $ filter (/= ",") $ groupBy (\x y -> (x /= ',') && (y /= ',')) x
