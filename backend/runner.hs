import System.Random (randomRIO)
import Control.Monad (replicateM_)

-- Game Rules 
-- 1. The tank is represented by the character 'X'
-- 2. The player is represented by the character 'P'
-- 3. The obstacles are represented by the character 'O'
-- 4. The empty spaces are represented by the character '.'
-- 
--
-- + The tank is placed in a random position on the board
-- + The tank is placed in a position where it does not overlap with any other character

-- Insertar un carácter en una posición específica
insertAt :: Int -> Int -> Char -> [[Char]] -> [[Char]]
insertAt row col char board =
  take row board ++
  [take col (board !! row) ++ [char] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board

-- Verificar si una posición está vacía
isEmpty :: Int -> Int -> [[Char]] -> Bool
isEmpty row col board = (board !! row) !! col == '.'

-- Encontrar una posición vacía
findEmptyPosition :: [[Char]] -> IO (Int, Int)
findEmptyPosition board = do
  row <- randomRIO (0, 19)  -- Number of rows, 0 a 19 (20 rows)
  col <- randomRIO (0, 49)  -- Number of columns, 0 a 49 (50 columns)
  if isEmpty row col board
    then return (row, col)
    else findEmptyPosition board

-- Insertar múltiples tanques
insertTanks :: Int -> [[Char]] -> IO [[Char]]
insertTanks 0 board = return board
insertTanks n board = do
  (row, col) <- findEmptyPosition board
  let updatedBoard = insertAt row col 'X' board
  insertTanks (n - 1) updatedBoard


-- Insertar obstaculo
insertObstacle :: Int -> [[Char]] -> [[Char]]
insertObstacle 0 board = board
insertObstacle n board = do
  (row, col) <- findEmptyPosition board  -- <---- Error
  let updatedBoard = insertAt row col 'O' board
  insertObstacle (n - 1) updatedBoard


-- Run function
level1 :: IO ()
level1 = do
  let initialBoard = replicate 20 (replicate 50 '.')
  putStrLn "Initial board:"
  putStrLn (unlines initialBoard)

  obstacleBoard <- 10 initialBoard
  putStrLn "Board with obstacles:"
  putStrLn (unlines obstacleBoard)

  finalBoard <- insertTanks 5 initialBoard
  putStrLn "Final board with tanks:"
  putStrLn (unlines finalBoard)

start :: IO ()
start = do
  -- Start server and listen in port 3000
  putStrLn "Server started at port 3000"
  -- Listen for incoming connections
  -- Accept incoming connections
  -- Start board initialization and game loop
  level1

main :: IO ()
main = start