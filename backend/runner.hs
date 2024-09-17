import System.Random (randomRIO)
import Control.Monad (replicateM_)

-- Colocar el objeto "P" en el centro de la matriz
placeUserAtCenter :: [[Char]] -> [[Char]]
placeUserAtCenter board =
  let numRows = length board
      numCols = length (head board)
      centerRow = numRows `div` 2
      centerCol = numCols `div` 2
  in insertAt centerRow centerCol 'P' board

-- Insertar un carácter en una posición específica
insertAt :: Int -> Int -> Char -> [[Char]] -> [[Char]]
insertAt row col char board
  | row < 0 || row >= length board = board  -- No hace nada si el índice es inválido
  | col < 0 || col >= length (board !! row) = board  -- No hace nada si el índice es inválido
  | otherwise =
      take row board ++
      [take col (board !! row) ++ [char] ++ drop (col + 1) (board !! row)] ++
      drop (row + 1) board

-- Verificar si una posición está vacía
isEmpty :: Int -> Int -> [[Char]] -> Bool
isEmpty row col board = (board !! row) !! col == '.'

-- Encontrar una posición vacía
findEmptyPosition :: [[Char]] -> IO (Int, Int)
findEmptyPosition board = do
  row <- randomRIO (0, 19)  -- Número de filas, de 0 a 19 (20 filas)
  col <- randomRIO (0, 49)  -- Número de columnas, de 0 a 49 (50 columnas)
  if isEmpty row col board
    then return (row, col)
    else findEmptyPosition board

-- Insertar una fila de obstáculos (vertical u horizontal)
insertObstacleRow :: Int -> Int -> Int -> Int -> Char -> [[Char]] -> IO [[Char]]
insertObstacleRow row col length maxLength char board = do
  orientation <- randomRIO (0 :: Int, 1 :: Int)  -- Anotación de tipo explícita
  let endPos = min (if orientation == 0 then col + length else row + length) maxLength
      coords = if orientation == 0
               then [(row, c) | c <- [col..endPos]]
               else [(r, col) | r <- [row..endPos]]
      updatedBoard = foldl (\b (r, c) -> insertAt r c char b) board coords
  return updatedBoard

-- Insertar múltiples obstáculos
insertObstacles :: Int -> Int -> Int -> [[Char]] -> IO [[Char]]
insertObstacles 0 _ _ board = return board
insertObstacles n minLength maxLength board = do
  (row, col) <- findEmptyPosition board
  length <- randomRIO (minLength, maxLength)  -- Generar una longitud aleatoria
  updatedBoard <- insertObstacleRow row col length 49 'O' board  -- Longitud máxima = 49
  insertObstacles (n - 1) minLength maxLength updatedBoard

-- Insertar múltiples tanques
insertTanks :: Int -> [[Char]] -> IO [[Char]]
insertTanks 0 board = return board
insertTanks n board = do
  (row, col) <- findEmptyPosition board
  let updatedBoard = insertAt row col 'X' board
  insertTanks (n - 1) updatedBoard

-- Run function
level1 :: IO ()
level1 = do
  let initialBoard = replicate 20 (replicate 50 '.')

  let boardWithUser = placeUserAtCenter initialBoard
  putStrLn "Board with user at center:"
  putStrLn (unlines boardWithUser)

  obstacleBoard <- insertObstacles 10 3 7 boardWithUser  -- 10 obstáculos con longitud entre 3 y 7
  putStrLn "Board with obstacles:"
  putStrLn (unlines obstacleBoard)

  finalBoard <- insertTanks 5 obstacleBoard
  putStrLn "Final board with tanks:"
  putStrLn (unlines finalBoard)

  -- Run level - Start game
  -- Listener with sockets 
  -- Threads for each tank
  ------ Deep search algorithm to find the user and move towards it
  ------ If user is near the tank, shoot (Shooter function)
  ------ Life points for each tank (3)
  -- Threads for user movements
  ------ User can move up, down, left, right
  ------ Shoot (Shooter function)
  ------ Life points for user (6)

  --- Threads for bullet 
  ----- Move bullet in the direction of the shooter
  ----- If bullet hits the tank, tank life points -1
  ----- If bullet hits the user, user life points -1
  ----- If bullet hits the obstacle, bullet disappears
  ----- If bullet hits the wall, bullet disappears

  -- Game over when user is killed or all tanks are killed or user reaches the end of the board or tanks crash with obstacles or each other

start :: IO ()
start = do
  -- Start server and listen in port 3000
  putStrLn "Server started at port 3000"
  level1

main :: IO ()
main = start