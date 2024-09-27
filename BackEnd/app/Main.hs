{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.List (sortBy, nub)
import qualified Data.ByteString.Char8 as BS
import Data.List.Split (splitOn)

-- Representamos una celda con un tipo para distinguir entre celdas vacías (Empty) y muros (Wall)
data Cell = Empty | Wall deriving (Eq, Show)

-- Definimos una coordenada con su posición (x, y) y el tipo de celda
data Coordinate = Coordinate (Int, Int) Cell deriving Show

-- Una matriz de coordenadas con celdas vacías y muros
type Matrix = [[Coordinate]]

-- Ejemplo de matriz con celdas vacías y muros
matrizEjemplo :: Matrix
matrizEjemplo = [
        [Coordinate (-200, -200) Wall, Coordinate (-150, -200) Wall, Coordinate (-100, -200) Wall, Coordinate (-50, -200) Wall, Coordinate (0, -200) Wall, Coordinate (50, -200) Wall, Coordinate (100, -200) Wall, Coordinate (150, -200) Wall, Coordinate (200, -200) Wall, Coordinate (250, -200) Wall, Coordinate (300, -200) Wall, Coordinate (350, -200) Wall],
        [Coordinate (-200, -150) Wall, Coordinate (-150, -150) Empty, Coordinate (-100, -150) Empty, Coordinate (-50, -150) Empty, Coordinate (0, -150) Empty, Coordinate (50, -150) Empty, Coordinate (100, -150) Empty, Coordinate (150, -150) Empty, Coordinate (200, -150) Empty, Coordinate (250, -150) Empty, Coordinate (300, -150) Empty, Coordinate (350, -150) Wall],
        [Coordinate (-200, -100) Wall, Coordinate (-150, -100) Empty, Coordinate (-100, -100) Wall, Coordinate (-50, -100) Wall, Coordinate (0, -100) Wall, Coordinate (50, -100) Wall, Coordinate (100, -100) Wall, Coordinate (150, -100) Wall, Coordinate (200, -100) Wall, Coordinate (250, -100) Wall, Coordinate (300, -100) Empty, Coordinate (350, -100) Wall],
        [Coordinate (-200, -50) Wall, Coordinate (-150, -50) Empty, Coordinate (-100, -50) Wall, Coordinate (-50, -50) Empty, Coordinate (0, -50) Empty, Coordinate (50, -50) Empty, Coordinate (100, -50) Empty, Coordinate (150, -50) Empty, Coordinate (200, -50) Empty, Coordinate (250, -50) Wall, Coordinate (300, -50) Empty, Coordinate (350, -50) Wall],
        [Coordinate (-200, 0) Wall, Coordinate (-150, 0) Empty, Coordinate (-100, 0) Wall, Coordinate (-50, 0) Empty, Coordinate (0, 0) Wall, Coordinate (50, 0) Wall, Coordinate (100, 0) Wall, Coordinate (150, 0) Empty, Coordinate (200, 0) Empty, Coordinate (250, 0) Wall, Coordinate (300, 0) Empty, Coordinate (350, 0) Wall],
        [Coordinate (-200, 50) Wall, Coordinate (-150, 50) Empty, Coordinate (-100, 50) Empty, Coordinate (-50, 50) Empty, Coordinate (0, 50) Wall, Coordinate (50, 50) Empty, Coordinate (100, 50) Empty, Coordinate (150, 50) Empty, Coordinate (200, 50) Wall, Coordinate (250, 50) Empty, Coordinate (300, 50) Empty, Coordinate (350, 50) Wall],
        [Coordinate (-200, 100) Wall, Coordinate (-150, 100) Empty, Coordinate (-100, 100) Wall, Coordinate (-50, 100) Wall, Coordinate (0, 100) Wall, Coordinate (50, 100) Wall, Coordinate (100, 100) Empty, Coordinate (150, 100) Wall, Coordinate (200, 100) Wall, Coordinate (250, 100) Wall, Coordinate (300, 100) Empty, Coordinate (350, 100) Wall],
        [Coordinate (-200, 150) Wall, Coordinate (-150, 150) Empty, Coordinate (-100, 150) Wall, Coordinate (-50, 150) Empty, Coordinate (0, 150) Empty, Coordinate (50, 150) Empty, Coordinate (100, 150) Empty, Coordinate (150, 150) Empty, Coordinate (200, 150) Empty, Coordinate (250, 150) Wall, Coordinate (300, 150) Empty, Coordinate (350, 150) Wall],
        [Coordinate (-200, 200) Wall, Coordinate (-150, 200) Empty, Coordinate (-100, 200) Wall, Coordinate (-50, 200) Wall, Coordinate (0, 200) Wall, Coordinate (50, 200) Wall, Coordinate (100, 200) Wall, Coordinate (150, 200) Wall, Coordinate (200, 200) Wall, Coordinate (250, 200) Wall, Coordinate (300, 200) Empty, Coordinate (350, 200) Wall],
        [Coordinate (-200, 250) Wall, Coordinate (-150, 250) Empty, Coordinate (-100, 250) Empty, Coordinate (-50, 250) Empty, Coordinate (0, 250) Empty, Coordinate (50, 250) Empty, Coordinate (100, 250) Empty, Coordinate (150, 250) Empty, Coordinate (200, 250) Empty, Coordinate (250, 250) Empty, Coordinate (300, 250) Empty, Coordinate (350, 250) Wall],
        [Coordinate (-200, 300) Wall, Coordinate (-150, 300) Empty, Coordinate (-100, 300) Empty, Coordinate (-50, 300) Empty, Coordinate (0, 300) Empty, Coordinate (50, 300) Empty, Coordinate (100, 300) Empty, Coordinate (150, 300) Empty, Coordinate (200, 300) Empty, Coordinate (250, 300) Empty, Coordinate (300, 300) Empty, Coordinate (350, 300) Wall],
        [Coordinate (-200, 350) Wall, Coordinate (-150, 350) Wall, Coordinate (-100, 350) Wall, Coordinate (-50, 350) Wall, Coordinate (0, 350) Wall, Coordinate (50, 350) Wall, Coordinate (100, 350) Wall, Coordinate (150, 350) Wall, Coordinate (200, 350) Wall, Coordinate (250, 350) Wall, Coordinate (300, 350) Wall, Coordinate (350, 350) Wall]
    ]

heuristica :: (Int, Int) -> (Int, Int) -> Int
heuristica (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Función principal que realiza la búsqueda en profundidad con heurística
profundidad :: Matrix -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
profundidad matriz inicio fin = reverse (profAux matriz [(inicio, heuristica inicio fin)] fin [])

-- Función auxiliar que realiza la búsqueda en profundidad
profAux :: Matrix -> [((Int, Int), Int)] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
profAux _ [] _ _ = []  -- No hay ruta, lista vacía
profAux matriz ((actual, _):rutas) fin visitados = 
    if actual == fin
    then actual : visitados  -- Si llegamos al destino, devolvemos la ruta
    else
        let nuevosVecinos = extender matriz actual visitados
            nuevosRutas = [(vecino, heuristica vecino fin) | vecino <- nuevosVecinos]
            rutasOrdenadas = sortBy (comparing snd) (nuevosRutas ++ rutas)
        in profAux matriz rutasOrdenadas fin (actual : visitados)

-- Extiende la ruta a los vecinos accesibles (no muros y no visitados)
extender :: Matrix -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
extender matriz (x, y) visitados =
    filter (\(nx, ny) -> esValido matriz (nx, ny) visitados) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Verifica si una posición es válida: está dentro de la matriz, no es un muro y no ha sido visitada
esValido :: Matrix -> (Int, Int) -> [(Int, Int)] -> Bool
esValido matriz (x, y) visitados =
    x >= 0 && y >= 0 && x < length matriz && y < length (head matriz) &&
    not (elem (x, y) visitados) && (tipoCelda matriz (x, y) == Empty)

-- Obtiene el tipo de una celda en la matriz
tipoCelda :: Matrix -> (Int, Int) -> Cell
tipoCelda matriz (x, y) = let Coordinate _ cell = (matriz !! x) !! y in cell

-- Función principal del servidor
main :: IO ()
main = do
    addr <- resolve 3000 
    sock <- open addr
    putStrLn "Servidor escuchando en el puerto 3000..."
    loop sock

resolve :: PortNumber -> IO AddrInfo
resolve port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
    return addr

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 1024
    return sock

loop :: Socket -> IO ()
loop sock = do
    (conn, _) <- accept sock
    handleConnection conn
    loop sock

-- Manejar la conexión, recibir posiciones, buscar ruta y devolverla
handleConnection :: Socket -> IO ()
handleConnection conn = do
    msg <- recv conn 1024
    let coordenadas = BS.unpack msg
    let [x1, y1, x2, y2] = map read (splitOn " " coordenadas) :: [Int]
    let inicio = (x1, y1)
    let fin = (x2, y2)
    let ruta = profundidad matrizEjemplo inicio fin
    let respuesta = if null ruta
                    then "No se encontró ruta"
                    else show ruta
    sendAll conn (BS.pack respuesta)
    close conn