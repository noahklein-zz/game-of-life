import System.Console.ANSI
import Control.Concurrent (threadDelay)

data Cell = Cell (Int, Int) Bool deriving Show
type World = [Cell]

testWorld :: World
testWorld = [Cell (x, y) (x `mod` 2 == 0) | x <- [0..15], y <- [0..8]]


main :: IO ()
main = do
    hideCursor
    gameLoop testWorld

gameLoop :: World -> IO ()
gameLoop world = do
    clearScreen
    showWorld world
    threadDelay 1000000
    gameLoop $ tick world

showWorld :: World -> IO ()
showWorld [] = return ()
showWorld ((Cell (x, y) isOn):xs) = do
    setCursorPosition y x
    putStr $ if isOn then "1" else "0"
    showWorld xs


tick :: World -> World
tick world = map (tickCell world) world

tickCell :: World -> Cell -> Cell
tickCell world cell@(Cell pos True)
    | n < 2 || n > 3 = Cell pos False
    | otherwise      = cell
    where n = numLiveNeighbors cell world
tickCell world cell@(Cell pos False)
    | n == 3    = Cell pos True
    | otherwise = cell
    where n = numLiveNeighbors cell world

numLiveNeighbors :: Cell -> World -> Int
numLiveNeighbors cell = length . living . neighbors cell

living :: World -> World
living = filter isOn
    where isOn (Cell _ on) = on

neighbors :: Cell -> World -> World
neighbors = filter . isNeighbor

isNeighbor :: Cell -> Cell -> Bool
isNeighbor (Cell pos1@(x1, y1) _) (Cell pos2@(x2, y2) _) =
    withinOne x1 x2 && withinOne y1 y2 && pos1 /= pos2
        where
            (x1, y1) = pos1
            (x2, y2) = pos2
            withinOne a b = (abs $ a - b) <= 1
