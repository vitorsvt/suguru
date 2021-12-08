module Suguru.Utils where

import Data.Maybe (catMaybes)

type Position = (Int, Int)

-- Matrizes são uma lista de listas
type Matrix t = [[t]]

-- Não é possível implementar Show diretamente por usarmos "type"
printMatrix :: Show t => Matrix t -> IO ()
printMatrix m = putStrLn (matrixAsString m)

matrixAsString :: Show t => Matrix t -> String
matrixAsString (a : xs) = show a ++ "\n" ++ matrixAsString xs
matrixAsString [] = ""

validPosition :: Matrix t -> Position -> Bool
validPosition matrix (x, y) = x >= 0 && y >= 0 && x < n && y < n
  where
    n = length matrix

getAt :: Matrix t -> Position -> Maybe t
getAt m (x, y)
  | validPosition m (x -1, y -1) = Just (m !! (x -1) !! (y -1))
  | otherwise = Nothing

getNeighborPositions :: Position -> [Position]
getNeighborPositions (x, y) = [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]

getNeighbors :: Matrix t -> Position -> [t]
getNeighbors m pos = catMaybes ([getAt m p | p <- getNeighborPositions pos])
