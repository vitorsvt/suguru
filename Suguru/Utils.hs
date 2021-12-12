module Suguru.Utils where

import Data.Maybe (mapMaybe)

-- ========================================================================== --
--                                    Tipos                                   --
-- ========================================================================== --

-- | Matriz do tabuleiro, representada por uma lista de listas
type Matrix t = [[t]]

-- | Representa uma coordenada 2D na matriz
type Position = (Int, Int)

-- ========================================================================== --
--                                   Funções                                  --
-- ========================================================================== --


-- | Imprime uma matriz
printMatrix :: Show t => Matrix t -> IO ()
printMatrix m = putStrLn (matrixAsString m)

-- | Converte uma matriz para string
matrixAsString :: Show t => Matrix t -> String
matrixAsString (a : xs) = show a ++ "\n" ++ matrixAsString xs
matrixAsString [] = ""

-- | Valida se a posicao esta dentro da matriz
validPosition :: Matrix t -> Position -> Bool
validPosition matrix (x, y) =
  let n = length matrix in
  x >= 0 && y >= 0 && x < n && y < n

-- | O elemento da posicao (x,y). se posicao não é válida, nothing.
getAt :: Matrix t -> Position -> Maybe t
getAt matrix (i, j)
  | validPosition matrix (i-1, j-1) = Just (matrix !! (i-1) !! (j-1))
  | otherwise = Nothing

-- | Inclui um elemento e retorna a nova matriz. Nothing se posicao invalida.
setAt :: Matrix t -> Position -> t -> Maybe (Matrix t)
setAt matrix (i, j) value
  | validPosition matrix (i-1, j-1) = Just (start++(x++value:ys):end)
  | otherwise = Nothing
  where
    (start, row:end) = splitAt (i-1) matrix
    (x, _:ys) = splitAt (j-1) row

-- | Posicao de todos os vizinhos imediatos a uma celula
getNeighborPositions :: Position -> [Position]
getNeighborPositions (i, j) =
  [(i+a, j+b)
  | a <- [-1..1]
  , b <- [-1..1]
  , (a, b) /= (0, 0)]

-- | Vizinhos de uma posicao (x,y)
getNeighbors :: Matrix t -> Position -> [t]
getNeighbors matrix pos = mapMaybe (getAt matrix) (getNeighborPositions pos)
