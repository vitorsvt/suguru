module Suguru.Board where

import Suguru.Utils (Matrix, Position)

-- Uma célula, com o identificador do bloco e o seu valor
-- "-1" significa valor desconhecido
data Cell = Cell String Int deriving (Eq)

-- Implementa o tipo Show
instance Show Cell where
  show (Cell block (-1)) = show (block ++ "_")
  show (Cell block value) = show (block ++ show value)

-- Gera uma célula a partir de uma string
parseCell :: String -> Cell
parseCell s = cellFromTuple (a, tail b) where (a, b) = span (/= '-') s

-- Gera uma célula a partir de uma tupla
cellFromTuple :: (String, String) -> Cell
cellFromTuple (block, value)
  | value /= "_" = Cell block (read value :: Int)
  | otherwise = Cell block (-1)

-- Implementação do tabuleiro, como uma matriz de células
type Board = Matrix Cell

-- Gera o tabuleiro a partir de uma lista de strings
-- String a string, para cada palavra separada por espaço cria-se uma célula.
boardFromText :: [String] -> Board
boardFromText [] = error "Não foi possível ler o tabuleiro"
boardFromText s = map (\x -> [parseCell c | c <- words x]) s

isBlock :: String -> Cell -> Bool
isBlock query (Cell block _) = block == query

-- Retorna o bloco (grupo de células) com base em um identificador
getBlock :: Board -> String -> [Cell]
getBlock m block = filter (isBlock block) (concat m)

