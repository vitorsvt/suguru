module Suguru.Board where

    import Suguru.Utils (Matrix (Matrix))

    -- Uma célula, com o identificador do bloco e o seu valor
    -- "-1" significa valor desconhecido
    data Cell = Cell String Int

    -- Implementa o tipo Show
    instance Show Cell where
        show (Cell block (-1)) = show (block ++ "_")
        show (Cell block value) = show (block ++ show value)

    -- Gera uma célula a partir de uma string
    parseCell :: String -> Cell
    parseCell s = cellFromTuple (a, tail b) where (a, b) = span (/= '-') s

    -- Gera uma célula a partir de uma tupla
    cellFromTuple :: (String, String) -> Cell
    cellFromTuple (block, value) | value /= "_" = Cell block (read value)
                                 | otherwise = Cell block (-1)

    -- Implementação do tabuleiro, como uma matriz de células
    newtype Board = Board (Matrix Cell) deriving Show

    -- Gera o tabuleiro a partir de uma lista de strings
    boardFromText :: [String] -> Board
    boardFromText [] = error "Não foi possível ler o tabuleiro"
    boardFromText s = Board (Matrix (map (\ x -> [parseCell s | s <- words x]) s))
