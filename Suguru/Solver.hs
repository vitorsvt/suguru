module Suguru.Solver where

import Suguru.Board (Board, Cell (Cell), getBlock)
import Suguru.Utils (Matrix, Position, getAt, getNeighbors, validPosition)

type Options = [Int]

type BoardOptions = Matrix Options

-- retorna as opções para uma determinada celula
-- O predicado da função filter se trata da restrição entre vizinhos
-- A lista recebe os valores retornados pela função getBlockOptions
getCellOptions :: Board -> Position -> Options
getCellOptions board (x, y) =
  filter
    (`notElem` map (\(Cell _ v) -> v) (getNeighbors board (x, y))) -- predicado
    (getBlockOptions board block) -- lista
  where
    Just (Cell block _) = getAt board (x, y)

getBlockOptions :: Board -> String -> Options
getBlockOptions board block = filter (`notElem` blocked) [1 .. n]
  where
    cells = getBlock board block
    n = length cells
    blocked = filter (/= -1) (map (\(Cell _ v) -> v) cells)


-- pretende-se implantar o backtracking na funcao abaixo
getBoardOptions :: Board -> Position -> Board
getBoardOptions board (6, y) = getBoardOptions board (1, y + 1) -- fim de linha
getBoardOptions board (_, 6) = board                     -- fim de coluna
getBoardOptions board (x, y) 
  | v == (-1) = writeBoard (head (getCellOptions board (x,y))) (x,y)  board  --(getCellOptions board (x, y))
  | otherwise = getBoardOptions board (x + 1, y)
  where
    v = getValueFromPos board (x, y)

-- retorna o valor de uma celula com base na sua posicao
getValueFromPos :: Board -> Position -> Int
getValueFromPos board (x, y) = v
  where
    Just (Cell _ v) = getAt board (x, y)

-- escreve no tabuleiro
writeBoard :: Int -> Position -> Board -> Board
writeBoard v (x,y) board = newboard
  where
    
  -- função lambda com problemas, apenas altera valores das celulas do bloco A
  -- foi adicionada uma derivacao de Eq em Board.hs na declaracao de Cell
    newboard = map (\row -> [ writeCell c v | c <- row]) board
    Just c = getAt board (x,y)

-- escreve na celula
writeCell :: Cell -> Int -> Cell
writeCell c v = Cell b v
  where     
    Cell b _ = c