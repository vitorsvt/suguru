module Suguru.Solver where

import Suguru.Board (Board, Cell (Cell), getBlock)
import Suguru.Utils (Matrix, Position, getAt, getNeighbors, validPosition, setAt)

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
getBoardOptions :: Board -> Position ->  Maybe Board
getBoardOptions board (7, y) = getBoardOptions board (1, y + 1) -- fim de linha
getBoardOptions board (_, 7) = return (Just board)                     -- fim de coluna
getBoardOptions board (x, y) = do 
                            v <- Just (getValueFromPos board (x,y))
                            case v of {
                            (-1) ->  getCellOptions board (x,y) >>= (getBoardOptions' board (x,y))
                            ;(_) -> (getBoardOptions board (x + 1, y))}

  where getBoardOptions' board (x,y) [] = return Nothing
        getBoardOptions' board (x,y) (v:vs) = do 
                                              writeBoard v (x,y) board
                                              r <- getBoardOptions board (x+1, y)
                                              writeBoard (-1) (x,y) board
                                              getBoardOptions' board (x,y) vs  



-- escreve no tabuleiro
writeBoard :: Int -> Position -> Board -> Board
writeBoard v (x,y) board = setAt board (x,y) cell
  where
    cell = Cell (getBlockFromPos board (x,y)) v



-- retorna o valor de uma celula com base na sua posicao
getValueFromPos :: Board -> Position -> Int
getValueFromPos board (x, y) = v
  where
    Just (Cell _ v) = getAt board (x, y)

-- retorna string com o bloco de uma celula
getBlockFromPos :: Board -> Position -> String
getBlockFromPos board (x, y) = b
  where
    Just (Cell b _) = getAt board (x, y)
