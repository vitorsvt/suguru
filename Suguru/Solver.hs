module Suguru.Solver where

import Suguru.Board (Board,
  Cell (Cell),
  getBlock,
  getCellValue,
  getNeighborValues,
  getValueFromPos,
  getBlockFromPos,
  writeBoard)
import Suguru.Utils (Matrix, Position, getAt, getNeighbors, validPosition, setAt)
import Data.Maybe (mapMaybe, isNothing)

-- ========================================================================== --
--                                    Tipos                                   --
-- ========================================================================== --

type Options = [Int]

type BoardOptions = Matrix Options

-- ========================================================================== --
--                                   Funções                                  --
-- ========================================================================== --

-- | As opções validas para uma determinada celula ou Nothing.
--  O predicado da função filter se trata da restrição entre vizinhos
--  A lista recebe os valores retornados pela função getBlockOptions
getCellOptions :: Board -> Position -> Maybe Options
getCellOptions b p = do
  Cell block _ <- getAt b p
  blockOptions <- getBlockOptions b block
  case filter (`notElem` getNeighborValues b p) blockOptions of
    [] -> Nothing
    n -> Just n

-- | As opções validas para um bloco 
getBlockOptions :: Board -> String -> Maybe Options
getBlockOptions board block =
  getBlock board block >>= (\c -> Just(filter 
                                        (`notElem` mapMaybe getCellValue c) 
                                        [1..(length c)]
                                      ))
        
-- | Verifica se o tabuleiro foi resolvido
isSolved :: Board -> Bool
isSolved board = not (any (\(Cell _ value) -> isNothing value) (concat board))


-- | A próxima célula, ou Nothing.
-- Itera sobre todas as celulas do tabuleiro retornando as posições que ainda 
-- nao possuem valor definido. Caso a lista posList esteja vazia retorna Nothing
-- Senão, chama getNextCell' que itera sobre as posições e retorna a com menos
-- opções.
getNextCell :: Board -> Maybe Position
getNextCell board = do
  case posList of
    [] -> Nothing
    n -> foldr1 (getNextCell' board) n
    where
      posList = [Just (i, j) | 
        i <- [1..(lb)], 
        j <- [1..(lb)], 
        isNothing (getValueFromPos board (i, j))]
      lb = length board


-- | Função auxiliar para escolha de próxima célula
--  Valida posições e em seguida retorna a posição com menos opções
getNextCell' :: Board -> Maybe Position -> Maybe Position -> Maybe Position
getNextCell' board Nothing _ = Nothing
getNextCell' board _ Nothing = Nothing
getNextCell' board (Just p) (Just q) = do
  a <- getCellOptions board p
  b <- getCellOptions board q
  Just(if length a > length b then q else p)


-- | Soluciona um tabuleiro.
--  Se recebe um tabuleiro valido, retorna a matriz. Senão:
--  Bloco do: pos recebe a proxima posicao a ser processada
--            options recebe as opções dessa posição 
--            solve' é chamada para realização do backtracking
solve :: Board -> Maybe Board
solve board
  | isSolved board = Just board
  | otherwise = do
                pos <- getNextCell board
                options <- getCellOptions board pos
                solve' board pos options

-- | Função auxiliar de solução que realiza o backtracking.
--  Bloco do, b recebe um quadro com uma nova entrada.
--  Se solve b retorna nothing, uma nova chamada é feita para solve'
--  sem a cabeça de lista, ou seja, ocorre o backtracking sem a opção invalida
solve' :: Board -> Position -> Options -> Maybe Board
solve' board pos [] = Nothing
solve' board pos (value:options) = do
                                   b <- writeBoard board pos (Just value)
                                   case solve b of
                                      Nothing -> solve' board pos options
                                      b -> b
