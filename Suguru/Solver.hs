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

type Options = [Int]

type BoardOptions = Matrix Options

-- retorna as opções para uma determinada celula
-- O predicado da função filter se trata da restrição entre vizinhos
-- A lista recebe os valores retornados pela função getBlockOptions
getCellOptions :: Board -> Position -> Maybe Options
getCellOptions b p = do
  Cell block _ <- getAt b p
  blockOptions <- getBlockOptions b block
  case filter (`notElem` getNeighborValues b p) blockOptions of
    [] -> Nothing
    n -> Just n

getBlockOptions :: Board -> String -> Maybe Options
getBlockOptions board block =
  getBlock board block >>= (\cells -> Just(filter (`notElem` mapMaybe getCellValue cells) [1..(length cells)]))

isSolved :: Board -> Bool
isSolved board = null [True | i <- [1..(length board)], j <- [1..(length board)], isNothing (getValueFromPos board (i, j))]

getNextCell :: Board -> Maybe Position
getNextCell board = do
  case [Just (i, j) | i <- [1..(length board)], j <- [1..(length board)], isNothing (getValueFromPos board (i, j))] of
    [] -> Nothing
    n -> foldr1 (getNextCell' board) n

getNextCell' :: Board -> Maybe Position -> Maybe Position -> Maybe Position
getNextCell' board Nothing _ = Nothing
getNextCell' board _ Nothing = Nothing
getNextCell' board (Just x) (Just y) = do
  a <- getCellOptions board x
  b <- getCellOptions board y
  Just(if length a > length b then y else x)

-- pretende-se implantar o backtracking na funcao abaixo
solve :: Board -> Maybe Board
solve board = do
  if isSolved board then Just board
  else getNextCell board >>= (\pos -> getCellOptions board pos >>= solve' board pos)

solve' :: Board -> Position -> Options -> Maybe Board
solve' board pos [] = Nothing
solve' board pos (value:options) = do
  b <- writeBoard board pos (Just value)
  case solve b of
    Nothing -> solve' board pos options
    b -> b
