module Main (main) where

import Data.ByteString (getLine)
import Suguru.Board (boardFromText, getBlock, cellFromTuple, writeBoard)
import Suguru.Solver (getBlockOptions, getCellOptions, solve, solve', getNextCell, getNextCell')
import Suguru.Utils (getNeighbors, printMatrix, setAt)
import System.IO (readFile)

main = do

  print "Escolha o tamanho do tabuleiro, ha disponiveis tabuleiros de 6, 8 e 10 celulas:"
  
  t_size <- Prelude.getLine
  let path = ("Examples/" ++ t_size ++ "x" ++ t_size ++".txt" )
  let a = " foi o arquivo escolhido:"  
  putStrLn (path ++ a)

  content <- readFile path
  let rows = lines content
  let board = boardFromText rows

  printMatrix board

  case solve board of
    Nothing -> print "Não foi encontrada uma solução para este tabuleiro"
    Just solution -> printMatrix solution
