module Main (main) where

import Data.ByteString (getLine)
import Suguru.Board (boardFromText, getBlock)
import Suguru.Solver (getBlockOptions, getCellOptions)
import Suguru.Utils (getNeighbors, printMatrix)
import System.IO (readFile)

main = do
  content <- readFile "Examples/1.txt"
  let rows = lines content
  let board = boardFromText rows

  print "Indexado a partir de 1"
  print "Linha:"
  is <- Prelude.getLine
  print "Coluna:"
  js <- Prelude.getLine

  let i = (read is :: Int)
  let j = (read js :: Int)

  printMatrix board
  print (getCellOptions board (i, j))
