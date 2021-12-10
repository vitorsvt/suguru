module Main (main) where

import Data.ByteString (getLine)
import Suguru.Board (boardFromText, getBlock, cellFromTuple)
import Suguru.Solver (getBlockOptions, getBlockFromPos, getCellOptions, getValueFromPos, getBoardOptions)
import Suguru.Utils (getNeighbors, printMatrix, setAt)
import System.IO (readFile)

main = do
  content <- readFile "Examples/2.txt"
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
  
  let n = getBoardOptions board (1,1)
  
  print( (getBlockFromPos n (i, j)))
  print ( (getValueFromPos n (i, j)))
  print (getCellOptions board (i, j))
  