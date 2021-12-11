module Main (main) where

import Data.ByteString (getLine)
import Suguru.Board (boardFromText, getBlock, cellFromTuple, writeBoard)
import Suguru.Solver (getBlockOptions, getCellOptions, solve, solve', getNextCell, getNextCell')
import Suguru.Utils (getNeighbors, printMatrix, setAt)
import System.IO (readFile)

main = do
  content <- readFile "Examples/3.txt"
  let rows = lines content
  let board = boardFromText rows

  -- print "Indexado a partir de 1"
  -- print "Linha:"
  -- is <- Prelude.getLine
  -- print "Coluna:"
  -- js <- Prelude.getLine

  -- let i = (read is :: Int)
  -- let j = (read js :: Int)

  printMatrix board

  case solve board of
    Nothing -> print "Nothing"
    Just solution -> printMatrix solution
