module Main (main) where

    import System.IO (readFile)
    import Suguru.Board (boardFromText, getBlock)
    import Suguru.Utils (printMatrix, getNeighbors)
    import Suguru.Solver (getCellOptions, getBlockOptions)
    
    main = do
        content <- readFile "Examples/1.txt"
        let rows = lines content
        let board = boardFromText rows
        printMatrix board
        print (getCellOptions board (4, 5))
