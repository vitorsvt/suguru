module Main (main) where

    import System.IO (readFile, IOMode (ReadMode) )

    import Board (Matrix (Matrix), boardFromText)

    main = do
        content <- readFile "../Examples/1.txt"
        let rows = lines content
        print (boardFromText rows)
