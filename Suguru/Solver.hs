module Suguru.Solver where

    import Suguru.Utils (Matrix, Position, getAt, getNeighbors)
    import Suguru.Board (Board, Cell (Cell), getBlock)

    type Options = [Int]
    type BoardOptions = Matrix Options

    getCellOptions :: Board -> Position -> Options
    getCellOptions board (x, y) = filter (`notElem` map(\(Cell _ v) -> v)(getNeighbors board (x, y))) (getBlockOptions board block) where
        Just (Cell block _) = getAt board (x, y)

    getBlockOptions :: Board -> String -> Options
    getBlockOptions board block = filter (`notElem` blocked) [1..n] where
        cells = getBlock board block
        n = length cells
        blocked = filter (/= -1) (map (\(Cell _ v) -> v) cells)

    -- getBoardOptions :: Board -> BoardOptions