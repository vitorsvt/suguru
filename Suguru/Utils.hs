module Suguru.Utils where

    -- Matrizes são uma lista de listas
    newtype Matrix t = Matrix [[t]]

    -- Implementa o tipo Show
    instance Show t => Show (Matrix t) where
        show (Matrix (a:xs)) = show a ++ "\n" ++ show (Matrix xs)
        show (Matrix []) = ""