module Lib
    ( Grid
    , formatGrid
    ) where

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines
