module Lib
    ( Grid
    , formatGrid
    , findWord
    , findWordInLine
    ) where

import Data.List (isInfixOf)

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines

findWord :: Grid -> String -> Bool
findWord grid word = or $ map (findWordInLine word) grid

findWordInLine :: String -> String -> Bool
-- findWordInLine word line = word `isInfixOf` line
-- findWordInLine word line = isInfixOf word line
findWordInLine = isInfixOf
