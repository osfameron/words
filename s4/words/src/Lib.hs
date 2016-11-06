module Lib
    ( Grid
    , formatGrid
    , findWord
    , findWordInLine
    , findWords
    ) where

import Data.List (isInfixOf)

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines

-- findWords :: Grid -> [String] -> [String]
findWords grid words =
  let findWord' = findWord grid
  in map findWord' words

findWord :: Grid -> String -> Bool
findWord grid word =
  let lines = grid ++ (map reverse grid)
  in or $ map (findWordInLine word) lines

findWordInLine :: String -> String -> Bool
-- findWordInLine word line = word `isInfixOf` line
-- findWordInLine word line = isInfixOf word line
findWordInLine = isInfixOf
