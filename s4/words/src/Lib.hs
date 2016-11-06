module Lib
    ( Grid
    , formatGrid
    , findWord
    , findWordInLine
    , findWords
    ) where

import Data.List (isInfixOf)
import Data.Maybe (catMaybes)

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines

findWords :: Grid -> [String] -> [String]
findWords grid words =
  let findWord' = findWord grid
      foundWords = map findWord' words
  in catMaybes foundWords

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let lines = grid ++ (map reverse grid)
      foundWord = or $ map (findWordInLine word) lines
  in if foundWord then Just word else Nothing

findWordInLine :: String -> String -> Bool
-- findWordInLine word line = word `isInfixOf` line
-- findWordInLine word line = isInfixOf word line
findWordInLine = isInfixOf
