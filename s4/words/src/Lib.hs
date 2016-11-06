module Lib
    ( Grid
    , formatGrid
    , findWord
    , findWordInLine
    , findWords
    , getLines
    ) where

import Data.List (isInfixOf, transpose)
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
  let lines = getLines grid
      foundWord = or $ map (findWordInLine word) lines
  in if foundWord then Just word else Nothing

getLines :: Grid -> [String]
getLines grid =
  let horizontal = grid
      vertical = transpose horizontal
      diagonal = diagonalize horizontal
      diagonal' = diagonalize (map reverse horizontal)
      lines = horizontal ++ vertical ++ diagonal ++ diagonal'
  in lines ++ (map reverse lines)

diagonalize :: Grid -> Grid
-- diagonalize grid = transpose (skew grid)
diagonalize = transpose . skew

skew :: Grid -> Grid
skew [] = []
skew (x:xs) = x : skew (map indent xs)
  where indent line = '_' : line

findWordInLine :: String -> String -> Bool
-- findWordInLine word line = word `isInfixOf` line
-- findWordInLine word line = isInfixOf word line
findWordInLine = isInfixOf
