module Lib
    ( Grid
    , formatGrid
    , findWord
    , findWordInLine
    , findWords
    , getLines
    , Cell(Cell)
    , gridWithCoords
    , findWordInCellInfix
    , findWordInCellPrefix
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid a = [[a]]

data Cell = Cell (Int, Int) Char
            deriving (Eq, Ord, Show)

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipWith zipRows [0..] grid
  where zipRows y row = zipWith (zipRow y) [0..] row
        zipRow y x char = Cell (y, x) char

formatGrid :: Grid Char -> String
formatGrid = unlines

findWords :: Grid Char -> [String] -> [String]
findWords grid words =
  let findWord' = findWord grid
      foundWords = map findWord' words
  in catMaybes foundWords

findWord :: Grid Char -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      foundWord = or $ map (findWordInLine word) lines
  in if foundWord then Just word else Nothing

getLines :: Grid Char -> Grid Char
getLines grid =
  let horizontal = grid
      vertical = transpose horizontal
      diagonal = diagonalize horizontal
      diagonal' = diagonalize (map reverse horizontal)
      lines = horizontal ++ vertical ++ diagonal ++ diagonal'
  in lines ++ (map reverse lines)

diagonalize :: Grid Char -> Grid Char
-- diagonalize grid = transpose (skew grid)
diagonalize = transpose . skew

skew :: Grid Char -> Grid Char
skew [] = []
skew (x:xs) = x : skew (map indent xs)
  where indent line = '_' : line

findWordInLine :: String -> String -> Bool
-- findWordInLine word line = word `isInfixOf` line
-- findWordInLine word line = isInfixOf word line
findWordInLine = isInfixOf

findWordInCellInfix :: String -> [Cell] -> Maybe [Cell]
findWordInCellInfix _ [] = Nothing
findWordInCellInfix word line =
  let foundWord = findWordInCellPrefix [] word line
  in case foundWord of
       Nothing -> findWordInCellInfix word (tail line)
       Just _ -> foundWord

findWordInCellPrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellPrefix acc (s:ss) (c@(Cell _ char):cs) | s == char
                                  = findWordInCellPrefix (c : acc) ss cs
findWordInCellPrefix acc []     _ = Just (reverse acc)
findWordInCellPrefix _    _     _ = Nothing
