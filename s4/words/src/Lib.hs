module Lib
    ( Game(Game, gameGrid, gameWords)
    , makeGame
    , score
    , totalWords
    , playWord
    , playGame
    , Grid
    , Cell(Cell)
    , formatGrid
    , findWord
    , findWordInLine
    , findWords
    , getLines
    , gridWithCoords
    , findWordInCellInfix
    , findWordInCellPrefix
    , cells2string
    ) where

import System.IO
import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as M

data Game = Game { gameGrid  :: Grid Cell
                 , gameWords :: M.Map String (Maybe [Cell])
                 }

type Grid a = [[a]]

data Cell = Cell (Int, Int) Char | Empty
            deriving (Eq, Ord, Show)

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let grid'  = gridWithCoords grid
      words' = M.fromList $ map (\word -> (word, Nothing)) words
  in Game grid' words'

totalWords :: Game -> Int
totalWords game = length $ M.keys (gameWords game)

score :: Game -> Int
score game = length $ catMaybes $ M.elems (gameWords game)

playWord :: Game -> String -> Game
playWord game word | not (M.member word (gameWords game)) = game
playWord game word =
  let grid = gameGrid game
      foundWord = findWord grid word
      newGame = case foundWord of
        Nothing -> game
        Just cs ->
          let words = gameWords game
              words' = M.insert word foundWord words
          in Game grid words'
  in newGame

playGame game = do
  let grid = gameGrid game
      words = gameWords game
      s = score game
      t = totalWords game
  putStrLn ((show s) ++ "/" ++ (show t))
  if s < t then do
    putStr (formatGrid grid)
    putStr "Enter a word> "
    word <- getLine
    let newGame = playWord game word
    playGame newGame
  else
    putStrLn "Congratulations!"

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipWith zipRows [0..] grid
  where zipRows y row = zipWith (zipRow y) [0..] row
        zipRow y x char = Cell (y, x) char

formatGrid :: Grid Cell -> String
formatGrid grid =
  let charGrid = map (map cell2char) grid
  in unlines charGrid

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
  let findWord' = findWord grid
      foundWords = map findWord' words
  in catMaybes foundWords

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      foundWords = map (findWordInCellInfix word) lines
  in listToMaybe (catMaybes foundWords)

getLines :: Grid Cell -> Grid Cell
getLines grid =
  let horizontal = grid
      vertical = transpose horizontal
      diagonal = diagonalize horizontal
      diagonal' = diagonalize (map reverse horizontal)
      lines = horizontal ++ vertical ++ diagonal ++ diagonal'
  in lines ++ (map reverse lines)

diagonalize :: Grid Cell -> Grid Cell
-- diagonalize grid = transpose (skew grid)
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (x:xs) = x : skew (map indent xs)
  where indent line = Empty : line

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

cell2char (Cell _ c) = c
cell2char _ = '?'

cells2string = map cell2char

findWordInCellPrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellPrefix acc (s:ss) (c:cs) | s == cell2char c
                                  = findWordInCellPrefix (c : acc) ss cs
findWordInCellPrefix acc []     _ = Just (reverse acc)
findWordInCellPrefix _    _     _ = Nothing
