module Main where

import Lib
import Data
import System.Random

main :: IO ()
main = do
  gen <- newStdGen
  let grid' = fillInBlanks gen grid
      game = makeGame grid' languages
  playGame game


