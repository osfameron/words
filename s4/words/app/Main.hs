module Main where

import Lib
import Data

g = makeGame grid languages

main :: IO ()
main = playGame g

