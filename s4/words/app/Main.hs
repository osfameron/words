module Main where

import Lib
import Data

main :: IO ()
main = putStrLn $ formatGrid grid

g = makeGame grid languages
