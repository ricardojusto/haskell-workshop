module Main where

import Lib99

main :: IO ()
main = do
  putStrLn (show (problem1 [1, 2, 3]))
  putStrLn (show (problem1 ['1', '2', '3', '5']))

