module Main where

import Lib99

main :: IO ()
main = do
  putStrLn (show (myLast [1, 2, 3]))
  putStrLn (show (myLast ['1', '2', '3', '5']))

