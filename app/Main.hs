module Main where

import Lib

main :: IO ()
main = do
  print (toJson "null")
  putStrLn ""
  print (toJson "{\"name\":  1234}")