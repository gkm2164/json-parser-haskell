module Main where

import Lib

main :: IO ()
main = do
  print (app parseBool "true")
  putStrLn ""
  print mapInfo
  putStrLn ""
  print (app parseString "\"Hello World!\"")
  putStrLn ""
  print stringInfo
  print (toJson "null")