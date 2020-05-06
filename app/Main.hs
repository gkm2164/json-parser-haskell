module Main where

import Lib

exampleJsonString = "{" ++
    "\"name\": \"Gyeongmin Go\"," ++
    "\"email\": \"gkm2164@gmail.com\"" ++
  "}"

main :: IO ()
main = do
  print (toJson "null")
  putStrLn ""
  print $ toJson exampleJsonString