module Lib
    (
      parseBool,
      parseString,
      parseArray,
      parseObject,
      app,
      mapInfo,
      stringInfo,
      toJson
    ) where

import Data.List

data JObject = JNumber Int
             | JString String
             | JBool Bool
             | JArray [JObject]
             | JMap [JsonAssoc]
  deriving Show

type JsonAssoc = (JObject, JObject)

--instance Show JObject where
--  show (JNumber x) = show x
--  show (JString x) = show x
--  show (JBool True) = "true"
--  show (JBool False) = "false"
--  show (JArray xs) = "[" ++ intercalate ", " (map show xs) ++ "]"
--  show (JMap xs) = "{" ++ intercalate ", " (map (\(k, v) -> "\"" ++ k ++ "\":" ++ show v) xs) ++ "}"

type State = String

newtype ST a = S (State -> (Maybe a, State))

app :: ST a -> State -> (Maybe a, State)
app (S f) = f

instance Functor ST where
  fmap f st = S (\s -> let (v, s') = app st s in (fmap f v, s'))

instance Applicative ST where
  pure x = S (\s -> (Just x, s))
  stF <*> stX = S (\s ->
    let (f, s') = app stF s
        (x, s'') = app stX s'
    in (f <*> x, s''))

instance Monad ST where
  stX >>= f = S (\s ->
    let (x, s') = app stX s in case x of
      Nothing -> (Nothing, s)
      Just v -> app (f v) s')

takeChar :: Char -> ST Char
takeChar ch = S t
  where
    t :: String -> (Maybe Char, String)
    t all@(x:xs)
      | x == ' '  = t xs
      | x == ch   = (Just x, xs)
      | otherwise = (Nothing, all)

parseAssoc :: ST JsonAssoc
parseAssoc = do
  key <- parseString
  _ <- takeChar ':'
  value <- parseObject
  return (key, value)

parseBool :: ST JObject
parseBool =
  S (\s ->
       case () of
         _
           | "true" `isPrefixOf` s -> (Just $ JBool True, drop 4 s)
           | "false" `isPrefixOf` s -> (Just $ JBool False, drop 5 s)
           | otherwise -> (Nothing, ""))

parseString :: ST JObject
parseString =
  S (\s ->
      let (_:ss) = s
          (v, nextS) = t ss "" False
      in (Just $ JString v, nextS))
  where
    t :: String -> String -> Bool -> (String, String)
    t [] _ _              = error ""
    t ('\"':xs) ret False = (ret, xs)
    t ('\"':xs) ret True  = t xs (ret ++ "\"") False
    t ('\\':xs) ret False = t xs ret True
    t ('\\':xs) ret True  = t xs (ret ++ "\\") False
    t (x:xs) ret _        = t xs (ret ++ [x]) False

parseArray :: ST JObject
parseArray = do
  _ <- takeChar '['
  elems <- parseUntil ']' ',' parseObject
  _ <- takeChar ']'
  return $ JArray elems

parseMap :: ST JObject
parseMap = do
  _ <- takeChar '{'
  objs <- parseUntil '}' ',' parseAssoc
  _ <- takeChar '}'
  return $ JMap objs

parseUntil :: Char -> Char -> ST a -> ST b
parseUntil _ _ _ = error "unimplemented yet"

parseNumber :: ST JObject
parseNumber = S (error "unimplemented yet")

parseObject :: ST JObject
parseObject = S (\s -> (Nothing, s))

toJson :: String -> Maybe JObject
toJson str = fst $ app parseObject str

mapInfo = JMap [(JString "a", JString "a"), (JString "b", JString "b")]
stringInfo = JString "xx"