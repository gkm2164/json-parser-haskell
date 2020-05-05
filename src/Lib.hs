{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Lib
    (
      toJson
    ) where

import Data.List

data JObject = JNumber Int
             | JString String
             | JBool Bool
             | JArray [JObject]
             | JMap [JsonAssoc]
             | JNull
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

type Error = String

type STResult a = Either Error a

newtype ST a = S (State -> (STResult a, State))

app :: ST a -> State -> (STResult a, State)
app (S f) = f

instance Functor ST where
  fmap f st = S (\s -> let (v, s') = app st s in (fmap f v, s'))

instance Applicative ST where
  pure x = S (Right x,)
  stF <*> stX = S (\s ->
    let (f, s') = app stF s
        (x, s'') = app stX s'
    in (f <*> x, s''))

instance Monad ST where
  stX >>= f = S (\s ->
    let (x, s') = app stX s in case x of
      Left e -> (Left e, s)
      Right v -> app (f v) s')

takeChar :: Char -> ST Char
takeChar ch = S t
  where
    t :: String -> (STResult Char, String)
    t all@(x:xs)
      | x == ' '  = t xs
      | x == ch   = (Right x, xs)
      | otherwise = (Left $ "head is not " ++ show ch, all)

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
           | "true" `isPrefixOf` s -> (Right $ JBool True, drop 4 s)
           | "false" `isPrefixOf` s -> (Right $ JBool False, drop 5 s)
           | otherwise -> (Left "error while parse Bool", ""))

parseString :: ST JObject
parseString =
  S (\s ->
      let (_:ss) = s
          (v, nextS) = t ss "" False
      in (Right $ JString v, nextS))
  where
    t :: String -> String -> Bool -> (String, String)
    t [] _ _              = error ""
    t ('\"':xs) ret False = (ret, xs)
    t ('\\':xs) ret False = t xs ret True
    t ('\"':xs) ret True  = t xs (ret ++ "\"") False
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

parseUntil :: forall a. Char -> Char -> ST a -> ST [a]
parseUntil term delim parser = S $ parseUntil' []
  where
    parseUntil' :: [a] -> State -> (STResult [a], State)
    parseUntil' res all@(x:xs)
      | x == term = (Right res, xs)
      | otherwise = app w all
      where
        w :: ST [a]
        w = do
          elem <- parser
          ifState (\xs -> head xs == delim) (S $ parseUntil' (elem : res)) (pure (elem : res))

ifState :: (State -> Bool) -> ST a -> ST a -> ST a
ifState p t f =
  S (\s ->
       if p s
         then app t s
         else app f s)

parseNumber :: ST JObject
parseNumber =
  S (\s ->
       let (num, v) = span (`elem` "0123456789") s
        in (Right $ JNumber (read num :: Int), v))

parseNull :: ST JObject
parseNull = S (\s -> (Right JNull, drop 4 s))

parseObject :: ST JObject
parseObject = S (\s ->
  let s' = trim s
      parser = case s' of (x:_) | x == 't' || x == 'f' -> parseBool
                                | x `elem` "0123456789" -> parseNumber
                                | x == '{' -> parseMap
                                | x == '[' -> parseArray
                                | x == '"' -> parseString
                                | x == 'n' -> parseNull
                                | otherwise -> error "Unexpected type"
  in app parser s')
  where trim = dropWhile (`elem` " \n\t")

toJson :: String -> STResult JObject
toJson str = fst $ app parseObject str