module Json where

import Data.Char (isDigit, digitToInt)

data Parser a = Parser (String -> Maybe (a, String))

run :: Parser a -> String -> Maybe (a, String)
run (Parser f) = f

lift0 :: a -> Parser a
lift0 a = Parser (\s -> Just(a, s))

lift1 :: (a -> b) -> Parser a -> Parser b
lift1 f p = Parser helper where
  helper input = 
    case (run p) input of -- :: Maybe (a, String)
      Nothing -> Nothing
      Just (x, rest) -> Just(f x, rest)

lift2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 = undefined

stringToInt :: String -> Int
stringToInt = read

number :: Parser Int
number = lift1 stringToInt $ some digit

anychar :: Parser Char
anychar = Parser helper where
  helper []     = Nothing
  helper (c:cs) = Just (c, cs)

checkChar :: (Char -> Bool) -> Parser Char
checkChar f = Parser helper where
  helper [] = Nothing
  helper (h:hs) = 
    if f h 
    then Just (h, hs) 
    else Nothing

{-digit :: Parser Int-}
{-digit = Parser helper where-}
  {-helper []   = Nothing-}
  {-helper (c:cs) = -}
    {-if isDigit c -}
    {-then Just((digitToInt c, cs)) -}
    {-else Nothing-}
digit :: Parser Char
digit = checkChar isDigit

some :: Parser x -> Parser [x]
-- one or more times
some p = Parser helper where
  helper input = -- helper :: String -> Maybe(x, String)
    case run p input of -- :: Maybe(x, String)
      Nothing -> Nothing
      Just (x, rest) -> -- :: Maybe([x], String)
        case helper rest of
          Nothing -> Just ([x], rest)
          Just (xs, rrest) -> Just (x : xs, rrest)

-- Values --

true :: Parser Bool
true = Parser helper where
  helper ('t':'r':'u':'e': rest) = Just(True, rest)
  helper _ = Nothing

false :: Parser Bool
false = Parser helper where
  helper ('f':'a':'l':'s':'e': rest) = Just(False, rest)
  helper _ = Nothing

nullj :: Parser ()
nullj = Parser helper where
  helper ('n':'u':'l':'l': rest) = Just((), rest)
  helper _ = Nothing

