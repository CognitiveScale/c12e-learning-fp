module Json where

import Data.Char (isDigit, digitToInt)

data Parser a = Parser (String -> Maybe (a, String))

run :: Parser a -> String -> Maybe (a, String)
run (Parser f) = f

-- Lifts --------------------------------------------------

lift0 :: a -> Parser a
lift0 a = Parser helper where
    helper input = Just (a, input)

lift1 :: (a -> b) -> Parser a -> Parser b
lift1 f pa = Parser helper where
    -- helper :: String -> Maybe (b, String)
    helper input =
    -- (run pa) :: String -> Maybe (a, String)
    -- (run pa) input :: Maybe (a, String)
        case (run pa) input of
            Nothing -> Nothing
            Just (a, rest) -> Just (f a, rest)

-- lift1 = \f -> \a -> Parser helper
-- lift1 f = \a -> Parser helper

lift2 :: (a -> b -> c) -> (Parser a -> Parser b -> Parser c)
lift2 = undefined
-- Parser a is parser of bool (reading + or -) parseSign
-- Parser b is number
-- Parser c is

-- Utility Functions --------------------------------------

checkChar :: (Char -> Bool) -> Parser Char
checkChar f = Parser helper where
  helper [] = Nothing
  helper (h:hs) = 
    if f h 
    then Just (h, hs) 
    else Nothing

stringToInt :: String -> Int
stringToInt = read

some :: Parser x -> Parser [x]
some p = Parser helper where
  helper input = -- helper :: String -> Maybe(x, String)
    case run p input of -- :: Maybe(x, String)
      Nothing -> Nothing
      Just (x, rest) -> -- :: Maybe([x], String)
        case helper rest of
          Nothing -> Just ([x], rest)
          Just (xs, rrest) -> Just (x : xs, rrest)

-- Parsers ------------------------------------------------

number :: Parser Int
number = lift1 stringToInt (some digit)
{-
number =
    let digits = some digit -- digits :: Parser String
        f = stringToInt -- f :: String -> Int
    in lift1 f digits
-}

signedNumber :: Parser Int
signedNumber = lift2 helper parseSign number where
    helper :: Bool -> Int -> Int
    helper = undefined
    parseSign :: Parser Bool
    parseSign = undefined

-- sample input --
-- run signedNumber "+123abc" == Just (123, "abc")
-- run signedNumber "-123abc" == Just (-123, "abc")
-- run signedNumber "123" == Nothing

anychar :: Parser Char
anychar = Parser helper where
  helper []     = Nothing
  helper (c:cs) = Just (c, cs)

digit :: Parser Char
digit = checkChar isDigit


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

