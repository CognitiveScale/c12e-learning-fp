module Json where

import Data.Char (isDigit, isSpace)


data Parser a = Parser (String -> Maybe (a, String))

run :: Parser a -> String -> Maybe (a, String)
run (Parser f) = f


----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

anyChar :: Parser Char
anyChar = Parser helper where
  helper []     = Nothing
  helper (c:cs) = Just (c, cs)


checkChar :: (Char -> Bool) -> Parser Char
checkChar f = Parser helper where
  helper [] = Nothing
  helper (h:hs) =
    if f h
    then Just (h, hs)
    else Nothing


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


--lift2 :: (a -> b -> c) -> (Parser a -> (Parser b -> (Parser c)))
lift2 :: (a -> b -> c) -> (Parser a -> Parser b -> Parser c)
lift2 f = apply where
     -- apply :: Parser a -> Parser b -> Parser c
     apply pa pb = Parser helper where
       helper input =
           case (run pa) input of
               Nothing -> Nothing
               Just (a, r1) -> case (run pb) r1 of
                   Nothing -> Nothing
                   Just (b, r2) -> Just (f a b, r2)


oldSome :: Parser x -> Parser [x]
oldSome p = Parser helper where
  helper input = -- helper :: String -> Maybe(x, String)
    case run p input of -- :: Maybe(x, String)
      Nothing -> Nothing
      Just (x, rest) -> -- :: Maybe([x], String)
        case helper rest of
          Nothing -> Just ([x], rest)
          Just (xs, rrest) -> Just (x : xs, rrest)


many :: Parser x -> Parser [x]
many p = some p <|> lift0 []


some :: Parser x -> Parser [x]
some p = lift2 (:) p (many p)


-- pa `orElse` pb === orElse pa pb
orElse :: Parser a -> Parser a -> Parser a
orElse pa pb = Parser helper where
  helper input =
    case run pa input of
      Nothing -> run pb input
      result -> result


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = orElse


failure :: Parser a
failure = Parser helper where
   helper _input = Nothing


{-
Laws:

  Left identity:      failure <|> pa === pa
  Right identity:     pa <|> failure === pa
  Associativity:      (pa <|> pb) <|> pc === pa <|> (pb <|> pc)


Prefix vs infix:

  pa <|> pb <|> pc
  === orElse (orElse pa pb) pc
  === pa `orElse` pb `orElse` pc
-}


void :: Parser a -> Parser ()
void p = lift1 (\_ -> ()) p


-- example: spaces `andThen` p
andThen :: Parser a -> Parser b -> Parser b
andThen = lift2 (\_ b -> b)


-- example: p `thenSkip` spaces
thenSkip :: Parser a -> Parser b -> Parser a
thenSkip = lift2 (\a _ -> a)



----------------------------------------------------------------
-- JSON parsing
----------------------------------------------------------------

true :: Parser Bool
true = Parser helper where
  helper ('t':'r':'u':'e': rest) = Just (True, rest)
  helper _ = Nothing


false :: Parser Bool
false = Parser helper where
  helper ('f':'a':'l':'s':'e': rest) = Just (False, rest)
  helper _ = Nothing


nullj :: Parser ()
nullj = Parser helper where
  helper ('n':'u':'l':'l': rest) = Just ((), rest)
  helper _ = Nothing


number :: Parser Int
number = lift1 stringToInt (some digit)


digit :: Parser Char
digit = checkChar isDigit


stringToInt :: String -> Int
stringToInt = read


space :: Parser ()
space =  void (checkChar isSpace)


spaces :: Parser ()
spaces = void (many space)


token :: Parser a -> Parser a
token p = spaces `andThen` p

------------------------------

openArray :: Parser Char
openArray = checkChar (== '[')
--openArray = checkChar (\c -> c == '[')

closeArray :: Parser Char
closeArray = checkChar (== ']')

numbers :: Parser [Int]
numbers = many (token number)

json :: Parser Json
json = jsonNull <|> jsonBool

jsonNull :: Parser Json
jsonNull = token $ lift1 (\_ -> JsonNull) nullj

jsonBool :: Parser Json
jsonBool = token $ lift1 JsonBool (true <|> false)

data Json 
    = JsonInt Int 
    | JsonBool Bool  
    | JsonNull  
    | JsonString String
    | JsonArray [Json]
    | JsonObject [(String, Json)]
    deriving Show


array :: Parser [Json]
array = (token openArray) `andThen`  (many json)  `thenSkip` (token closeArray `andThen` spaces)

