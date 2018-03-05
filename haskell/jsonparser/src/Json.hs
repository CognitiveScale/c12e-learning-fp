module Json where


import Data.Char (isDigit, isSpace, toLower,  isControl)
import Data.List (intercalate)


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


match :: Char -> Parser ()
match c = void (checkChar (==c))


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


lift3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
lift3 f pa pb pc =
    lift2 ($) (lift2 f pa pb) pc


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

-- Parse a character that occurs inside a JSON string, including
-- proper treatment of escape characters.
--
char :: Parser Char
char = Parser go
    where
    go []               = Nothing
    go ('\\':'\\':rest) = Just ('\\', rest)
    go ('\\':'"':rest)  = Just ('"', rest)
    go ('\\':'n':rest)  = Just ('\n', rest)
    go ('\\':'r':rest)  = Just ('\r', rest)
    go ('\\':'t':rest)  = Just ('\t', rest)
    go ('"':rest)       = Nothing
    go ('\\':rest)      = Nothing
    go (ch:rest)        =
        if isControl ch then Nothing else Just (ch, rest)

require :: Char -> Parser ()
require c = void $ checkChar (\x -> x == c)

requireToken :: Char -> Parser ()
requireToken c = token (require c)

------------------------------

openArray :: Parser Char
openArray = checkChar (== '[')
--openArray = checkChar (\c -> c == '[')

closeArray :: Parser Char
closeArray = checkChar (== ']')

numbers :: Parser [Int]
numbers = many (token number)

json :: Parser Json
json = jsonNull
     <|> jsonBool
     <|> jsonInt
     <|> jsonString
     <|> jsonArray
     <|> jsonObject

jsonNull :: Parser Json
jsonNull = token $ lift1 (\_ -> JsonNull) nullj

jsonBool :: Parser Json
jsonBool = token $ lift1 JsonBool (true <|> false)


jsonInt :: Parser Json
jsonInt = token $ lift1 JsonInt number

legalString = token $ (require '"') `andThen` ((many char) `thenSkip` (require '"'))

jsonString :: Parser Json
jsonString = token $ lift1 JsonString legalString

jsonArray :: Parser Json
jsonArray = token $ lift1 JsonArray array

separated :: Char -> Parser a -> Parser [a]
separated c pa = multiple <|> single <|> empty where
  empty = lift0 []
  single = lift1 (\x -> [x]) (token pa)
  multiple = lift2 (\x y -> x : y) (token pa) ((requireToken c) `andThen` (c `separated` pa))

jsonObject :: Parser Json
jsonObject = token $ lift1 JsonObject object where
  object = (requireToken '{') `andThen` (fields `thenSkip` (requireToken '}'))
  fields = ',' `separated` field
  field = token $ lift2 (\x y -> (x,y)) legalString ((requireToken ':') `andThen` (token json))

data Json 
    = JsonInt Int 
    | JsonBool Bool  
    | JsonNull
    | JsonString String
    | JsonArray [Json]
    | JsonObject [(String, Json)]
    deriving Show


array :: Parser [Json]
array = (token openArray) `andThen` ((',' `separated` json) `thenSkip` (token closeArray `andThen` spaces))

-- TODO:
-- Enforce uniqueness of field names in objects
-- Handle more complex escape sequences in strings

-- Toy usage - prettification with variable indent
prettyJson :: Int -> Json -> String
prettyJson = format 0 where
  format :: Int -> Int -> Json -> String
  format depth indent = format_i depth where
    format_i depth = (indentStr depth) . (format_depth depth) where
      format_depth :: Int -> Json -> String
      format_depth n = format_element where
        format_element :: Json -> String
        format_element (JsonInt i)    = show i
        format_element (JsonBool b)   = fmap toLower $ show b
        format_element JsonNull       = "null"
        format_element (JsonString s) = quote s
        format_element (JsonArray a)  = subCollection '[' ']' (format_i (n+1)) a
        format_element (JsonObject o) = subCollection '{' '}' format_field o

        format_field :: (String, Json) -> String
        format_field (name, value) = indentStr (n+1) $ (quote name) ++ (": " ++ (format_depth (n+1) value))

        subElements :: (a -> String) -> [a] -> String
        subElements f l = intercalate (',' : [lf]) $ fmap f l

        subCollection :: Char -> Char -> (a -> String) -> [a] -> String
        subCollection start end f = (embed start end) . subElements f

        embed :: Char -> Char -> String -> String
        embed start end s = start : lf : (s ++ (lf : (indentStr n [end])))

        quote s = '"' : (s ++ "\"")

      indentStr :: Int -> String -> String
      indentStr d s = (replicate (d*indent) ws) ++ s

      lf = '\n'
      ws = ' '

-- test
jsonOrEmpty :: Maybe Json -> Json
jsonOrEmpty (Just j)  = j
jsonOrEmpty Nothing   = JsonNull

rawStr = "{ \"a\": [ 1,2, 3 ], \"b\": { \"a1\": null}}"
j = jsonOrEmpty $ fmap fst (run json rawStr)
testPretty n = putStrLn $ prettyJson n j
