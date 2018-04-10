module Solutions04 where

import Json


{----------------------------------------------------------------

Task: Implement the combinator "sepBy", which parses zero or more
of "p", separated by "sep".

----------------------------------------------------------------}


sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = multiple <|> empty
    where
    multiple = lift2 (:) p (many (sep `andThen` p))
    empty = lift0 []


commaSeparated :: Parser a -> Parser [a]
commaSeparated p =
    token p `sepBy` token (match ',')


test1 :: Bool
test1 =
    Just ([1,2,3,4], " ") == run (commaSeparated number) "  1, 2 ,  3  ,4 "

test2 :: Bool
test2 =
    Just ([], "  ") == run (commaSeparated number) "  "
