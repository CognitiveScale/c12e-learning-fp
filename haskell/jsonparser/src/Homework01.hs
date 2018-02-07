module Homework01 where

import Json (many, true, false, (<|>), run)


{----------------------------------------------------------------

Part 1: Predict the output of these examples.

This is for people who missed the session last week.

----------------------------------------------------------------}


input1 = "truetruefalsetrue"

input2 = "falsetruefalsetrue"

example1 =
    run (many (true <|> false)) input1

example2 =
    run (many (true <|> false)) input2

example3 =
    run (many true <|> many false) input1

example4 =
    run (many true <|> many false) input2



{----------------------------------------------------------------

Part 2: Implement the functions below.

The purpose is to give you more practice with this "lift" pattern,
which turns out to be a fundamental pattern that will show up in many
places (under different names, and organized differently).

The assignment is intentionally a bit vague.  Try to write the
implementation that you think is intended, based on reading the
function signature.

----------------------------------------------------------------}


maybeLift0 :: a -> Maybe a
maybeLift0 = undefined


maybeLift1 :: (a -> b) -> Maybe a -> Maybe b
maybeLift1 = undefined


maybeLift2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeLift2 = undefined


----------------------------------------------------------------

type Stringy a = String -> (a, String)


-- Hint: these will be similar to what we did for Parsers

stringyLift0 :: a -> Stringy a
stringyLift0 = undefined


stringyLift1 :: (a -> b) -> Stringy a -> Stringy b
stringyLift1 = undefined


stringyLift2 :: (a -> b -> c) -> Stringy a -> Stringy b -> Stringy c
stringyLift2 = undefined


----------------------------------------------------------------

listLift0 :: a -> [a]
listLift0 = undefined


listLift1 :: (a -> b) -> [a] -> [b]
listLift1 = undefined


listLift2 :: (a -> b -> c) -> [a] -> [b] -> [c]
listLift2 = undefined
