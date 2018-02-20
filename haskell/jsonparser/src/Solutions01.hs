module Solutions01 where

import Json (many, true, false, (<|>), run)


{----------------------------------------------------------------

Part 1: Predict the output of these examples.

This is for people who missed the session last week.

----------------------------------------------------------------}


input1 = "truetruefalsetrue"

input2 = "falsetruefalsetrue"

example1 =
    run (many (true <|> false)) input1
--Just((True : True : False : True, "")

example2 =
    run (many (true <|> false)) input2
--Just((False : True : False : True, "")

example3 =
    run (many true <|> many false) input1
--Just((True : True, "falsetrue")

example4 =
    run (many true <|> many false) input2
--Just(([], "falsetruefalsetrue"))


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
maybeLift0 a = Just(a)


maybeLift1 :: (a -> b) -> Maybe a -> Maybe b
maybeLift1 f ma = case ma of
  Just(a) -> Just(f a)
  Nothing -> Nothing


maybeLift2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeLift2 f = apply where
  apply ma mb = case (maybeLift1 f ma) of
    Just(fbc) -> maybeLift1 fbc mb
    Nothing   -> Nothing


--tests
ml0 = maybeLift0 "test"

ml1a = maybeLift1 (\x -> x + 1) (Just 2)
ml1b = maybeLift1 (\x -> x + 1) Nothing

ml2a = maybeLift2 (\x y -> x + y) (Just 2) (Just 3)
ml2b = maybeLift2 (\x y -> x + y) Nothing (Just 3)
ml2c = maybeLift2 (\x y -> x + y) (Just 2) Nothing
ml2d = maybeLift2 (\x y -> x + y) Nothing Nothing

----------------------------------------------------------------

type Stringy a = String -> (a, String)


-- Hint: these will be similar to what we did for Parsers

stringyLift0 :: a -> Stringy a
stringyLift0 a = \s -> (a, s)

-- stringLift1:: (u -> v) -> Stringy u -> Stringy v
stringyLift1 :: (a -> b) -> Stringy a -> Stringy b
stringyLift1 f sa = \s -> case sa s of
  (a, s2) -> (f a, s2)


stringyLift2 :: (a -> b -> c) -> Stringy a -> Stringy b -> Stringy c
stringyLift2 f sa sb = \s -> case (stringyLift1 f sa) s of
  (fbc, s2) -> (stringyLift1 fbc sb) s2


----------------------------------------------------------------

listLift0 :: a -> [a]
listLift0 a = a : []


listLift1 :: (a -> b) -> [a] -> [b]
listLift1 f la = recurse la where
  recurse l = case l of
    []    -> []
    h : t -> (f h) : recurse t


listLift2 :: (a -> b -> c) -> [a] -> [b] -> [c]
listLift2 f la lb = trv (listLift1 f la) lb where
  trv lg l = recurse [] lg where
    recurse acc remaining = case remaining of
      []    -> acc
      h : t -> acc ++ recurse ((listLift1 h l)) t

-- test
l0 = listLift0 "test"

l1 = listLift1 (\x -> x + 1) (1 : 2 : [])

l2 = listLift2 (\x y -> x + y) (1 : 2 : []) (7 : 8 : [])