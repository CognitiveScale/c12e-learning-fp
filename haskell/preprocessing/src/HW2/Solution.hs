module HW2.Solution where

import System.IO (isEOF)
import System.Exit (die)
import Text.Read (readEither)
import Control.Monad (sequence)


{----------------------------------------------------------------

Task: implement "processStdin".  The overall task is the same as the
last homework, but you must use the new version of "readInt" (the old
one was a partial function, this one is total).

Do not use any partial functions in your solution.

Note that we've changed the output type of "processStdin". It now
explicitly conveys the idea that errors can happen.

----------------------------------------------------------------}


type Result a = Either ErrorMsg a
type ErrorMsg = String


main :: IO ()
main = do
    res <- processStdin
    case res of
        Left msg -> die msg
        Right _  -> pure ()


processStdin :: IO (Result ())
processStdin = 
  do
  eof <- isEOF
  if eof then
    (pure . pure) ()
  else
    {-processLineStdin >>= print . formatLine >> processStdin-}
    -- or:
    do 
    r <- processLineStdin
    case r of 
      Right nums -> (print . (unwords . (fmap show))) nums >> processStdin
      Left msg -> pure $ Left msg

processLineStdin :: IO (Result [Int])
processLineStdin =  
  do
  ls <- getLine 
  return $ processLine ls

formatLine :: Result [Int] -> String
formatLine (Left a) = show a
formatLine (Right b) = (unwords . (fmap show)) b

processLine :: String -> Result [Int]
processLine xs = sequence . appendSum $ str2ints xs

appendSum :: [Result Int] -> [Result Int]
appendSum xs = xs ++ [numSum xs]

numSum :: [Result Int] -> Result Int
numSum xs = fmap sum $ insideOut' xs

str2ints :: String -> [Result Int]
str2ints xs = map readEither (words xs)

readInt :: String -> Result Int
readInt = readEither

insideOut' :: [Result a] -> Result [a]
insideOut' = sequence

-- so far failed to implement this...
insideOut :: [Result a] -> Result [a]
insideOut [] = Right []
insideOut ((Left i):xs) = Left i
insideOut ((Right i):xs) = (insideOut xs) `sum` (Right [i])
  
