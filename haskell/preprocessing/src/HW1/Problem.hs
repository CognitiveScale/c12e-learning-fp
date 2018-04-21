module HW1.Problem where

import System.IO (isEOF)


{----------------------------------------------------------------

Homework: write a program that reads lines of numbers from stdin and
writes back those lines, including their sums, to stdout.

For example,

$ cat data/numbers.txt
1 2 3
4 5 6

$ stack exec homework1 < data/numbers.txt
1 2 3 6
4 5 6 15


For convenience, assume space-delimited input/output.  You'll find the
"words" and "unwords" functions handy.

You'll need to use "isEOF" before any call to "getLine" to check for
the end of the input.

----------------------------------------------------------------}


main :: IO ()
main =
    processStdin

processStdin :: IO ()
processStdin = do
  line <- isEOF
  if not line then
    (processLineStdin >>= putStrLn >> processStdin)
  else
    putStrLn "Done!"

processLineStdin :: IO String
processLineStdin =  do
  ls <- getLine 
  return $ processLine ls

processLine :: String -> String
processLine nums = 
  unwords . (map show) $ lsn ++ [ns]
  where
    lsn = str2ints nums
    ns  = sum lsn

str2ints :: String -> [Int]
str2ints xs = map read (words xs)


-- Yes, this is partial function.
-- Yes, partial functions are bad.
-- Yes, we will eventually fix this.
--
readInt :: String -> Int
readInt = read
