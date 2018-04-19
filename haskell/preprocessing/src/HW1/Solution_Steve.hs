-- executables: the module name *must* be "Main" (!)

module HW1.Solution_Steve where

import HW1.IntCSV

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
processStdin =
  do
    csv    <- stdreadCSV
    stdwriteCSV $ transform csv
    putStrLn "" -- Add in line feed to avoid commandline clutter
  where
    transform :: CSV -> CSV
    transform (CSV csv) = CSV $ fmap appendSum csv where
      appendSum l = l ++ [sum(l)]
