{-----------------------------------------------------------------

Task: a small tweak on HW2 -- write a program that reads numbers from
stdin and computes column sums in addition to row sums.

For example, if the input from stdin is

1 2 3
4 5 6

the output to stdout should be

1 2 3 6
4 5 6 15
5 7 9 21

Just like HW2, if there is a parse error when reading a number, the
program should stop immediately (as before, be sure to propagate the
error all the way up to "main" and handle it there).

One of the goals of the homework is to practice writing things in an
accumulating/streaming style.  So, just like HW2, process stdin a line
at a time.  This also means that you are not allowed to use
Data.IORef .

You must figure out the type signatures you need.

-----------------------------------------------------------------}


module HW3.Problem where

import System.IO (isEOF)
import System.Exit (die)
import Text.Read (readEither)


type Result a = Either ErrorMsg a
type ErrorMsg = String


main :: IO ()
main =
    undefined


-- processStdin :: ???
processStdin =
    undefined


addLists :: [Int] -> [Int] -> [Int]
addLists = zipWith (+)
