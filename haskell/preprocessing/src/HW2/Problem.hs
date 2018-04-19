module HW2.Problem where

import System.IO (isEOF)
import System.Exit (die)
import Text.Read (readEither)


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
    error "processStdin: not implemented"


readInt :: String -> Result Int
readInt = readEither
