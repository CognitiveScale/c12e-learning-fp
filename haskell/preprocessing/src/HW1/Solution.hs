module HW1.Solution where

import System.IO (isEOF)


main :: IO ()
main =
    processStdin


processStdin :: IO ()
processStdin =
    isEOF >>= \eof ->
    if eof then
        pure ()
    else
        processLine >> processStdin


processLine =
    getLine >>= \str ->
    let nums  = map readInt (words str)
        nums' = nums ++ [sum nums]
        line  = unwords (map show nums')
    in
        putStrLn line


-- Yes, this is partial function.
-- Yes, partial functions are bad.
-- Yes, we will eventually fix this.
--
readInt :: String -> Int
readInt = read


----------------------------------------------------------------
-- Same functions, written with do-notation
----------------------------------------------------------------

processStdin_do =
    do
    eof <- isEOF
    (if eof then
        pure ()
    else
        do
        processLine_do
        processStdin_do)


processLine_do =
    do
    str <- getLine
    let nums  = map readInt (words str)
        nums' = nums ++ [sum nums]
        line  = unwords (map show nums')
    putStrLn line
