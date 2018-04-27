module HW3.Solution where

import System.IO (isEOF)
import System.Exit (die)
import Text.Read (readEither)

import HW2.Solution (traverseEither, sequenceEither)

type Result a = Either ErrorMsg a
type ErrorMsg = String


main :: IO ()
main = do
    res <- processStdin []
    case res of
        Left msg -> die msg
        Right _  -> pure ()


processStdin :: [Int] -> IO (Result ())
processStdin acc =
    do
    eof <- isEOF
    if eof then
        do
        putStrLn $ unwords (map show acc)
        pure (Right ())
    else
        do
        res <- processLine acc
        case res of
            Left msg -> pure (Left msg)
            Right cols  -> processStdin cols


processLine :: [Int] -> IO (Result [Int])
processLine acc =
    do
    str <- getLine
    let res = traverseEither readInt (words str)
    case res of
        Left msg -> pure (Left msg)
        Right nums ->
            let nums' = nums ++ [sum nums]
                line  = unwords (map show nums')
                cols  = addLists' acc nums'
            in
                putStrLn line >> pure (Right cols)



addLists :: [Int] -> [Int] -> [Int]
addLists = zipWith (+)

addLists' :: [Int] -> [Int] -> [Int]
addLists' [] ys = zipWith (+) (take (length ys) $ repeat 0) ys
addLists' xs ys = zipWith (+) xs ys

readInt :: String -> Result Int
readInt = readEither
