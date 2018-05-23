module HW3.Solution where

import System.IO (isEOF)
import System.Exit (die)
import Text.Read (readEither)


type Result a = Either ErrorMsg a
type ErrorMsg = String


main :: IO ()
main = do
    res <- processStdin (repeat 0)
    case res of
        Left msg -> die msg
        Right colSums ->
            let line = unwords (map show colSums)
            in putStrLn line


processStdin :: [Int] -> IO (Result [Int])
processStdin colSums =
    do
    eof <- isEOF
    if eof then
        pure (Right colSums)
    else
        do
        res <- processLine
        case res of
            Left msg -> pure (Left msg)
            Right nums ->
                processStdin $ zipWith (+) colSums nums


processLine :: IO (Result [Int])
processLine =
    do
    str <- getLine
    let res = traverseEither readInt (words str)
    case res of
        Left msg -> pure (Left msg)
        Right nums ->
            let nums' = nums ++ [sum nums]
                line  = unwords (map show nums')
            in
                putStrLn line >> pure (Right nums')


----------------------------------------------------------------
----------------------------------------------------------------


traverseEither :: (a -> Either e b) -> [a] -> Either e [b]
traverseEither f as =
    case as of
        [] -> Right []
        a:rest ->
            case f a of
                Left e -> Left e
                Right b ->
                    case traverseEither f rest of
                        Left e' -> Left e'
                        Right bs -> Right (b:bs)


traverseEither' :: (a -> Either e b) -> [a] -> Either e [b]
traverseEither' f as = sequenceEither (map f as)


sequenceEither :: [Either e a] -> Either e [a]
sequenceEither xs =
    case xs of
        [] -> Right []
        x:rest ->
            case x of
                Left e -> Left e
                Right a ->
                    case sequenceEither rest of
                        Left e' -> Left e'
                        Right as -> Right (a:as)


sequenceEither' :: [Either e a] -> Either e [a]
sequenceEither' xs = traverseEither id xs


readInt :: String -> Result Int
readInt = readEither
