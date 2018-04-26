module HW2.Solution where

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
    do
    eof <- isEOF
    if eof then
        pure (Right ())
    else
        do
        res <- processLine
        case res of
            Left msg -> pure (Left msg)
            Right _  -> processStdin


processLine :: IO (Result ())
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
                putStrLn line >> pure (Right ())


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
