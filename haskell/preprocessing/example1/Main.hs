-- executables: the module name *must* be "Main" (!)

module Main where


main :: IO ()
-- main = putStrLn "Hello Mars!"
-- main = echo
main = echoQuit


echo :: IO ()
echo = getLine >>= putStrLn


echoServer :: IO ()
echoServer = getLine >>= (\s -> putStrLn s >>= (\_ -> echoServer) )


echoServer2 :: IO ()
echoServer2 = (getLine >>= putStrLn) >>= (\_ -> echoServer2)


echoServer3 :: IO ()
echoServer3 = (getLine >>= putStrLn) >> echoServer3


echoServer4 :: IO ()
echoServer4 = echo >> echoServer4


echoQuit :: IO ()
echoQuit = getLine >>= (\s ->
    if s == "quit" then
        pure ()
    else
        putStrLn s >>= (\_ -> echoQuit)
    )


{-
 (>>=) :: IO a -> (a -> IO b) -> IO b
-}

(>>>) :: Monad m => m a -> m b -> m b
(>>>) a b = a >>= (\_ -> b)
