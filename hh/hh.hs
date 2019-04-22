main = do
    putStrLn "hh: Hello Haskell!"
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")
