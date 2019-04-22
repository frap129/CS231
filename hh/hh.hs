
hh::String -> String
hh name = "Hello " ++ name ++ "!"

main = do
    putStrLn "hh: Hello Haskell!"
    putStrLn "What is your name?"
    name <- getLine
    putStrLn(hh name)
