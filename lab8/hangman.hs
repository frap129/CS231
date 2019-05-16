import System.Environment
import System.Exit
import System.IO

printUsage = do
    name <- getProgName
    die ("Usage: ./" ++ name ++ " dictionary_path word_len num_guesses")

handleArgs :: [String] -> Int
handleArgs [_, _, _] = 3
handleArgs [_, _, _, s]
    | s == "-s" = 4
    | otherwise = 3
handleArgs _ = 0

main = do
    args <- getArgs
    let numArgs = handleArgs args
    if (numArgs == 0) then do
        printUsage
    else
        print numArgs -- Temporary