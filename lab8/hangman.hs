import System.Environment
import System.Exit
import System.IO
import Data.Function
import Data.List
import Data.Char
import qualified Data.Map as Map

type Family = (String, [String])

printUsage :: IO a
printUsage = do
    name <- getProgName
    die ("Usage: ./" ++ name ++ " dictionary_path word_len num_guesses")

checkArgs :: [String] -> Int
checkArgs [_, _, _] = 3
checkArgs [_, _, _, s]
    | s == "-s" = 4
    | otherwise = 3
checkArgs _ = 0

splitArgs :: [String] -> Int -> (String, Int, Int, Bool)
splitArgs a numArgs
    | numArgs == 4 = (a!!0, (read $ a!!1), (read $ a!!2), (a!!3 == "-s"))
    | otherwise    = (a!!0, (read $ a!!1), (read $ a!!2), False)

getPattern :: Char -> String -> String -> String
getPattern _ _ [] = []
getPattern letter (x:xs:xss) (y:ys)
    | letter == y = letter : ' ' : getPattern letter xss ys
    | otherwise   = x : xs : getPattern letter xss ys

wordFamilies :: Char -> [String] -> String -> [Family]
wordFamilies guess words prevPattern = Map.toList $ foldr (\i -> Map.insertWith (++) (getPattern guess prevPattern i) [i]) Map.empty words

largestFamily :: Char -> [String] -> String -> Family
largestFamily guessed words pattern = head $ sortByLength $ wordFamilies guessed words pattern

checkGuess :: Char -> [String] -> Bool
checkGuess guess availWords
    | elem guess (availWords!!0) = True
    | otherwise                  = False

sortByLength :: [Family] -> [Family]
sortByLength a = reverse $ sortBy (compare `on` length) a

rightLenWords :: [String] -> Int -> [String]
rightLenWords a len = [x | x <- a, length x == len]

updateGuesses :: Char -> String -> String -> String
updateGuesses _ _ [] = []
updateGuesses guess (x:xs) (y:ys)
    | y == guess = y : xs
    | otherwise  = x : updateGuesses guess xs ys

askGuess :: String -> IO Char
askGuess prevGuesses = do
    putStrLn "Enter a guess:"
    anyCase <- getLine
    guess <-
        if length anyCase > 0 then
             return $ toUpper $ head anyCase
        else
            askGuess prevGuesses
    if elem guess prevGuesses then
        askGuess prevGuesses
    else if not $ isAlpha guess then
        askGuess prevGuesses
    else
        return guess

gameLoop :: [String] -> String -> String -> Int -> Bool -> IO ()
gameLoop availWords _ _ 0 _ = do putStrLn $ "Sorry, the word was " ++ head availWords
gameLoop availWords pattern guesses numGuess debug
    | not $ elem '_' pattern = do putStrLn "You win!"
    | otherwise              = do
        guess <- askGuess guesses
        let (newPattern, newWords) = largestFamily guess availWords pattern
        let newNumGuess = if checkGuess guess newWords then numGuess else numGuess - 1
        let prefix = if checkGuess guess newWords then "Correct! " else "Incorrect! "
        let newGuesses = updateGuesses guess guesses "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

        putStrLn $ prefix ++ "Guesses remaining: " ++ show newNumGuess
        putStrLn $ "Letters guessed: " ++ newGuesses
        putStrLn $ "Word so far: " ++ newPattern
        if debug then do putStrLn $ "Words in family: " ++ show (length newWords)
        else return ()
        gameLoop newWords newPattern newGuesses newNumGuess debug

main = do
    args <- getArgs
    let numArgs = checkArgs args
    if or [(numArgs == 3), (numArgs == 4)] then
        if and $ map isDigit [(args!!1!!0), (args!!2!!0)] then
            return ()
        else
            printUsage
    else
        printUsage

    let (dictName, wordLen, numGuess, debug) = splitArgs args numArgs
    dictContent <- readFile dictName
    let dictWords = map (map toUpper) $ rightLenWords (lines dictContent) wordLen

    if dictWords == [] then do
        die "The dictionary doesn\'t have any words of that length"
    else
        return ()

    gameLoop dictWords (concat $ replicate word_len "_ ") (replicate 26 ' ') numGuess debug
    return ()