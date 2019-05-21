import System.Environment
import System.Exit
import System.IO
import Data.Function
import Data.List
import Data.Char
import qualified Data.Map as Map

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

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

splitArgs :: [String] -> (String, Int, Int, Bool)
splitArgs [a, b, c, d] = (a, (read b), (read c), (d == "-s"))
splitArgs [a, b, c, d, e] = (a, (read b), (read c), (d == "-s"))

userView :: Char -> String -> String -> String
userView _ _ [] = []
userView letter (x:xs:xss) (y:ys)
    | (toUpper letter) == (toUpper y) = (toUpper letter) : ' ' : userView letter xss ys
    | otherwise                       = x : xs : userView letter xss ys

wordView :: Char -> String -> String
wordView letter word = map (\x -> if x == letter then x else '_') word

insertFamily :: Char -> String -> Map.Map String [String] -> Map.Map String [String]
insertFamily guessed word = Map.insertWith (++) (wordView guessed word) [word]

wordFamilies :: Char -> [String] -> Map.Map String [String]
wordFamilies guessed words = foldr (insertFamily guessed) Map.empty words

largestFamily :: Char -> [String] -> [String]
largestFamily guessed words = head $ map snd $ sortByLength $ Map.toList $ wordFamilies guessed words

checkGuess :: Char -> [String] -> Bool
checkGuess guess availWords
    | elem guess (availWords!!0) = True
    | otherwise                  = False

sortByLength :: [(String, [String])] -> [(String, [String])]
sortByLength a = sortBy (compare `on` length) a

rightLenWords :: [String] -> Int -> [String]
rightLenWords a len = [x | x <- a, length x == len]

hiddenString :: Int -> String
hiddenString len = (concat $ replicate len "_ ")

afterGuess :: String -> Int -> String -> String -> Bool -> Int -> IO ()
afterGuess prefix numGuess newGuesses userWord debug familySize = do
    putStrLn $ prefix ++ "Guesses remaining: " ++ show numGuess
    putStrLn $ "Letters guessed: " ++ newGuesses
    putStrLn $ "Word so far: " ++ userWord
    if debug then do putStrLn $ "Words in family: " ++ show familySize
    else return ()

gameLoop :: [String] -> String -> String -> Int -> Bool -> IO ()
gameLoop availWords _ _ 0 _ = do putStrLn $ "Sorry, the word was " ++ head availWords
gameLoop availWords userWord guesses numGuess debug
    | not $ elem '_' userWord = do putStrLn "You win!"
    | otherwise               = do
        putStrLn "Input a guess:"
        uncheckedGuess <- getLine

        let guess = uncheckedGuess!!0
        let newWords = largestFamily guess availWords
        let newUserWord = userView guess userWord $ head newWords
        let newNumGuess = if checkGuess guess newWords then numGuess else numGuess - 1
        let prefix = if checkGuess guess newWords then "Correct! " else "Incorrect! "
        let newGuesses = userView guess guesses alphabet

        afterGuess prefix newNumGuess newGuesses newUserWord debug (length newWords)
        gameLoop newWords newUserWord newGuesses newNumGuess debug

main = do
    args <- getArgs
    let numArgs = checkArgs args
    if numArgs == 0 then do
        printUsage
    else
        return ()

    let (dictName, wordLen, numGuess, debug) = splitArgs (args ++ ["no"])
    dictContent <- readFile dictName
    let dictWords = rightLenWords (lines dictContent) wordLen


    if dictWords == [] then do
        die "The dictionary doesn\'t have any words of that length"
    else
        return ()

    gameLoop dictWords (hiddenString wordLen) (hiddenString 26) numGuess debug
    return ()