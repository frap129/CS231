import System.Environment
import System.Exit
import System.IO
import Data.Function
import Data.List
import Data.Char
import qualified Data.Map as Map

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

userView :: String -> String -> String -> String
userView _ _ [] = []
userView letter (x:xs:xss) (y:ys)
    | letter!!0 == y = (letter ++ " " ++ userView letter xss ys)
    | otherwise      = (x : xs : userView letter xss ys)

wordView :: String -> String -> String
wordView letter word = map (\x -> if elem x letter then x else '_') word

insertFamily ::String -> String -> Map.Map String [String] -> Map.Map String [String]
insertFamily guessed word = Map.insertWith (++) (wordView guessed word) [word]

wordFamilies :: String -> [String] -> Map.Map String [String]
wordFamilies guessed words = foldr (insertFamily guessed) Map.empty words

largestFamily :: String -> [String] -> [String]
largestFamily guessed words = head $ map snd $ sortByLength $ Map.toList $ wordFamilies guessed words

checkGuess :: String -> [String] -> Bool
checkGuess guess availWords
    | elem (guess!!0) (availWords!!0) = True
    | otherwise                       = False

sortByLength :: [(String, [String])] -> [(String, [String])]
sortByLength a = sortBy (compare `on` length) a

rightLenWords :: [String] -> Int -> [String]
rightLenWords a len = [x | x <- a, length x == len]

gameLoop :: [String] -> String -> Int -> IO ()
gameLoop _ _ 0 = do putStrLn "Sorry, you loose."
gameLoop availWords userWord guesses
    | not $ elem '_' userWord = do putStrLn "You win!"
    | otherwise               = do
        putStrLn "Input a guess:"
        guess <- getLine
        let newWords = largestFamily guess availWords
        let newUserWord = userView guess userWord $ head newWords
        let newGuesses = if checkGuess guess newWords then guesses else guesses - 1

        if checkGuess guess newWords then do
            putStrLn $"Correct! Word: " ++ userView guess userWord (head newWords)
            putStrLn $ "Guesses remaining: " ++ show guesses
        else do
            let newGuesses = guesses - 1
            putStrLn $ "Incorrect! Word: " ++ userWord
            putStrLn $ "Guesses remaining: " ++ show (guesses - 1)
            
        gameLoop newWords newUserWord newGuesses

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

    gameLoop dictWords (concat $replicate wordLen "_ ") numGuess
    return ()