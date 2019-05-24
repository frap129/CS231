-- Written by Joseph Maples for CS231
import System.Environment
import System.Exit
import System.IO
import Data.Function
import Data.List
import Data.Char
import Data.Map hiding (map, foldr)

-- Family is a tuple of the patttern and a list of matching words
type Family = (String, [String])

-- printUsage prints the program usage with the correct name
printUsage :: IO a
printUsage = do
    name <- getProgName
    die ("Usage: ./" ++ name ++ " dictionary_path word_len num_guesses")

{-
    splitArgs accepts a list of strings (the passed arguments) and
    the size of the list. If there are 4 args, it returns a tuple
    of 4 values, evaluating each to the correct type. If there are
    3 args, the 4th vale in the tuple is always false.
-}
splitArgs :: [String] -> Int -> (String, Int, Int, Bool)
splitArgs a numArgs
    | numArgs == 4 = (a!!0, (read $ a!!1), (read $ a!!2), (a!!3 == "-s"))
    | otherwise    = (a!!0, (read $ a!!1), (read $ a!!2), False)

{-
    getPattern accepts a Char (the guessed letter), and 2 strings.
    The first string is the previous pattern (can be all underscores
    and spaces) and the second string is the actual word to form a
    pattern from. It returns a pattern string containing the letters
    guessed so far.
-}
getPattern :: Char -> String -> String -> String
getPattern _ _ [] = []
getPattern letter (x:xs:xss) (y:ys)
    | letter == y = letter : ' ' : getPattern letter xss ys
    | otherwise   = x : xs : getPattern letter xss ys

{-
    wordFamilies accepts a Char (the guessed letter), as list of
    strings (the available words), a single string (the old pattern)
    and returns a list of Family, it works by getting the pattern for
    each word, and inserting them into a map as a key-value pair where
    the key is the pattern and the value is the words. By folding this
    across the list of words, we get a map containing pattern keys and
    matching word lists as the value. This is converted to a list of
    Family and returned
-}
wordFamilies :: Char -> [String] -> String -> [Family]
wordFamilies guess words prevPattern = toList $ foldr (\x -> insertWith (++) (getPattern guess prevPattern x) [x]) empty words

{-
    largestFamily accepts a Char (the guessed letter), a list of
    strings (the available words), a single string (the old pattern)
    and returns a the largest family of words with the guessed letter.
    It uses wordFamilies to get all word families, then sorts them by
    length in decending order, and returns the head of the list which
    should be the largest family.
-}
largestFamily :: Char -> [String] -> String -> Family
largestFamily guessed words pattern = head $ sortByLength $ wordFamilies guessed words pattern

-- sortByLength sorts a list by length, in decending order
sortByLength :: [Family] -> [Family]
sortByLength a = reverse $ sortBy (compare `on` length) a

-- rightLenWords removes elements of a list that aren't the given length
rightLenWords :: [String] -> Int -> [String]
rightLenWords a len = [x | x <- a, length x == len]

{-
    updateGuesses is the same as get pattern, but uses as single
    space as a blank rather than an underscore and a space. It
    returns a correctly formated string of previously guessed
    letters.
-}
updateGuesses :: Char -> String -> String -> String
updateGuesses _ _ [] = []
updateGuesses guess (x:xs) (y:ys)
    | y == guess = y : xs
    | otherwise  = x : updateGuesses guess xs ys

{-
    askGuess accepts a string (the previous guessed letters)
    and returns a new guess. It first asks the user to enter
    a guess, then reads the guess. The guess is then verified,
    first by length to ensure that a guess was entered. If this
    test passes, the function checks if it was already guessed.
    If that test passes, the fuction tests if the guess is an
    alphabetic character. If this test passes, the guess is
    returned. Should any of these tests fail, the function
    makes a recursive call to itself to ask for another guess.
-}
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

{-
    gameLoop is the function that executes the game logic. It
    accepts a list of strings (the available words), a string
    (the pattern of the remaining words), an int (number of wrong
    guesses remaining) and a boolean (control debug mode). The
    losing condition is executed if the number of wrong guesses
    remaining is 0. The wining condition is executed if the
    pattern no longer contains underscores. If neither of these
    conditions are met, the main game logic executes. First, the
    user is asked to enter a guess. The function uses the guess
    to update the list of words and the word pattern. If the
    guessed letter is an element of a word in the remaining
    family, the guess is correct. If the guess is correct, the
    program prints correct. If it is incorrect, the program
    prints incorrect and decrements the remaining guesses.
    The next step is to print game state info including the
    guesses remaining, the letters guessed so far, and the
    current word pattern. If debuging is enabled, the number of
    words remaining is also printed. Finally, the game makes a
    recursive call to itself with the new game data as the
    parameters. This continues until either the win or lose
    conditions are met.
-}
gameLoop :: [String] -> String -> String -> Int -> Bool -> IO ()
gameLoop availWords _ _ 0 _ = do putStrLn $ "Sorry, the word was " ++ head availWords
gameLoop availWords pattern guesses numGuess debug
    | not $ elem '_' pattern = do putStrLn "You win!"
    | otherwise              = do
        guess <- askGuess guesses
        let (newPattern, newWords) = largestFamily guess availWords pattern
        let newNumGuess = if elem guess (newWords!!0) then numGuess else numGuess - 1
        let prefix = if elem guess (newWords!!0) then "Correct! " else "Incorrect! "
        let newGuesses = updateGuesses guess guesses "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

        putStrLn $ prefix ++ "Guesses remaining: " ++ show newNumGuess
        putStrLn $ "Letters guessed: " ++ newGuesses
        putStrLn $ "Word so far: " ++ newPattern
        if debug then do putStrLn $ "Words in family: " ++ show (length newWords)
        else return ()
        gameLoop newWords newPattern newGuesses newNumGuess debug

{-
    main is the controlling function of the program. Its
    purpose is to verify arguments and prepare initial
    game data before starting the game loop. When
    verifying the arguments, it first checks that the
    length is either 3 or 4. If this passes, main checks
    if arguments 3 and 4 are numeric digits. Should
    either of these cases fail, the program prints its
    usage and exits. Next, splitArgs is called to return
    correctly formated arguments. If numGuesses is
    between 5 and 10 inclusive, the program continues.
    Otherwise, the program tells the user that they can
    only have between 5 and 10 guesses. Next, the
    provided dictuonary file is read, split into words,
    has all incorrect length words removed, and changed
    to upper case. If the dictionary is empty, this means
    that 0 correct length words exist, and the user is
    informed of this before exiting. Finally, the game
    loop is started.
-}
main = do
    args <- getArgs
    let numArgs = length args
    if numArgs == 3 || numArgs == 4 then
        if and $ map isDigit [(args!!1!!0), (args!!2!!0)] then
            return ()
        else
            printUsage
    else
        printUsage

    let (dictName, wordLen, numGuess, debug) = splitArgs args numArgs

    if numGuess > 4 && numGuess < 11 then
        return ()
    else
        putStrLn "Number of guesses must be between 5 and 10."

    dictContent <- readFile dictName
    let dictWords = map (map toUpper) $ rightLenWords (lines dictContent) wordLen

    if dictWords == [] then do
        die "The dictionary doesn\'t have any words of that length"
    else
        return ()

    gameLoop dictWords (concat $ replicate wordLen "_ ") (replicate 26 ' ') numGuess debug
    return ()
