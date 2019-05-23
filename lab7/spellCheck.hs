-- Written by Joseph Maples for CS231
import Data.List
import Data.Char
import System.Environment
import System.Exit
import System.IO

{-
    splitWords accepts a string parameter and
    returns a list of strings. The string is
    split at any non-alphabetic character.
    The case expression uses dropWhile to drop
    all preceeding non-alpha chars. The main
    spliting case splits the words by traversing
    the string and using break to stop at any
    non-alpha char. dropWhile removes non-alpha
    chars, and the process repeats until the
    string is empty.
-}
splitWords :: String -> [String]
splitWords x =  case dropWhile (not . isAlpha) x of
  "" -> []
  xs -> word : splitWords xss
    where (word, xss) =
           break (not . isAlpha) xs

{-
    insensitveCompare accepts two strings,
    changes them to uppercase, and passes them
    to the native compare function. It returns
    and Ordering value: LT, EQ, or GT. 
-}
insensitiveCompare :: String -> String -> Ordering
insensitiveCompare a b = compare (map toUpper a) (map toUpper b)

{-
    insensitveEqual accepts two strings,
    passes them to insensitveCompare, and
    converts the Ordering value to a boolean
    by checking if the value is equal to EQ.
-}
insensitiveEqual :: String -> String -> Bool
insensitiveEqual a b = insensitiveCompare a b == EQ

{-
    insensitveSort simply wraps the sortBy
    function such that it uses insensitiveCompare.
-}
insensitiveSort :: [String] -> [String]
insensitiveSort = sortBy insensitiveCompare

{-
    insensitveGroup wraps the groupBy
    function such that it uses insensitiveEqual.
-}
insensitiveGroup :: [String] -> [[String]]
insensitiveGroup = groupBy insensitiveEqual

{-
    insensitiveUniq chains the output of
    insensitveSort to insensitivieGroup,
    and chains the output of that to map
    head. The logic is that sort will
    place duplicates next to each other,
    group with join the duplicates into
    a sublist, and map head will take the
    first element of each sublist and
    discard the rest, removing any
    duplicated words.
-}
insensitiveUniq :: [String] -> [String]
insensitiveUniq = map head . insensitiveGroup . insensitiveSort

{-
    spellCheck accepts two lists of strings as
    parameters, and returns a list of strings.
    Words to be checked are changed to lowercase
    to avoid case issues, and elem is used to
    deterimine if each item in the first list
    exists in the second. The base case exits
    when the first list becomes empty,
    signifying that all words have been checked.
-}
spellCheck :: [String] -> [String] -> [String]
spellCheck [] _ = []
spellCheck (word:words) dict
  | elem (map toLower word) dict = ("Correct: " ++ word) : spellCheck words dict
  | otherwise = ("Incorrect: " ++ word) : spellCheck words dict

{-
    main is the controlling function of the program.
    First, main verifies it was given 3 arguments.
    Then, main reads in these arguments: the input
    file path, the dictionary file path, and the
    output file path. Next, it loads the contents of
    the input and dictionary files. Both files are
    split into a list by words, and the input list
    is sorted and has duplicates removed. Finally,
    the input list is spell checked and the results
    are written to the outputfile.
-}
main = do
  args <- getArgs
  if length args /= 3 then
    die ("Wrong number of arguments supplied")
  else
    return ()
  let [inFile, dictFile, outFile] = args
  inContent <- readFile inFile
  dictContent <- readFile dictFile
  let inWords = insensitiveUniq $ splitWords inContent
  let dictWords = map (map toLower) $ splitWords dictContent
  outFileHandle <- openFile outFile WriteMode
  mapM (hPutStrLn outFileHandle) $ spellCheck inWords dictWords
  hClose outFileHandle