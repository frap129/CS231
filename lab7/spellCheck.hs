import Data.List
import Data.Char
import System.Environment
import System.IO

isNotAlpha :: Char -> Bool
isNotAlpha = not . isAlpha

getWords :: String -> [String]
getWords string =  case dropWhile isNotAlpha string of
  "" -> [] -- Empty strings have no words
  string' -> word : getWords string''
    where (word, string'') =
           break isNotAlpha string'

insensitiveCompare :: String -> String -> Ordering
insensitiveCompare a b = compare (map toUpper a) (map toUpper b)

insensitiveEqual :: String -> String -> Bool
insensitiveEqual a b = insensitiveCompare a b == EQ

insensitiveSort :: [String] -> [String]
insensitiveSort = sortBy insensitiveCompare

insensitiveGroup :: [String] -> [[String]]
insensitiveGroup = groupBy insensitiveEqual

insensitiveUniq :: [String] -> [String]
insensitiveUniq = map head . insensitiveGroup . insensitiveSort

spellCheck :: [String] -> [String] -> [String]
spellCheck [] _ = [] -- Empty strings have no words
spellCheck (word:words) dict
    | elem (map toLower word) dict = ("Correct: " ++ word) : spellCheck words dict
    | otherwise = ("Incorrect: " ++ word) : spellCheck words dict

main = do
  [inFile, dictFile, outFile] <- getArgs
  inContent <- readFile inFile
  dictContent <- readFile dictFile
  let inWords = insensitiveUniq $ getWords inContent
  let dictWords = getWords dictContent
  outFileHandle <- openFile outFile WriteMode
  mapM_ (hPutStrLn outFileHandle) $ spellCheck inWords dictWords
  hClose outFileHandle
  
 