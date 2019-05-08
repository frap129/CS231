module HoldMeeting where

checkTimes :: [Int] -> Int -> Int
checkTimes list length
    | length == 2                     = 0
    | list!!(length - 1) <= list!!1   = 1 + checkTimes list (length - 1)
    | otherwise                       = checkTimes list (length - 1)

holdMeeting :: String -> (Int, Bool)
holdMeeting input =
    (onTime, onTime >= head intList) where
    intList = map read (words input)
    onTime = checkTimes intList (length intList)