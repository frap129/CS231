module HoldMeeting where

checkTimes :: [Int] -> Int -> Int -> Int
checkTimes list length people
    | length == 2                     = people
    | list!!(length - 1) <= list!!1   = checkTimes list (length - 1) (people + 1)
    | otherwise                       = checkTimes list (length - 1) people

holdMeeting :: String -> (Int, Bool)
holdMeeting input =
    (onTime, onTime >= head intList) where
    intList = map read (words input)
    onTime = checkTimes intList (length intList) 0