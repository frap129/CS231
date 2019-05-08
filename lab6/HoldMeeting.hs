-- Written by Joseph Maples for CS231
module HoldMeeting where

{-
    checkTimes accepts 2 parameters:
    - The given list of integers
    - The number of elements that haven't been checked
    and returns the number of people who were on time.

    The given list of integers is used to compare
    arrival times to the time limit

    The number of unchecked elements is used to change
    the index of the element to be checked in each
    recursive call and is decremented until it is 2,
    which means only the required number of people and
    the time limit remain in the list
-}
checkTimes :: [Int] -> Int -> Int
checkTimes list length
    | length == 2                     = 0
    | list!!(length - 1) <= list!!1   = 1 + checkTimes list (length - 1)
    | otherwise                       = checkTimes list (length - 1)

{-
    holdMeeting accepts one parameter, the input
    string, and returns a tuple containing the
    number of on time people and a boolean stating
    whether the meeting should be held or not
-}
holdMeeting :: String -> (Int, Bool)
holdMeeting input =
    (onTime, onTime >= head intList) where
    intList = map read (words input)
    onTime = checkTimes intList (length intList)