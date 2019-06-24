module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear n
    | n `mod` 4 == 0 = testException n
    | otherwise = False
    where
        testException x
            | x `mod` 100 /= 0 = True
            | otherwise = x `mod` 400 == 0
