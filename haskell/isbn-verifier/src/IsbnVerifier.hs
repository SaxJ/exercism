module IsbnVerifier (isbn) where

import Data.Char

validChar :: Char -> Bool
validChar c = (c == 'X') || isNumber c

toIntList :: String -> [Int]
toIntList = map conv
    where
        conv c
            | isNumber c = digitToInt c
            | otherwise = 10

xInTenth :: String -> Bool
xInTenth = (notElem 'X') . (take 9)

validSum :: String -> Bool
validSum cs = (sum $ zipWith (*) [10,9..1] $ toIntList cs) `mod` 11 == 0

isbn :: String -> Bool
isbn s = length validChars == 10 && xInTenth validChars && validSum validChars
    where
        validChars = filter validChar s
