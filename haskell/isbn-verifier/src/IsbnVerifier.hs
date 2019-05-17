module IsbnVerifier (isbn) where

import Data.Char

validChar :: Char -> Bool
validChar c = (c == 'X') || isNumber c

toIntList :: String -> [Int]
toIntList = map conv
    where
        conv c = if isNumber c then digitToInt c else 10

stringValid :: String -> Bool
stringValid xs = length xs == 10 && notElem 'X' (take 9 xs)

isValid :: String -> Bool
isValid cs = foldl (\c (i,n) -> c + i * n) 0 (zip [10,9..1] $ toIntList cs) `mod` 11 == 0

isbn :: String -> Bool
isbn s = stringValid filtered && isValid filtered
    where
        filtered = filter validChar s
