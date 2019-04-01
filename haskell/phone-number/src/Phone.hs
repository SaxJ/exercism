module Phone (number) where

import Data.Char

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)

number :: String -> Maybe String
number xs = 
