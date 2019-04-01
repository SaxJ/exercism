module Phone (number) where

import Data.Char

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)

validate :: String -> Maybe String
validate [] = Nothing
validate (x:xs)
    | length xs == 10 = if x /= '1' then Nothing else rules xs
    | otherwise = rules (x:xs)
    where
        rules ss = if ss !! 0 >= '2' && ss !! 3 >= '2' then Just ss else Nothing

number :: String -> Maybe String
number xs
    | length clean < 10 || length clean > 11 = Nothing
    | otherwise = validate clean
    where
        clean = filter isNumber xs
