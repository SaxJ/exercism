module CollatzConjecture (collatz) where

collatzSequence :: Integer -> [Integer]
collatzSequence 1 = []
collatzSequence x = v : collatzSequence v
    where
        v = if even x then x `div` 2 else 3 * x + 1

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0 = Nothing
    | otherwise = Just $ toInteger . length $ collatzSequence n
