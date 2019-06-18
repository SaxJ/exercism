module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0 = Nothing
    | otherwise = Just $ toInteger . length . (takeWhile (/= 1)) $ iterate coll n
        where
            coll x = if even x then x `div` 2 else 3 * x + 1
