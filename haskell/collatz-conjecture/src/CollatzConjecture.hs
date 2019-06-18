module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0 = Nothing
    | otherwise = Just $ toInteger . length . (takeWhile (/= 1)) $ iterate colz n
        where
            colz x
                | even x = x `div` 2
                | otherwise = 3 * x + 1
