module Hamming (distance) where


distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just $ length $ [a | (a,b) <- zp, a /= b]
        where
            zp = zip xs ys
