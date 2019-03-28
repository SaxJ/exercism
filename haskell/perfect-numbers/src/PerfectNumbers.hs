module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

classify :: Int -> Maybe Classification
classify x
    | x <= 0 = Nothing
    | s == x = Just Perfect
    | s < x = Just Deficient
    | otherwise = Just Abundant
        where
            s = sum $ factors x
