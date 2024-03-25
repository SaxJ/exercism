module RunLength (decode, encode) where

import Data.List (group, groupBy)
import Data.Char (isDigit)
import Text.Read (readMaybe)

decode :: String -> String
decode =  snd . foldl builder (Nothing, "") . groupBy digitMatch
  where
    digitMatch a b = isDigit a && isDigit b
    builder (num, acc) s = let
      currentTerm = readMaybe s
      in
        case currentTerm of
          Just _ -> (currentTerm, acc)
          Nothing -> case num of
                      Nothing -> (Nothing, acc ++ s)
                      Just x -> (Nothing, acc ++ concat (replicate x s))

encode :: String -> String
encode = concatMap codeRun . group
  where
    codeRun s = if length s == 1 then [head s] else show (length s) ++ [head s]
