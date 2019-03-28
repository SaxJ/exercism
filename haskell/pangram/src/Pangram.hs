module Pangram (isPangram) where

import Data.Char
import qualified Data.HashSet as HS

{-
 - This is a better implementation, since it will stop iterating over the string as soon as it contains
 - all letters of the alphabet.
 -}
isPangram :: String -> Bool
isPangram text = loop HS.empty text
    where
        loop hs [] = HS.size hs == 26
        loop hs (x:xs) = if HS.size hs == 26 then True else loop hs' xs
            where
                hs' = if isLetter x then (flip HS.insert) hs $ toLower x else hs

{-
isPangram :: String -> Bool
isPangram text = HS.size letterSet == 26
    where
        filtered = map toLower $ filter isLetter text
        letterSet = foldr HS.insert HS.empty filtered
-}
