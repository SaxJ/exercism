module Acronym (abbreviate) where

import Data.Char
import Text.Casing

abbreviate :: String -> String
abbreviate xs = map (toUpper . head) $ concatMap (unIdentifier . fromAny) $ words xs
