module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Read, Enum)

value :: [Color] -> Int
value cs = foldl (\acc (a, b) -> acc + 10 ^ a * b) 0 $ zip [0..] $ reverse $ map fromEnum cs
