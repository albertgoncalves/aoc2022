{-# LANGUAGE LambdaCase #-}

import Data.Char (ord)
import Data.List (intersect, nub, sort, unfoldr)

priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = 1 + ord c - ord 'a'
  | 'A' <= c && c <= 'Z' = 27 + ord c - ord 'A'
  | otherwise = undefined

part1 :: String -> Int
part1 xs = sum $ map priority $ nub $ uncurry intersect $ splitAt n xs
  where
    n = length xs `div` 2

part2 :: [String] -> Int
part2 =
  sum
    . map (sum . map priority . nub . sort . foldr1 intersect)
    . unfoldr
      ( \case
          [] -> Nothing
          xs -> Just $ splitAt 3 xs
      )

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith ($) [sum . map part1, part2]
      . replicate 2
      . lines
