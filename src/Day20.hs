{-# LANGUAGE Strict #-}

import Data.Char (isDigit)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch,
    readP_to_S,
    satisfy,
    skipSpaces,
    (<++),
  )

int :: ReadP Int
int = (read .) . (:) <$> (satisfy isDigit <++ char '-') <*> munch isDigit

push :: Int -> a -> [a] -> [a]
push 0 x xs = x : xs
push n a (b : bs) = b : push (pred n) a bs
push _ _ _ = undefined

mix :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
mix l n (x@(a, b) : xs)
  | l < n = xs ++ [x]
  | n /= a = mix l n $ xs ++ [x]
  | otherwise = mix l (succ n) $ push j x xs
  where
    j = b `mod` l
mix _ _ _ = undefined

extract :: [Int] -> [Int]
extract xs = map (\m -> xs !! ((n + m) `mod` l)) [1000, 2000, 3000]
  where
    n = fromJust $ elemIndex 0 xs
    l = length xs

part1 :: [(Int, Int)] -> [(Int, Int)]
part1 xs = mix (length xs - 1) 0 xs

part2 :: [(Int, Int)] -> [(Int, Int)]
part2 xs = iterate (mix l 0) (map ((811589153 *) <$>) xs) !! 10
  where
    l :: Int
    l = length xs - 1

main :: IO ()
main =
  interact $
    unlines
      . map (show . sum . extract . map snd)
      . zipWith ($) [part1, part2]
      . repeat
      . zip [0 ..]
      . fst
      . head
      . readP_to_S (many1 (int <* skipSpaces) <* eof)
