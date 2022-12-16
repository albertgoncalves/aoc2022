{-# LANGUAGE LambdaCase #-}

import Data.Char (isSpace)
import Data.List (unfoldr)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    string,
    (<++),
  )
import Prelude hiding (cycle)

data Inst
  = InstNoop
  | InstAdd Int

inst :: ReadP Inst
inst =
  (InstNoop <$ string "noop")
    <++ (InstAdd . read <$> (string "addx " *> munch1 (not . isSpace)))

run :: Int -> [Inst] -> [Int]
run _ [] = []
run register (InstNoop : insts) = register : run register insts
run register (InstAdd x : insts) =
  register : register : run (register + x) insts

part1 :: Int -> [(Int, Int)] -> [Int]
part1 n ((_, r) : xs@((c, _) : _))
  | 40 <= c - n = (r * m) : part1 m xs
  | otherwise = part1 n xs
  where
    m = n + 40
part1 _ _ = []

part2 :: Int -> Int -> Char
part2 cycle register
  | ((register - 1) <= c) && (c <= (register + 1)) = '#'
  | otherwise = ' '
  where
    c = cycle `mod` 40

chunks :: Int -> [a] -> [[a]]
chunks n =
  unfoldr $
    \case
      [] -> Nothing
      xs -> Just $ splitAt n xs

main :: IO ()
main =
  interact $
    unlines
      . zipWith
        ($)
        [ show . sum . part1 (-20),
          unlines . chunks 40 . map (uncurry part2)
        ]
      . repeat
      . zip [0 ..]
      . run 1
      . fst
      . head
      . readP_to_S (many1 (inst <* char '\n') <* eof)
