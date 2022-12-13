import Data.Char (isSpace)
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

data Inst
  = InstNoop
  | InstAdd Int
  deriving (Show)

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

main :: IO ()
main =
  interact $
    show
      . sum
      . part1 (-20)
      . zip [0 ..]
      . run 1
      . fst
      . head
      . readP_to_S (many1 (inst <* char '\n') <* eof)
