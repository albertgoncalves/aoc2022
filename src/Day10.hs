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

run :: Int -> Int -> Inst -> (Int, Int)
run cycles register InstNoop = (cycles + 1, register)
run cycles register (InstAdd x) = (cycles + 2, register + x)

tally :: Int -> [(Int, Int)] -> [Int]
tally n ((_, r) : xs@((c, _) : _))
  | 40 <= c - n = (r * m) : tally m xs
  | otherwise = tally n xs
  where
    m = n + 40
tally _ _ = []

main :: IO ()
main =
  interact $
    show
      . sum
      . tally (-20)
      . scanl (uncurry run) (0, 1)
      . fst
      . head
      . readP_to_S (many1 (inst <* char '\n') <* eof)
