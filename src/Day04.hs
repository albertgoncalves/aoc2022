import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    skipSpaces,
  )

type Range = (Int, Int)

type Pair = (Range, Range)

int :: ReadP Int
int = read <$> munch1 isDigit

range :: ReadP Range
range = (,) <$> int <*> (char '-' *> int)

pair :: ReadP Pair
pair = (,) <$> range <*> (char ',' *> range)

parse :: String -> [Pair]
parse = fst . head . readP_to_S (many1 (pair <* skipSpaces) <* eof)

part1 :: Range -> Range -> Bool
part1 (l0, r0) (l1, r1) = (l0 <= l1 && r1 <= r0) || (l1 <= l0 && r0 <= r1)

part2 :: Range -> Range -> Bool
part2 (l0, r0) (l1, r1) = (l0 <= r1 && l1 <= r0) || (l1 <= r0 && l0 <= r1)

main :: IO ()
main =
  interact $
    unlines
      . map (show . length)
      . zipWith (filter . uncurry) [part1, part2]
      . replicate 2
      . parse
