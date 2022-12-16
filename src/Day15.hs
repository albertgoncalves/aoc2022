import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
  ( ReadP,
    eof,
    many1,
    munch,
    readP_to_S,
    satisfy,
    skipSpaces,
    string,
  )

type Pos = (Int, Int)

type Pair = (Pos, Pos)

type Span = (Int, Int)

type Row = ([Span], Int)

int :: ReadP Int
int =
  (read .) . (:)
    <$> satisfy (\x -> isDigit x || x == '-') <*> munch isDigit

line :: ReadP Pair
line = do
  _ <- string "Sensor at x="
  x0 <- int
  _ <- string ", y="
  y0 <- int
  _ <- string ": closest beacon is at x="
  x1 <- int
  _ <- string ", y="
  y1 <- int
  return ((x0, y0), (x1, y1))

parse :: ReadP (Int, [Pair])
parse = (,) <$> (int <* skipSpaces) <*> (many1 (line <* skipSpaces) <* eof)

rows :: Int -> Pos -> Pos -> [Row]
rows n (x0, y0) (x1, y1) =
  [ ([(x0 - (d - abs k), x0 + (d - abs k))], y0 + k)
    | k <- [-d .. d],
      (y0 + k) == n
  ]
  where
    d = abs (x1 - x0) + abs (y1 - y0)

merge :: Span -> [Span] -> [Span]
merge a [] = [a]
merge a@(l0, r0) (b@(l1, r1) : bs)
  | l1 < l0 = merge b $ a : bs
  | (l0 <= r1) && (l1 <= r0) = merge (l0, max r0 r1) bs
  | otherwise = a : b : bs

part1 :: Int -> [Pair] -> Int
part1 n =
  sum
    . map (\(l, r) -> r - l)
    . foldr merge []
    . flip (M.!) n
    . M.fromListWith (++)
    . map swap
    . concatMap (uncurry (rows n))

part2 :: Int -> [Pair] -> Int
part2 = undefined

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith uncurry [part1, part2]
      . repeat
      . fst
      . head
      . readP_to_S parse
