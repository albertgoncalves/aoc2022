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
    <$> satisfy (\x -> isDigit x || x == '-')
    <*> munch isDigit

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

distance :: Pos -> Pos -> Int
distance (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

rows :: Int -> Pos -> Pos -> [Row]
rows n sensor@(x0, y0) beacon =
  [ ([(x0 - s, x0 + s)], y1)
    | k <- [-d .. d],
      let y1 = y0 + k,
      y1 == n,
      let s = d - abs k
  ]
  where
    d = distance sensor beacon

merge :: Span -> [Span] -> [Span]
merge a [] = [a]
merge a@(l0, r0) (b@(l1, r1) : bs)
  | l1 < l0 = merge b $ a : bs
  | (l0 <= r1) && (l1 <= r0) = merge (l0, max r0 r1) bs
  | succ r0 == l1 = merge (l0, r1) bs
  | otherwise = a : merge b bs

part1 :: Int -> [Pair] -> Int
part1 n =
  sum
    . map (\(l, r) -> r - l)
    . foldr merge []
    . flip (M.!) n
    . M.fromListWith (++)
    . map swap
    . concatMap (uncurry $ rows n)

pointRadius :: Pos -> Pos -> (Pos, Int)
pointRadius sensor beacon = (sensor, distance sensor beacon)

neighbors :: Pos -> Int -> [Pos]
neighbors (x, y) radius =
  concat
    [ [(succ $ x + j, y + i) | j <- rs, let i = radius - j],
      [(x + j, succ $ y + i) | i <- rs, let j = radius - i],
      [(x - succ j, y + i) | j <- rs, let i = radius - j],
      [(x + j, y - succ i) | i <- rs, let j = radius - i]
    ]
  where
    rs = [0 .. radius]

withinRange :: Pos -> Pos -> Int -> Bool
withinRange candidate center = (distance center candidate <=)

withinBounds :: Int -> Pos -> Bool
withinBounds n (x, y) = (0 <= x) && (0 <= y) && (x <= n) && (y <= n)

part2 :: Int -> [Pair] -> Int
part2 n pairs =
  (\(x, y) -> (x * 4000000) + y)
    $ head
    $ filter
      (\candidate -> not $ any (uncurry (withinRange candidate)) sensors)
    $ filter (withinBounds $ n * 2)
    $ concatMap (uncurry neighbors) sensors
  where
    sensors = map (uncurry pointRadius) pairs

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
