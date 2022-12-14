import Data.Bifunctor (first, second)
import Data.List (nub, sort)

data Dir
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Show)

type Pos = (Int, Int)

type Rope = [Pos]

parse :: [String] -> [Dir]
parse ["U", x] = replicate (read x) DirUp
parse ["D", x] = replicate (read x) DirDown
parse ["L", x] = replicate (read x) DirLeft
parse ["R", x] = replicate (read x) DirRight
parse _ = undefined

move :: Dir -> Pos -> Pos
move DirUp = second (+ 1)
move DirDown = second $ subtract 1
move DirLeft = first $ subtract 1
move DirRight = first (+ 1)

follow :: Pos -> Pos -> Pos
follow (x0, y0) (x1, y1)
  | (abs (x0 - x1) <= 1) && (abs (y0 - y1) <= 1) = (x1, y1)
  | otherwise = (x2, y2)
  where
    x2
      | x0 < x1 = x1 - 1
      | x1 < x0 = x1 + 1
      | otherwise = x1
    y2
      | y0 < y1 = y1 - 1
      | y1 < y0 = y1 + 1
      | otherwise = y1

sim :: Rope -> Dir -> Rope
sim (x : xs) dir = scanl follow (move dir x) xs
sim _ _ = undefined

solve :: Int -> [Dir] -> String
solve n =
  show . length . nub . sort . map last . scanl sim (replicate n (0, 0))

main :: IO ()
main =
  interact $
    unlines
      . zipWith solve [2, 10]
      . repeat
      . concatMap (parse . words)
      . lines
