import Data.Char (isDigit)
import Data.List (foldl', nub, sort)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
  )
import Prelude hiding (max, min)

type Pos = (Int, Int, Int)

int :: ReadP Int
int = read <$> munch1 isDigit

comma :: ReadP Char
comma = char ','

parse :: ReadP [Pos]
parse =
  many1 ((,,) <$> (int <* comma) <*> (int <* comma) <*> int <* char '\n')
    <* eof

intoNeighbors :: Pos -> [Pos]
intoNeighbors (x, y, z) =
  [ (pred x, y, z),
    (x, pred y, z),
    (x, y, pred z),
    (succ x, y, z),
    (x, succ y, z),
    (x, y, succ z)
  ]

check :: M.Map Pos Int -> Int -> [Pos] -> (Int, M.Map Pos Int)
check cubes sides [] = (sides, cubes)
check _ sides _
  | sides <= 0 = undefined
check cubes sides (neighbor : neighbors) =
  case M.lookup neighbor cubes of
    Nothing -> check cubes sides neighbors
    Just _ -> check (M.adjust pred neighbor cubes) (pred sides) neighbors

insert :: M.Map Pos Int -> Pos -> M.Map Pos Int
insert cubes0 cube
  | M.member cube cubes0 || sides < 0 = undefined
  | otherwise = M.insert cube sides cubes1
  where
    (sides, cubes1) = check cubes0 6 $ intoNeighbors cube

part1 :: M.Map Pos Int -> M.Map Pos Int
part1 = id

search :: S.Set Pos -> M.Map Pos Int -> Pos -> Pos -> [Pos] -> [Pos]
search
  visited
  cubes
  min@(xMin, yMin, zMin)
  max@(xMax, yMax, zMax)
  (probe@(x, y, z) : probes)
    | S.member probe visited
        || M.member probe cubes
        || x < xMin
        || y < yMin
        || z < zMin
        || xMax < x
        || yMax < y
        || zMax < z =
        search visited cubes min max probes
    | otherwise =
        probe
          : search
            (S.insert probe visited)
            cubes
            min
            max
            (probes ++ intoNeighbors probe)
search _ _ _ _ [] = []

minMax :: (Ord a, Ord b) => (a -> b) -> M.Map a c -> (b, b)
minMax f collection = (fst $ M.findMin items, fst $ M.findMax items)
  where
    items = M.mapKeys f collection

part2 :: M.Map Pos Int -> M.Map Pos Int
part2 cubes =
  foldl'
    insert
    M.empty
    $ S.difference
      ( S.fromList
          [ (x, y, z)
            | x <- [xMin .. xMax],
              y <- [yMin .. yMax],
              z <- [zMin .. zMax]
          ]
      )
    $ S.fromList
    $ search
      S.empty
      cubes
      (pred xMin, pred yMin, pred zMin)
      (succ xMax, succ yMax, succ zMax)
      [(pred xMin, pred yMin, pred zMin)]
  where
    (xMin, xMax) = minMax (\(x, _, _) -> x) cubes
    (yMin, yMax) = minMax (\(_, y, _) -> y) cubes
    (zMin, zMax) = minMax (\(_, _, z) -> z) cubes

main :: IO ()
main =
  interact $
    unlines
      . map (show . sum . M.elems)
      . zipWith ($) [part1, part2]
      . repeat
      . foldl' insert M.empty
      . nub
      . sort
      . fst
      . head
      . readP_to_S parse
