import Data.Char (isDigit)
import Data.List (elemIndex, foldl')
import Data.Map ((!))
import qualified Data.Map as M
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

type Cell = (Int, Int)

type Neighbors = (Cell, Cell)

int :: ReadP Int
int = (read .) . (:) <$> (satisfy isDigit <++ char '-') <*> munch isDigit

swapRight :: Cell -> M.Map Cell Neighbors -> M.Map Cell Neighbors
swapRight b m
  | (b /= aR) || (aR /= cL) || (bR /= dL) = undefined
  | otherwise =
      foldr
        (uncurry M.insert)
        m
        [ (bL, (aL, bR)),
          (b, (bR, cR)),
          (bR, (bL, cL)),
          (cR, (cL, dR))
        ]
  where
    (aL, aR) = m ! bL
    (bL, bR) = m ! b
    (cL, cR) = m ! bR
    (dL, dR) = m ! cR

swapLeft :: Cell -> M.Map Cell Neighbors -> M.Map Cell Neighbors
swapLeft c m
  | (c /= bR) || (aR /= cL) || (bR /= dL) = undefined
  | otherwise =
      foldr
        (uncurry M.insert)
        m
        [ (bL, (aL, bR)),
          (cL, (bR, cR)),
          (c, (bL, cL)),
          (cR, (cL, dR))
        ]
  where
    (aL, aR) = m ! bL
    (bL, bR) = m ! cL
    (cL, cR) = m ! c
    (dL, dR) = m ! cR

for :: Int -> (a -> b -> b) -> a -> b -> b
for n f a b = iterate (f a) b !! n

swap :: Int -> Cell -> M.Map Cell Neighbors -> M.Map Cell Neighbors
swap l x@(_, k)
  | (l `div` 2) <= n = for (l - n) swapLeft x
  | otherwise = for n swapRight x
  where
    n = k `mod` l

unpack :: Cell -> M.Map Cell Neighbors -> [Cell]
unpack a m = a : unpack b m
  where
    (_, b) = m ! a

mix :: Int -> [Cell] -> M.Map Cell Neighbors -> M.Map Cell Neighbors
mix l xs0 m = foldl' (flip $ swap l) m xs0

intoMap :: Int -> [Cell] -> M.Map Cell Neighbors
intoMap l xs0 = M.fromList $ take l $ zip xs2 $ zip xs1 $ tail xs2
  where
    xs1 = cycle xs0
    xs2 = tail xs1

part1 :: [Cell] -> (Int, [Cell])
part1 xs = (l, unpack (head xs) $ mix (pred l) xs $ intoMap l xs)
  where
    l = length xs

part2 :: [Cell] -> (Int, [Cell])
part2 xs0 =
  (l, unpack (head xs1) $ iterate (mix (pred l) xs1) (intoMap l xs1) !! 10)
  where
    l = length xs0
    xs1 = map ((811589153 *) <$>) xs0

extract :: Int -> [Int] -> [Int]
extract l xs = map (\k -> xs !! ((n + k) `mod` l)) [1000, 2000, 3000]
  where
    n = fromJust $ elemIndex 0 xs

main :: IO ()
main =
  interact $
    unlines
      . map (show . sum . (\(l, xs) -> extract l $ map snd $ take l xs))
      . zipWith ($) [part1, part2]
      . repeat
      . zip [0 ..]
      . fst
      . head
      . readP_to_S (many1 (int <* skipSpaces) <* eof)
