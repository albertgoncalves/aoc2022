{-# LANGUAGE TypeApplications #-}

import Data.Array (Array, bounds, listArray, (!))
import Data.List (nub, nubBy, sort, transpose)

intoArray :: [[Int]] -> Array (Int, Int) Int
intoArray xs = listArray ((0, 0), (x, y)) $ concat $ transpose xs
  where
    x = length (head xs) - 1
    y = length xs - 1

visible :: Array (Int, Int) Int -> [(Int, Int)] -> [(Int, Int)]
visible array path =
  map fst $
    nubBy (\a b -> snd a == snd b) $ f 0 $ zip path $ map (array !) path
  where
    f :: Int -> [(a, Int)] -> [(a, Int)]
    f _ [] = []
    f n (x@(_, m) : xs)
      | n <= m = x : f m xs
      | otherwise = f n xs

part1 :: Array (Int, Int) Int -> Int
part1 array = length $ nub $ sort $ concatMap (visible array) paths
  where
    (x, y) = snd $ bounds array
    l0 = [[(j, i) | j <- [0 .. x]] | i <- [0 .. y]]
    l1 = [[(j, i) | i <- [0 .. y]] | j <- [0 .. x]]
    paths = concat [l0, l1, map reverse l0, map reverse l1]

main :: IO ()
main =
  interact $
    show
      . part1
      . intoArray
      . map (map (read @Int . (: [])))
      . lines
