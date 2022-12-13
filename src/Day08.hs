{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.Array (Array, bounds, listArray, (!))
import Data.List (nub, nubBy, sort, transpose)

intoArray :: [[Int]] -> Array (Int, Int) Int
intoArray xs = listArray ((0, 0), (x, y)) $ concat $ transpose xs
  where
    x = length (head xs) - 1
    y = length xs - 1

part1 :: Array (Int, Int) Int -> Int
part1 array =
  length $
    nub $
      sort $
        concatMap visible $ concat [p0, p1, map reverse p0, map reverse p1]
  where
    (x, y) = snd $ bounds array
    p0 = [[(j, i) | j <- [0 .. x]] | i <- [0 .. y]]
    p1 = [[(j, i) | i <- [0 .. y]] | j <- [0 .. x]]

    visible :: [(Int, Int)] -> [(Int, Int)]
    visible = map fst . nubBy (\a b -> snd a == snd b) . f 0
      where
        f :: Int -> [(Int, Int)] -> [((Int, Int), Int)]
        f _ [] = []
        f n (x' : xs)
          | n <= m = (x', m) : f m xs
          | otherwise = f n xs
          where
            m = array ! x'

part2 :: Array (Int, Int) Int -> Int
part2 array = maximum $ map (uncurry f) starts
  where
    (x, y) = snd $ bounds array
    starts = [(j, i) | j <- [0 .. x], i <- [0 .. y]]

    f :: Int -> Int -> Int
    f j i =
      product $
        map
          (visible (array ! (j, i)) . tail)
          [ map (j,) $ reverse [0 .. i],
            map (,i) $ reverse [0 .. j],
            reverse $ map (j,) $ reverse [i .. y],
            reverse $ map (,i) $ reverse [j .. x]
          ]

    visible :: Int -> [(Int, Int)] -> Int
    visible k = length . f' k
      where
        f' :: Int -> [(Int, Int)] -> [(Int, Int)]
        f' _ [] = []
        f' n (x' : xs)
          | array ! x' < n = x' : f' n xs
          | otherwise = [x']

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith ($) [part1, part2]
      . replicate 2
      . intoArray
      . map (map (read @Int . (: [])))
      . lines
