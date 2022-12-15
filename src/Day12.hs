{-# LANGUAGE TupleSections #-}

import Data.Array (Array, Ix, assocs, bounds, listArray, (!))
import Data.Ix (inRange)
import Data.List (transpose)
import qualified Data.Set as S

type Pos = (Int, Int)

swap :: Char -> Char
swap 'S' = 'a'
swap 'E' = 'z'
swap c = c

steps :: (Enum e, Ord e) => Array Pos e -> Pos -> [Pos]
steps array (j, i) =
  map fst $
    filter ((<= succ current) . snd) $
      zip candidates $ map (array !) candidates
  where
    candidates =
      filter
        (inRange $ bounds array)
        [(pred j, i), (succ j, i), (j, pred i), (j, succ i)]
    current = array ! (j, i)

search ::
  (Enum e, Ord e, Enum c) =>
  S.Set Pos ->
  Array Pos e ->
  [(Pos, c)] ->
  [(Pos, c)]
search visited array ((start, n) : rest)
  | S.member start visited = (start, n) : search visited array rest
  | otherwise =
    (start, n) :
    search
      (S.insert start visited)
      array
      (rest ++ map (,succ n) (steps array start))
search _ _ _ = []

find :: (Ix i, Eq e) => (e -> Bool) -> Array i e -> [i]
find f = map fst . filter (f . snd) . assocs

main :: IO ()
main =
  interact $
    unlines
      . map (show . snd . head)
      . zipWith
        ( \starts array ->
            filter ((== head (find (== 'E') array)) . fst) $
              search
                S.empty
                (swap <$> array)
                $ map (,0 :: Int) $ find (`elem` starts) array
        )
        ["S", "Sa"]
      . repeat
      . ( \rows ->
            listArray
              ((0, 0), (length (head rows) - 1, length rows - 1))
              $ concat $ transpose rows
        )
      . lines
