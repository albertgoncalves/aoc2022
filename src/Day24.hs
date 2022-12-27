{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (bimap)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)

type Pos = (Int, Int)

type World = M.Map Pos String

type History = M.Map Int World

bounds :: [(Int, [(Int, a)])] -> (Int, Int)
bounds = swap . ((fst . last) <$>) . last

extract :: Int -> [(Int, Char)] -> [(Pos, String)]
extract y = map (bimap (,y) (: [])) . filter ((/= '.') . snd)

sim :: Int -> Int -> History -> Int -> (History, World)
sim width height history0 time =
  case M.lookup time history0 of
    Just world -> (history0, world)
    Nothing ->
      let (history1, world0) = sim width height history0 $ pred time
          world1 =
            M.fromListWith (++) $
              concatMap (uncurry $ step width height) $ M.toList world0
       in (M.insert time world1 history1, world1)

step :: Int -> Int -> Pos -> String -> [(Pos, String)]
step _ _ _ [] = []
step width height pos@(x, y) ('>' : dirs) =
  ((succ x `mod` width, y), ">") : step width height pos dirs
step width height pos@(x, y) ('<' : dirs) =
  ((pred x `mod` width, y), "<") : step width height pos dirs
step width height pos@(x, y) ('^' : dirs) =
  ((x, succ $ pred (pred y) `mod` height), "^") : step width height pos dirs
step width height pos@(x, y) ('v' : dirs) =
  ((x, succ $ pred (succ y) `mod` height), "v") : step width height pos dirs
step _ _ _ _ = undefined

neighbors :: Int -> Int -> Pos -> [Pos]
neighbors width height (x, y) =
  [(x, y)]
    ++ [(left, y) | (y /= succ height) && (0 <= left)]
    ++ [(right, y) | (y /= 0) && (right < width)]
    ++ [(x, up) | ((x == 0) && (y == 1)) || (0 < up)]
    ++ [(x, down) | ((x == pred width) && (y == height)) || (down <= height)]
  where
    left = pred x
    right = succ x
    up = pred y
    down = succ y

search ::
  Int ->
  Int ->
  Pos ->
  S.Set (Int, Pos) ->
  History ->
  [(Int, Pos)] ->
  (History, Int)
search _ _ end _ history ((time, pos) : _)
  | pos == end = (history, time)
search width height end visited history0 ((time0, pos) : queue0) =
  search width height end (foldr S.insert visited queue1) history1 $
    queue0 ++ queue1
  where
    (history1, world1) = sim width height history0 time1
    queue1 =
      filter (`S.notMember` visited) $
        map (time1,) $
          filter (`M.notMember` world1) $ neighbors width height pos
    time1 = succ time0
search _ _ _ _ _ _ = undefined

searchFromTo :: Int -> Pos -> Pos -> Int -> Int -> History -> (History, Int)
searchFromTo time from to width height history =
  search width height to S.empty history [(time, from)]

part1 :: (Int, Int) -> World -> Int
part1 (width0, height0) world =
  snd $
    searchFromTo 0 (0, 0) (width0, height0) width1 height1 $
      M.singleton 0 world
  where
    width1 = succ width0
    height1 = pred height0

part2 :: (Int, Int) -> World -> Int
part2 (width0, height0) world0 = time2
  where
    width1 = succ width0
    height1 = pred height0
    start = (0, 0) :: (Int, Int)
    end = (width0, height0)
    (history0, time0) =
      searchFromTo 0 start end width1 height1 $ M.singleton 0 world0
    (history1, time1) = searchFromTo time0 end start width1 height1 history0
    (_, time2) = searchFromTo time1 start end width1 height1 history1

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith uncurry [part1, part2]
      . repeat
      . ( \input ->
            (bounds input, M.fromList $ concatMap (uncurry extract) input)
        )
      . zip [0 ..]
      . map (filter ((/= '#') . snd) . zip [0 ..] . tail)
      . lines
