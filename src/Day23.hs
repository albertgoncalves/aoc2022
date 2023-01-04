{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import qualified Data.Set as S

data Dir
  = DirNorth
  | DirSouth
  | DirWest
  | DirEast
  deriving (Show)

type Pos = (Int, Int)

trio :: (Int -> (Int, Int)) -> Int -> [(Int, Int)]
trio f x = map (f . ($ x)) [pred, id, succ]

neighbors :: Pos -> Maybe Dir -> [Pos]
neighbors (x, y) (Just DirNorth) = trio (,pred y) x
neighbors (x, y) (Just DirSouth) = trio (,succ y) x
neighbors (x, y) (Just DirWest) = trio (pred x,) y
neighbors (x, y) (Just DirEast) = trio (succ x,) y
neighbors (x, y) Nothing =
  (pred x, y) : (succ x, y) : trio (,pred y) x ++ trio (,succ y) x

step :: Pos -> Dir -> Pos
step (x, y) DirNorth = (x, pred y)
step (x, y) DirSouth = (x, succ y)
step (x, y) DirWest = (pred x, y)
step (x, y) DirEast = (succ x, y)

propose :: S.Set Pos -> [Dir] -> Pos -> [(Pos, Pos)]
propose world dirs pos
  | not $ any (`S.member` world) $ neighbors pos Nothing = []
  | otherwise =
      take 1 $
        map (snd <$>) $
          filter (not . any (`S.member` world) . fst . snd) $
            zip (map (step pos) dirs) $
              zip (map (neighbors pos . Just) dirs) $
                repeat pos

resolve :: S.Set Pos -> [(Pos, Pos)] -> S.Set Pos
resolve world =
  ( \(inserts, deletes) ->
      foldr S.insert (foldr S.delete world deletes) inserts
  )
    . unzip
    . map (head <$>)
    . filter ((== 1) . length . snd)
    . M.toList
    . M.fromListWith (++)
    . map ((: []) <$>)

run :: [Dir] -> S.Set Pos -> S.Set Pos
run dirs world = resolve world $ concatMap (propose world dirs) $ S.elems world

tally :: S.Set Pos -> Int
tally world =
  sum $
    concat
      [ [ if S.member (x, y) world then 0 else 1
          | x <- [xMin .. xMax]
        ]
        | y <- [yMin .. yMax]
      ]
  where
    xs = S.map fst world
    ys = S.map snd world
    xMin = S.findMin xs
    yMin = S.findMin ys
    xMax = S.findMax xs
    yMax = S.findMax ys

part1 :: Int -> [Dir] -> S.Set Pos -> Int
part1 0 _ world = tally world
part1 n dirs world = part1 (pred n) (tail dirs) $ run (take 4 dirs) world

part2 :: Int -> [Dir] -> S.Set Pos -> Int
part2 n dirs world0
  | world0 == world1 = succ n
  | otherwise = part2 (succ n) (tail dirs) world1
  where
    world1 = run (take 4 dirs) world0

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith uncurry [part1 10, part2 0]
      . repeat
      . (cycle [DirNorth, DirSouth, DirWest, DirEast],)
      . S.fromList
      . concat
      . zipWith (\i -> map (\(j, _) -> (j, i))) [0 :: Int ..]
      . map (filter ((== '#') . snd) . zip [0 :: Int ..])
      . lines
