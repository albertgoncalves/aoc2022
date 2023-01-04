{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (first, second)
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (ceiling, div, drop, mod)

data Shape
  = ShapeMinus
  | ShapePlus
  | ShapeCorner
  | ShapePipe
  | ShapeSquare

type Pos = (Int, Int)

width :: Int
width = 7

offset :: Int
offset = 2

intoPos :: Shape -> [Pos]
intoPos ShapeMinus = map (,0) [offset .. offset + 3]
intoPos ShapePlus = map (,1) [offset .. offset + 2] ++ map (offset + 1,) [0, 2]
intoPos ShapeCorner =
  map (,0) [offset .. offset + 2] ++ map (offset + 2,) [1, 2]
intoPos ShapePipe = map (offset,) [0 .. 3]
intoPos ShapeSquare = (,) <$> [offset, offset + 1] <*> [0, 1]

push :: Char -> [Pos] -> [Pos]
push '<' = map (first $ subtract 1)
push '>' = map (first (+ 1))
push _ = undefined

stepPush :: S.Set Pos -> Char -> [Pos] -> [Pos]
stepPush world direction from
  | any ((< 0) . fst) to = from
  | any ((width <=) . fst) to = from
  | any (`S.member` world) to = from
  | otherwise = to
  where
    to = push direction from

stepFall :: S.Set Pos -> [Pos] -> (Bool, [Pos])
stepFall world from
  | any ((== 0) . snd) to = (False, from)
  | any (`S.member` world) to = (False, from)
  | otherwise = (True, to)
  where
    to = map (second $ subtract 1) from

stepLoop :: S.Set Pos -> [(Int, Char)] -> [Pos] -> ([(Int, Char)], S.Set Pos)
stepLoop world ((_, direction) : directions) from =
  case stepFall world $ stepPush world direction from of
    (True, to) -> stepLoop world directions to
    (False, to) -> (directions, foldr S.insert world to)
stepLoop _ _ _ = undefined

ceiling :: S.Set Pos -> Int
ceiling = maximum . (0 :) . map snd . S.elems

drop ::
  (S.Set Pos, [(Int, Shape)], [(Int, Char)]) ->
  (S.Set Pos, [(Int, Shape)], [(Int, Char)])
drop (world0, (_, shape) : shapes, directions0) = (world1, shapes, directions1)
  where
    (directions1, world1) =
      stepLoop world0 directions0 $
        map (second (+ (4 + ceiling world0))) $
          intoPos shape
drop _ = undefined

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

part1 :: [(S.Set Pos, [(Int, Shape)], [(Int, Char)])] -> Int
part1 = ceiling . fst3 . (!! 2022)

intoSig :: (S.Set Pos, [(Int, Shape)], [(Int, Char)]) -> ([Int], Int, Int)
intoSig (world, (shape, _) : _, (direction, _) : _) =
  ( map ((ceiling world -) . snd) $
      M.toAscList $
        M.fromListWith max $
          S.elems world,
    shape,
    direction
  )
intoSig _ = undefined

part2 ::
  M.Map ([Int], Int, Int) (Int, Int) ->
  [(Int, (S.Set Pos, [(Int, Shape)], [(Int, Char)]))] ->
  Int
part2 visited ((step1, iteration) : iterations) =
  case M.lookup sig visited of
    Nothing -> part2 (M.insert sig (step1, height1) visited) iterations
    Just (step0, height0) ->
      let (div, mod) = (1000000000000 - step0) `divMod` (step1 - step0)
          height2 = ceiling $ fst3 $ snd $ iterations !! mod
       in height0 + ((height1 - height0) * div) + (height2 - height1)
  where
    height1 = ceiling $ fst3 iteration
    sig = intoSig iteration
part2 _ _ = undefined

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith ($) [part1, part2 M.empty . zip [1 ..]]
      . repeat
      . ( \directions ->
            iterate
              drop
              ( S.empty,
                cycle $
                  zip
                    [0 ..]
                    [ ShapeMinus,
                      ShapePlus,
                      ShapeCorner,
                      ShapePipe,
                      ShapeSquare
                    ],
                directions
              )
        )
      . cycle
      . zip [0 ..]
      . head
      . lines
