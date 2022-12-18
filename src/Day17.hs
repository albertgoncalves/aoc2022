{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (first, second)
import qualified Data.Set as S
import Prelude hiding (ceiling, drop)

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

stepLoop :: S.Set Pos -> String -> [Pos] -> (String, S.Set Pos)
stepLoop world (direction : directions) from =
  case stepFall world $ stepPush world direction from of
    (True, to) -> stepLoop world directions to
    (False, to) -> (directions, foldr S.insert world to)
stepLoop _ _ _ = undefined

ceiling :: S.Set Pos -> Int
ceiling = maximum . (0 :) . map snd . S.elems

drop :: (S.Set Pos, [Shape], String) -> (S.Set Pos, [Shape], String)
drop (world0, shape : shapes, directions0) = (world1, shapes, directions1)
  where
    (directions1, world1) =
      stepLoop world0 directions0 $
        map (second (+ (4 + ceiling world0))) $ intoPos shape
drop _ = undefined

main :: IO ()
main =
  interact $
    unlines
      . map (show . ceiling . (\(world, _, _) -> world))
      . zipWith (flip (!!)) [2022]
      . repeat
      . ( \directions ->
            iterate
              drop
              ( S.empty,
                cycle
                  [ShapeMinus, ShapePlus, ShapeCorner, ShapePipe, ShapeSquare],
                directions
              )
        )
      . cycle
      . head
      . lines
