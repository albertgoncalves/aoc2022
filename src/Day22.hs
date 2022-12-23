{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (first, second)
import Data.Char (isDigit)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    satisfy,
    (<++),
  )

data Dir
  = DirRight
  | DirDown
  | DirLeft
  | DirUp
  deriving (Enum, Show)

data Inst
  = InstMove Int
  | InstTurn Bool
  deriving (Show)

type Wall = Bool

type Pos = (Int, Int)

type Bounds = (Int, M.Map Int (Int, Int), M.Map Int (Int, Int))

type Wrapper = Pos -> Dir -> M.Map (Int, Int) Bool -> Bounds -> (Pos, Dir)

parse :: ReadP (Int, Int, M.Map Pos Wall, [Inst])
parse =
  (,,,)
    <$> (int <* char ' ')
    <*> (int <* newline)
    <*> (intoMap <$> many1 row <* newline)
    <*> (many1 inst <* newline <* eof)
  where
    newline :: ReadP Char
    newline = char '\n'

    int :: ReadP Int
    int = read <$> munch1 isDigit

    row :: ReadP [(Int, Wall)]
    row =
      map (second (== '#')) . filter ((/= ' ') . snd) . zip [0 ..]
        <$> many1 (satisfy (`elem` " .#")) <* newline

    turn :: ReadP Bool
    turn = (True <$ char 'R') <++ (False <$ char 'L')

    inst :: ReadP Inst
    inst = (InstMove <$> int) <++ (InstTurn <$> turn)

    intoMap :: [[(Int, Wall)]] -> M.Map Pos Wall
    intoMap = M.fromList . concat . zipWith (\i -> map (first (,i))) [0 ..]

translate :: Int -> Int -> Pos -> Dir -> Pos
translate k n (x, y) DirRight = (x + (k * n), y)
translate k n (x, y) DirDown = (x, y + (k * n))
translate k n (x, y) DirLeft = (x - (k * n), y)
translate k n (x, y) DirUp = (x, y - (k * n))

step :: Pos -> Dir -> (Pos, Dir)
step pos dir = (translate 1 1 pos dir, dir)

move ::
  (Int, Int) -> Wrapper -> Dir -> M.Map Pos Wall -> Bounds -> Maybe (Pos, Dir)
move pos0 f dir0 world bounds =
  case M.lookup pos1 world of
    Just False -> Just posDir1
    Just True -> Nothing
    Nothing | world ! pos2 -> Nothing
    Nothing -> Just posDir2
  where
    posDir1@(pos1, _) = step pos0 dir0
    posDir2@(pos2, _) = f pos0 dir0 world bounds

moves :: Wrapper -> Int -> Pos -> Dir -> M.Map Pos Wall -> Bounds -> (Pos, Dir)
moves _ 0 pos dir _ _ = (pos, dir)
moves f n pos0 dir0 world bounds =
  case move pos0 f dir0 world bounds of
    Just (pos1, dir1) -> moves f (pred n) pos1 dir1 world bounds
    Nothing -> (pos0, dir0)

changeDir :: Bool -> Dir -> Dir
changeDir True = toEnum . (`mod` 4) . (+ 1) . fromEnum
changeDir False = toEnum . (`mod` 4) . subtract 1 . fromEnum

run :: Wrapper -> Pos -> Dir -> M.Map Pos Wall -> Bounds -> [Inst] -> Int
run _ (x, y) dir _ _ [] = (succ x * 4) + (succ y * 1000) + fromEnum dir
run f pos dir world bounds (InstTurn turn : insts) =
  run f pos (changeDir turn dir) world bounds insts
run f pos0 dir0 world bounds (InstMove n : insts) =
  run f pos1 dir1 world bounds insts
  where
    (pos1, dir1) = moves f n pos0 dir0 world bounds

minMax :: [(Int, Int)] -> M.Map Int (Int, Int)
minMax =
  M.map (\values -> (minimum values, maximum values))
    . M.fromListWith (++)
    . map (second (: []))

intoBounds :: Int -> M.Map Pos Wall -> Bounds
intoBounds k world = (k, vertical, horizontal)
  where
    keys = M.keys world
    vertical = minMax keys
    horizontal = minMax $ map swap keys

part1 :: Wrapper
part1 (_, y) DirRight _ (_, _, horizontal) =
  ((fst $ horizontal ! y, y), DirRight)
part1 (x, _) DirDown _ (_, vertical, _) =
  ((x, fst $ vertical ! x), DirDown)
part1 (_, y) DirLeft _ (_, _, horizontal) =
  ((snd $ horizontal ! y, y), DirLeft)
part1 (x, _) DirUp _ (_, vertical, _) =
  ((x, snd $ vertical ! x), DirUp)

rotate :: Bool -> Int -> Pos -> Pos
rotate True k (x, y) = (xDelta - (yMod - (k - 1)), yDelta + xMod)
  where
    xMod = x `mod` k
    yMod = y `mod` k
    xDelta = x - xMod
    yDelta = y - yMod
rotate False k (x, y) = (xDelta + yMod, yDelta - (xMod - (k - 1)))
  where
    xMod = x `mod` k
    yMod = y `mod` k
    xDelta = x - xMod
    yDelta = y - yMod

translateRotate :: Int -> Int -> Bool -> Pos -> Dir -> (Pos, Dir)
translateRotate k n turn pos0 dir0 =
  (rotate turn k $ translate k n pos0 dir0, changeDir turn dir0)

part2 :: Wrapper
part2 pos@(11, _) dir@DirRight _ (4, _, _) =
  uncurry step $ translateRotate 4 1 True pos dir
part2 pos@(0, y) dir@DirLeft _ (50, _, _)
  | 150 <= y =
    uncurry step $
      uncurry (translateRotate 50 1 True) $
        translateRotate 50 4 True (rotate True 50 pos) $ changeDir True dir
part2 pos@(50, y) dir@DirLeft _ (50, _, _)
  | 50 <= y = uncurry step $ translateRotate 50 1 False pos dir
part2 pos@(99, y) dir@DirRight _ (50, _, _)
  | 100 < y =
    uncurry step $
      uncurry (translateRotate 50 2 False) $ translateRotate 50 2 False pos dir
part2 pos@(149, y) dir@DirRight _ (50, _, _)
  | y <= 50 =
    uncurry step $
      translateRotate 50 2 True (rotate True 50 pos) $ changeDir True dir
part2 pos dir@DirRight _ (k, _, _) =
  uncurry step $ translateRotate k 1 False pos dir
part2 pos dir@DirDown world (k, _, _) =
  head $
    filter ((`M.member` world) . fst) $
      map
        (uncurry step)
        [ translateRotate k 1 True pos dir,
          uncurry (translateRotate k 2 True) $
            translateRotate k 4 True (rotate True k $ rotate True k pos) $
              changeDir True $ changeDir True dir,
          translateRotate k 2 True (rotate True k pos) $ changeDir True dir
        ]
part2 pos dir@DirLeft world (k, _, _) =
  head $
    filter ((`M.member` world) . fst) $
      map
        (uncurry step)
        [ uncurry (translateRotate k 2 False) $
            translateRotate k 2 False pos dir,
          translateRotate k 2 True (rotate True k pos) $ changeDir True dir
        ]
part2 pos dir@DirUp world (k, _, _) =
  head $
    filter ((`M.member` world) . fst) $
      map
        (uncurry step)
        [ uncurry (translateRotate k 3 False) $
            translateRotate k 2 False (rotate False k pos) $
              changeDir False dir,
          uncurry (translateRotate k 2 True) $
            translateRotate k 4 True (rotate True k $ rotate True k pos) $
              changeDir True $ changeDir True dir,
          translateRotate k 1 True pos dir
        ]

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith
        ( \f (k, start, world, insts) ->
            run f (start, 0) DirRight world (intoBounds k world) insts
        )
        [part1, part2]
      . repeat
      . fst
      . head
      . readP_to_S parse
