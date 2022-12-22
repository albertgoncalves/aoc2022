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

type Bounds = (M.Map Int (Int, Int), M.Map Int (Int, Int))

type Wrapper = Pos -> Dir -> Bounds -> (Pos, Dir)

parse :: ReadP (M.Map Pos Wall, [Inst])
parse =
  (,)
    <$> (intoMap <$> many1 row <* newline)
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

step :: Pos -> Dir -> Pos
step (x, y) DirRight = (succ x, y)
step (x, y) DirDown = (x, succ y)
step (x, y) DirLeft = (pred x, y)
step (x, y) DirUp = (x, pred y)

move ::
  (Int, Int) -> Wrapper -> Dir -> M.Map Pos Wall -> Bounds -> Maybe (Pos, Dir)
move pos0 f dir0 world bounds =
  case M.lookup pos1 world of
    Just False -> Just (pos1, dir0)
    Just True -> Nothing
    Nothing | world ! pos2 -> Nothing
    Nothing -> Just posDir2
  where
    pos1 = step pos0 dir0
    posDir2@(pos2, _) = f pos0 dir0 bounds

moves :: Wrapper -> Int -> Pos -> Dir -> M.Map Pos Wall -> Bounds -> (Pos, Dir)
moves _ 0 pos dir _ _ = (pos, dir)
moves f n pos0 dir0 world bounds =
  case move pos0 f dir0 world bounds of
    Just (pos1, dir1) -> moves f (pred n) pos1 dir1 world bounds
    Nothing -> (pos0, dir0)

intoDir :: Dir -> Bool -> Dir
intoDir dir True = toEnum $ (fromEnum dir + 1) `mod` 4
intoDir dir False = toEnum $ (fromEnum dir - 1) `mod` 4

run :: Wrapper -> Pos -> Dir -> M.Map Pos Wall -> Bounds -> [Inst] -> Int
run _ (x, y) dir _ _ [] = (succ x * 4) + (succ y * 1000) + fromEnum dir
run f pos dir world bounds (InstTurn turn : insts) =
  run f pos (intoDir dir turn) world bounds insts
run f pos0 dir0 world bounds (InstMove n : insts) =
  run f pos1 dir1 world bounds insts
  where
    (pos1, dir1) = moves f n pos0 dir0 world bounds

minMax :: [(Int, Int)] -> M.Map Int (Int, Int)
minMax =
  M.map (\values -> (minimum values, maximum values))
    . M.fromListWith (++)
    . map (second (: []))

intoBounds :: M.Map Pos Wall -> Bounds
intoBounds world = (minMax keys, minMax $ map swap keys)
  where
    keys = M.keys world

part1 :: Wrapper
part1 (_, y) DirRight (_, horizontal) = ((fst $ horizontal ! y, y), DirRight)
part1 (x, _) DirDown (vertical, _) = ((x, fst $ vertical ! x), DirDown)
part1 (_, y) DirLeft (_, horizontal) = ((snd $ horizontal ! y, y), DirLeft)
part1 (x, _) DirUp (vertical, _) = ((x, snd $ vertical ! x), DirUp)

part2 :: Wrapper
part2 = undefined

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith
        ( \f (world, insts) ->
            run f (8, 0) DirRight world (intoBounds world) insts
        )
        [part1, part2]
      . repeat
      . fst
      . head
      . readP_to_S parse
