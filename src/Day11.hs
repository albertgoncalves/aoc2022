import Data.Char (isDigit)
import Data.IntMap ((!))
import qualified Data.IntMap as M
import Data.List (foldl', sort)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    sepBy1,
    skipSpaces,
    string,
    (<++),
  )
import Prelude hiding (round)

data Monkey = Monkey (Int -> Int) Int Int Int

monkeyTest :: Monkey -> Int
monkeyTest (Monkey _ test _ _) = test

int :: ReadP Int
int = read <$> munch1 isDigit

monkey :: ReadP (Int, [Int], Monkey)
monkey = do
  _ <- string "Monkey "
  index <- int
  _ <- char ':'
  skipSpaces
  _ <- string "Starting items: "
  items <- sepBy1 int (string ", ")
  skipSpaces
  _ <- string "Operation: new = old "
  f <- ((+) <$ char '+') <++ ((*) <$ char '*')
  skipSpaces
  op <- (f <$> int) <++ ((\x -> f x x) <$ string "old")
  skipSpaces
  _ <- string "Test: divisible by "
  test <- int
  skipSpaces
  _ <- string "If true: throw to monkey "
  ifTrue <- int
  skipSpaces
  _ <- string "If false: throw to monkey "
  ifFalse <- int
  return (index, items, Monkey op test ifTrue ifFalse)

pop :: Enum a => Int -> M.IntMap (a, [b]) -> Maybe (b, M.IntMap (a, [b]))
pop k m =
  case m ! k of
    (_, []) -> Nothing
    (n, v : vs) -> Just (v, M.insert k (succ n, vs) m)

append :: Int -> b -> M.IntMap (a, [b]) -> M.IntMap (a, [b])
append k v m = M.insert k (n, vs ++ [v]) m
  where
    (n, vs) = m ! k

solve :: (Int, Bool) -> ([Int], [[Int]], [Monkey]) -> [M.IntMap (Int, [Int])]
solve (n, flag) (indices, items, monkeys0) =
  drop n $ iterate round $ M.fromList $ zip indices $ zip (repeat 0) items
  where
    f =
      if flag
        then (`mod` product (map monkeyTest monkeys0))
        else (`div` 3)
    monkeys1 = M.fromList $ zip indices monkeys0

    turn :: M.IntMap (Int, [Int]) -> Int -> M.IntMap (Int, [Int])
    turn items0 index =
      case pop index items0 of
        Nothing -> items0
        Just (x0, items1) ->
          let Monkey op test ifTrue ifFalse = monkeys1 ! index
              x1 = f $ op x0
              recipient =
                if x1 `mod` test == 0
                  then ifTrue
                  else ifFalse
           in turn (append recipient x1 items1) index

    round :: M.IntMap (Int, [Int]) -> M.IntMap (Int, [Int])
    round = flip (foldl' turn) indices

main :: IO ()
main =
  interact $
    unlines
      . map
        (show . product . take 2 . reverse . sort . map fst . M.elems . head)
      . zipWith solve [(20, False), (10000, True)]
      . repeat
      . unzip3
      . fst
      . head
      . readP_to_S (many1 (monkey <* skipSpaces) <* eof)
