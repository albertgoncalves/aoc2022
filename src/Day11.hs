{-# LANGUAGE TupleSections #-}

import Control.Monad.State (State, execState, modify, state)
import Data.Char (isDigit)
import Data.List (sort)
import qualified Data.Map as M
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

data Op
  = OpAdd
  | OpMul

data Monkey = Monkey
  { monkeyItems :: [Int],
    monkeyOp :: (Op, Maybe Int),
    monkeyTest :: Int,
    monkeyIfTrue :: Int,
    monkeyIfFalse :: Int
  }

int :: ReadP Int
int = read <$> munch1 isDigit

operation :: ReadP (Op, Maybe Int)
operation = do
  _ <- string "new = old "
  op <- (OpAdd <$ char '+') <++ (OpMul <$ char '*')
  skipSpaces
  (op,) <$> (Just <$> int) <++ (Nothing <$ string "old")

monkey :: ReadP Monkey
monkey = do
  _ <- string "Monkey "
  _ <- int
  _ <- char ':'
  skipSpaces
  _ <- string "Starting items: "
  items <- sepBy1 int (string ", ")
  skipSpaces
  _ <- string "Operation: "
  op <- operation
  skipSpaces
  _ <- string "Test: divisible by "
  test <- int
  skipSpaces
  _ <- string "If true: throw to monkey "
  ifTrue <- int
  skipSpaces
  _ <- string "If false: throw to monkey "
  Monkey items op test ifTrue <$> int

getMonkey :: Int -> State (M.Map Int (Int, Monkey)) (Int, Monkey)
getMonkey i = state $ \s -> ((M.!) s i, s)

popItem :: Int -> State (M.Map Int (Int, Monkey)) (Maybe Int)
popItem i = do
  (t, m) <- getMonkey i
  case monkeyItems m of
    [] -> return Nothing
    (x : xs) -> state $ \s ->
      (Just x, M.insert i (t + 1, m {monkeyItems = xs}) s)

pushItem :: Int -> Int -> State (M.Map Int (Int, Monkey)) ()
pushItem i x = do
  (t, m) <- getMonkey i
  modify $ \s -> M.insert i (t, m {monkeyItems = monkeyItems m ++ [x]}) s

select :: Op -> (Int -> Int -> Int)
select OpAdd = (+)
select OpMul = (*)

runOp :: Op -> Maybe Int -> Int -> Int
runOp op (Just a) b = select op a b
runOp op Nothing x = select op x x

turn :: Maybe Int -> Int -> State (M.Map Int (Int, Monkey)) ()
turn mode i = do
  maybeItem <- popItem i
  case maybeItem of
    Nothing -> return ()
    Just x0 -> do
      m <- snd <$> getMonkey i
      let x1 =
            case mode of
              Nothing -> uncurry runOp (monkeyOp m) x0 `div` 3
              Just d -> uncurry runOp (monkeyOp m) x0 `mod` d
      pushItem
        ( ( if x1 `mod` monkeyTest m == 0
              then monkeyIfTrue
              else monkeyIfFalse
          )
            m
        )
        x1
      turn mode i

round :: Bool -> State (M.Map Int (Int, Monkey)) ()
round mode = do
  n <- state $ \s -> (M.size s, s)
  d <-
    if mode
      then state $ \s ->
        (Just $ product $ map (monkeyTest . snd) $ M.elems s, s)
      else return Nothing
  mapM_ (turn d) [0 .. n - 1]

sim :: Int -> Bool -> [Monkey] -> String
sim k mode =
  show
    . product
    . take 2
    . reverse
    . sort
    . map (fst . snd)
    . M.toList
    . last
    . take k
    . iterate (execState (round mode))
    . M.fromList
    . zip [0 ..]
    . zip (repeat 0)

main :: IO ()
main =
  interact $
    unlines
      . zipWith (uncurry sim) [(21, False), (10001, True)]
      . repeat
      . fst
      . head
      . readP_to_S (many1 (monkey <* skipSpaces) <* eof)
