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
  deriving (Show)

data Monkey = Monkey
  { monkeyItems :: [Int],
    monkeyOp :: (Op, Maybe Int),
    monkeyTest :: Int,
    monkeyIfTrue :: Int,
    monkeyIfFalse :: Int
  }
  deriving (Show)

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

part1 :: [Monkey] -> [Int]
part1 =
  map (fst . snd)
    . M.toList
    . last
    . take 21
    . iterate (execState round)
    . M.fromList
    . zip [0 ..]
    . zip (repeat 0)
  where
    getMonkey :: Int -> State (M.Map Int (Int, Monkey)) (Int, Monkey)
    getMonkey i = state $ \s -> ((M.!) s i, s)

    popItem :: Int -> State (M.Map Int (Int, Monkey)) (Maybe Int)
    popItem i = do
      (t, m) <- getMonkey i
      case monkeyItems m of
        [] -> return Nothing
        (x : xs) -> state $ \s ->
          ( Just x,
            M.insert
              i
              (t + 1, m {monkeyItems = xs})
              s
          )

    pushItem :: Int -> Int -> State (M.Map Int (Int, Monkey)) ()
    pushItem i x = do
      (t, m) <- getMonkey i
      modify $ \s ->
        M.insert
          i
          (t, m {monkeyItems = monkeyItems m ++ [x]})
          s

    select :: Op -> (Int -> Int -> Int)
    select OpAdd = (+)
    select OpMul = (*)

    runOp :: Op -> Maybe Int -> Int -> Int
    runOp op (Just a) b = select op a b
    runOp op Nothing x = select op x x

    turn :: Int -> State (M.Map Int (Int, Monkey)) ()
    turn i = do
      item0 <- popItem i
      case item0 of
        Nothing -> return ()
        Just x -> do
          m <- snd <$> getMonkey i
          let d = monkeyTest m
          let item1 = uncurry runOp (monkeyOp m) x
          let item2 = item1 `div` 3
          let j =
                ( if item2 `mod` d == 0
                    then monkeyIfTrue
                    else monkeyIfFalse
                )
                  m
          pushItem j item2
          turn i

    round :: State (M.Map Int (Int, Monkey)) ()
    round = do
      n <- state $ \s -> (M.size s, s)
      mapM_ turn [0 .. n - 1]

main :: IO ()
main =
  interact $
    unlines
      . map (show . product . take 2 . reverse . sort)
      . zipWith ($) [part1]
      . repeat
      . fst
      . head
      . readP_to_S (many1 (monkey <* skipSpaces) <* eof)
