{-# LANGUAGE TupleSections #-}

import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.List (inits)
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    look,
    many1,
    munch1,
    readP_to_S,
    string,
    (<++),
  )

data Command
  = CommandChange String
  | CommandList [(String, Int)] [String]

ident :: ReadP String
ident = munch1 (not . isSpace)

space :: ReadP ()
space = void $ char ' '

newline :: ReadP ()
newline = void $ char '\n'

commandChange :: ReadP Command
commandChange =
  CommandChange
    <$> (char '$' *> space *> string "cd" *> space *> ident <* newline)

listResults :: ReadP ([(String, Int)], [String])
listResults = do
  rest <- look
  case rest of
    'd' : _ -> do
      dir <- string "dir" *> space *> ident <* newline
      (files, dirs) <- listResults
      return (files, dir : dirs)
    c : _ | '0' <= c && c <= '9' -> do
      file <-
        flip (,) <$> (read <$> munch1 isDigit) <*> (space *> ident <* newline)
      (files, dirs) <- listResults
      return (file : files, dirs)
    _ -> return ([], [])

commandList :: ReadP Command
commandList =
  uncurry CommandList
    <$> (char '$' *> space *> string "ls" *> newline *> listResults)

execute :: [String] -> [Command] -> [([String], [(String, Int)])]
execute _ [] = []
execute parents (CommandChange ".." : commands) =
  execute (tail parents) commands
execute parents (CommandChange dir : commands) =
  execute (dir : parents) commands
execute parents (CommandList files _ : commands) =
  (reverse parents, files) : execute parents commands

tally ::
  [String] ->
  [(String, Int)] ->
  M.Map [String] Int ->
  M.Map [String] Int
tally path files =
  M.unionWith (+) $
    M.fromList $
      map (,sum $ map snd files) $
        filter (not . null) $
          inits path

part1 :: [Int] -> Int
part1 = sum . filter (<= 100000)

part2 :: [Int] -> Int
part2 sizes = minimum $ filter (30000000 - (70000000 - maximum sizes) <=) sizes

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith ($) [part1, part2]
      . repeat
      . map snd
      . M.toList
      . foldr (uncurry tally) M.empty
      . execute []
      . fst
      . head
      . readP_to_S (many1 (commandChange <++ commandList) <* eof)
