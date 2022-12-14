import Data.Array (Array, elems, listArray, (!), (//))
import Data.Char (isDigit, isUpper)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    satisfy,
    sepBy1,
    skipSpaces,
    string,
    (<++),
  )

data Move = Move Int Int Int
  deriving (Show)

block :: ReadP (Maybe Char)
block =
  (Just <$> (char '[' *> satisfy isUpper <* char ']'))
    <++ (Nothing <$ string "   ")

row :: ReadP [Maybe Char]
row = sepBy1 block (char ' ')

int :: ReadP Int
int = read <$> munch1 isDigit

move :: ReadP Move
move = do
  _ <- string "move"
  skipSpaces
  count <- int
  skipSpaces
  _ <- string "from"
  skipSpaces
  from <- int
  skipSpaces
  _ <- string "to"
  skipSpaces
  Move count from <$> int

intoStacks :: [[Maybe Char]] -> Array Int String
intoStacks xs = listArray (1, length $ head xs) $ map catMaybes $ transpose xs

part1 :: Array Int String -> Move -> Array Int String
part1 stacks (Move 0 _ _) = stacks
part1 stacks (Move n from to) =
  case stacks ! from of
    (x : xs) ->
      part1 (stacks // [(from, xs), (to, x : stacks ! to)]) $
        Move (n - 1) from to
    _ -> undefined

part2 :: Array Int String -> Move -> Array Int String
part2 stacks (Move n from to) = stacks // [(from, b), (to, a ++ stacks ! to)]
  where
    (a, b) = splitAt n $ stacks ! from

parse :: String -> (Array Int String, [Move])
parse =
  fst
    . head
    . readP_to_S
      ( do
          rows <- sepBy1 row (char '\n')
          skipSpaces
          _ <- sepBy1 int skipSpaces
          skipSpaces
          moves <- many1 $ move <* skipSpaces
          eof
          return (intoStacks rows, moves)
      )

main :: IO ()
main =
  interact $
    unlines
      . map (map head . elems)
      . zipWith (uncurry . foldl) [part1, part2]
      . repeat
      . parse
