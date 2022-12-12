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

execute :: Array Int String -> Move -> Array Int String
execute stacks (Move 0 _ _) = stacks
execute stacks (Move n from to) =
  case stacks ! from of
    (x : xs) ->
      execute (stacks // [(from, xs), (to, x : stacks ! to)]) $
        Move (n - 1) from to
    _ -> undefined

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
main = interact $ map head . elems . uncurry (foldl execute) . parse
