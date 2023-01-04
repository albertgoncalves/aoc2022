import Control.Monad (void)
import Data.Char (isDigit)
import Data.List (findIndices, sort)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    sepBy,
    skipSpaces,
    (<++),
  )

data Data
  = DataList [Data]
  | DataInt Int
  deriving (Eq)

data Status
  = StatusOk
  | StatusError
  | StatusContinue
  deriving (Eq)

instance Ord Data where
  compare l r
    | l == r = EQ
    | otherwise =
        case check l r of
          StatusOk -> LT
          StatusError -> GT
          StatusContinue -> undefined

dataList :: ReadP Data
dataList =
  DataList
    <$> ( char '['
            *> sepBy
              (dataList <++ (DataInt . read <$> munch1 isDigit))
              (char ',')
            <* char ']'
        )

newline :: ReadP ()
newline = void $ char '\n'

pair :: ReadP (Data, Data)
pair = (,) <$> (dataList <* newline) <*> (dataList <* newline)

check :: Data -> Data -> Status
check (DataInt l) (DataInt r)
  | l == r = StatusContinue
  | l < r = StatusOk
  | otherwise = StatusError
check l@(DataInt _) rs@(DataList _) = check (DataList [l]) rs
check ls@(DataList _) r@(DataInt _) = check ls (DataList [r])
check (DataList []) (DataList (_ : _)) = StatusOk
check (DataList (_ : _)) (DataList []) = StatusError
check (DataList (l : ls)) (DataList (r : rs)) =
  case check l r of
    StatusContinue -> check (DataList ls) (DataList rs)
    status -> status
check (DataList []) (DataList []) = StatusContinue

part1 :: [(Data, Data)] -> Int
part1 =
  sum
    . map fst
    . filter ((== StatusOk) . snd)
    . zip [1 :: Int ..]
    . map (uncurry check)

part2 :: [(Data, Data)] -> Int
part2 =
  product
    . map succ
    . findIndices (`elem` xs)
    . sort
    . (xs ++)
    . concatMap (\(a, b) -> [a, b])
  where
    xs = [DataList [DataList [DataInt 2]], DataList [DataList [DataInt 6]]]

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith ($) [part1, part2]
      . repeat
      . fst
      . head
      . readP_to_S (many1 (pair <* skipSpaces) <* eof)
