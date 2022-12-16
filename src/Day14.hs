import Data.Char (isDigit)
import qualified Data.Set as S
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
  )
import Prelude hiding (floor)

type Pair = (Int, Int)

int :: ReadP Int
int = read <$> munch1 isDigit

range :: Int -> Int -> [Int]
range a b
  | a < b = [a .. b]
  | otherwise = [b .. a]

expand :: [Pair] -> [(Int, Int)]
expand ((x0, y0) : pairs@((x1, y1) : _))
  | x0 == x1 = [(x0, y) | y <- range y0 y1] ++ expand pairs
  | y0 == y1 = [(x, y0) | x <- range x0 x1] ++ expand pairs
  | otherwise = undefined
expand _ = []

sim :: Int -> Int -> Int -> S.Set (Int, Int) -> Maybe (S.Set (Int, Int))
sim floor x0 y0 world
  | floor < y0 = Nothing
  | S.notMember (x0, y1) world = sim floor x0 y1 world
  | S.notMember (x1, y1) world = sim floor x1 y1 world
  | S.notMember (x2, y1) world = sim floor x2 y1 world
  | otherwise = Just $ S.insert (x0, y0) world
  where
    y1 = succ y0
    x1 = pred x0
    x2 = succ x0

unfold :: (a -> Maybe a) -> a -> [a]
unfold f x0 =
  case f x0 of
    Nothing -> []
    Just x1 -> x1 : unfold f x1

part1 :: S.Set (Int, Int) -> Int
part1 world =
  length $ unfold (sim (maximum $ map snd $ S.toList world) 500 0) world

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith ($) [part1]
      . repeat
      . S.fromList
      . concatMap expand
      . fst
      . head
      . readP_to_S
        ( many1
            ( sepBy1
                ((,) <$> (int <* char ',') <*> int)
                (string " -> ")
                <* skipSpaces
            )
            <* eof
        )
