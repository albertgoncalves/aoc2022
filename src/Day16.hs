{-# LANGUAGE TupleSections #-}

import Data.Char (isDigit, isUpper)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
  ( ReadP,
    eof,
    many1,
    munch1,
    readP_to_S,
    sepBy1,
    skipSpaces,
    string,
    (<++),
  )

type Valve = (String, (Int, [String]))

type Memo = M.Map (S.Set String, String, Int, Int) Int

ident :: ReadP String
ident = munch1 isUpper

valve :: ReadP Valve
valve = do
  _ <- string "Valve "
  label <- ident
  _ <- string " has flow rate="
  rate <- read <$> munch1 isDigit
  _ <- string "; tunnels lead to valves " <++ string "; tunnel leads to valve "
  (label,) . (rate,) <$> sepBy1 ident (string ", ")

-- NOTE: See `https://www.youtube.com/watch?v=DgqkVDr1WX8`.
-- NOTE: See `https://github.com/jonathanpaulson/AdventOfCode/blob/master/2022/16.cc`.
solve ::
  Int ->
  M.Map String (Int, [String]) ->
  Int ->
  S.Set String ->
  String ->
  Int ->
  Memo ->
  (Memo, Int)
solve _ _ 0 _ _ 0 memo = (memo, 0)
solve reset graph 0 visited _ others memo =
  solve reset graph reset visited "AA" (pred others) memo
solve reset graph time0 visited current others memo0 =
  case M.lookup (visited, current, others, time0) memo0 of
    Nothing ->
      let (value, neighbors) = (M.!) graph current
          -- NOTE: Turn on current valve.
          (memo1, best1) =
            if value /= 0 && S.notMember current visited
              then
                ((time1 * value) +)
                  <$> solve
                    reset
                    graph
                    time1
                    (S.insert current visited)
                    current
                    others
                    memo0
              else (memo0, 0)
          -- NOTE: Move to neighbors.
          (memo4, best4) =
            foldl'
              ( \(memo2, best2) destination ->
                  let (memo3, best3) =
                        solve
                          reset
                          graph
                          time1
                          visited
                          destination
                          others
                          memo2
                   in (memo3, max best2 best3)
              )
              (memo1, best1)
              neighbors
       in (M.insert (visited, current, others, time0) best4 memo4, best4)
    Just best0 -> (memo0, best0)
  where
    time1 = pred time0

main :: IO ()
main =
  interact $
    unlines
      . map (show . snd)
      . zipWith
        ( \(time, others) graph ->
            solve time graph time S.empty "AA" others M.empty
        )
        [(30, 0), (26, 1)]
      . repeat
      . M.fromList
      . fst
      . head
      . readP_to_S (many1 (valve <* skipSpaces) <* eof)
