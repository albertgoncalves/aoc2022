{-# LANGUAGE Strict #-}

import Data.Char (isDigit)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
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
import Prelude hiding (id)

data Res = Res
  { resOre :: Int,
    resClay :: Int,
    resObsid :: Int,
    resGeode :: Int
  }
  deriving (Eq, Show)

type Blueprint = (Res, Res, Res, Res, Res)

instance Num Res where
  (+) (Res r0 c0 o0 g0) (Res r1 c1 o1 g1) =
    Res (r0 + r1) (c0 + c1) (o0 + o1) (g0 + g1)
  (-) (Res r0 c0 o0 g0) (Res r1 c1 o1 g1) =
    Res (r0 - r1) (c0 - c1) (o0 - o1) (g0 - g1)
  (*) (Res r0 c0 o0 g0) (Res r1 c1 o1 g1) =
    Res (r0 * r1) (c0 * c1) (o0 * o1) (g0 * g1)
  abs (Res r c o g) = Res (abs r) (abs c) (abs o) (abs g)
  signum (Res r c o g) = Res (signum r) (signum c) (signum o) (signum g)
  fromInteger n =
    Res (fromInteger n) (fromInteger n) (fromInteger n) (fromInteger n)

instance Ord Res where
  (Res r0 c0 o0 g0) <= (Res r1 c1 o1 g1) =
    r0 <= r1 && c0 <= c1 && o0 <= o1 && g0 <= g1

parse :: ReadP [(Int, Blueprint)]
parse = many1 (blueprint <* skipSpaces) <* eof
  where
    int :: ReadP Int
    int = read <$> munch1 isDigit

    cost :: ReadP Res
    cost =
      string "costs "
        *> (sum <$> sepBy1 (ore <++ clay <++ obsid) (string " and "))
      where
        ore = (\c -> Res c 0 0 0) <$> (int <* string " ore")
        clay = (\c -> Res 0 c 0 0) <$> (int <* string " clay")
        obsid = (\c -> Res 0 0 c 0) <$> (int <* string " obsidian")

    blueprint :: ReadP (Int, Blueprint)
    blueprint = do
      _ <- string "Blueprint "
      id <- int
      _ <- char ':'

      skipSpaces
      _ <- string "Each ore robot "
      ore <- cost
      _ <- char '.'

      skipSpaces
      _ <- string "Each clay robot "
      clay <- cost
      _ <- char '.'

      skipSpaces
      _ <- string "Each obsidian robot "
      obsid <- cost
      _ <- char '.'

      skipSpaces
      _ <- string "Each geode robot "
      geode <- cost
      _ <- char '.'

      return
        ( id,
          ( ore,
            clay,
            obsid,
            geode,
            foldr1
              ( \(Res r0 c0 o0 g0) (Res r1 c1 o1 g1) ->
                  Res (max r0 r1) (max c0 c1) (max o0 o1) (max g0 g1)
              )
              [ore, clay, obsid, geode]
          )
        )

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith ($) [part1, part2]
      . repeat
      . fst
      . head
      . readP_to_S parse
  where
    part1 :: [(Int, Blueprint)] -> Int
    part1 =
      sum
        . map (uncurry (*))
        . sortOn snd
        . map (\(id, costs) -> (id, snd $ search M.empty 24 reset 0 ore costs))

    part2 :: [(Int, Blueprint)] -> Int
    part2 =
      product
        . map (\(_, costs) -> snd $ search M.empty 32 reset 0 ore costs)
        . take 3

    reset :: (Bool, Bool, Bool)
    reset = (False, False, False)

    ore :: Res
    ore = Res 1 0 0 0

    clay :: Res
    clay = Res 0 1 0 0

    obsid :: Res
    obsid = Res 0 0 1 0

    geode :: Res
    geode = Res 0 0 0 1

    search ::
      M.Map (Int, Res, Res) Int ->
      Int ->
      (Bool, Bool, Bool) ->
      Res ->
      Res ->
      Blueprint ->
      (M.Map (Int, Res, Res) Int, Int)
    search m 0 _ (Res _ _ _ g) _ _ = (m, g)
    search m0 time1 (iR, iC, iO) bank1 robots blueprint@(r, c, o, g, p) =
      case M.lookup key m0 of
        Just best0 -> (m0, best0)
        Nothing
          | g <= bank1 ->
            let (m1, best1) =
                  search m0 time2 reset (bank2 - g) (robots + geode) blueprint
             in (M.insert key best1 m1, best1)
        Nothing ->
          let (m1, best1)
                | r <= bank1 && resOre robots < resOre p && not iR =
                  search m0 time2 reset (bank2 - r) (robots + ore) blueprint
                | otherwise = (m0, 0)
              (m2, best2)
                | c <= bank1 && resClay robots < resClay p && not iC =
                  search m1 time2 reset (bank2 - c) (robots + clay) blueprint
                | otherwise = (m1, 0)
              (m3, best3)
                | o <= bank1 && resObsid robots < resObsid p && not iO =
                  search m2 time2 reset (bank2 - o) (robots + obsid) blueprint
                | otherwise = (m2, 0)
              (m4, best4) = search m3 time2 ignore bank2 robots blueprint
              best5 = maximum [best1, best2, best3, best4]
           in (M.insert key best5 m4, best5)
      where
        key = (time1, bank1, robots)
        time2 = pred time1
        bank2 = bank1 + robots
        ignore = (r <= bank1, c <= bank1, o <= bank1)
