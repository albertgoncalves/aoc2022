{-# LANGUAGE LambdaCase #-}

import Data.List (nub, sort, unfoldr)

solve :: Ord a => Int -> [a] -> Int
solve n =
  (n +)
    . length
    . unfoldr
      ( \case
          [] -> undefined
          xs ->
            if ((== n) . length) $ nub $ sort $ take n xs
              then Nothing
              else Just ((), tail xs)
      )

main :: IO ()
main = interact $ unlines . map show . zipWith solve [4, 14] . replicate 2
