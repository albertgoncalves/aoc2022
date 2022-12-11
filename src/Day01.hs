{-# LANGUAGE TypeApplications #-}

import Data.List (groupBy, sortBy)

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith ((sum .) . take) [1, 3]
      . replicate 2
      . sortBy (flip compare)
      . map (sum . map (read @Int) . filter (not . null))
      . groupBy (const $ not . null)
      . lines
