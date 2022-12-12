data Choice
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

intoPoints :: Choice -> Int
intoPoints Rock = 1
intoPoints Paper = 2
intoPoints Scissors = 3

score :: Choice -> Choice -> Int
score Rock player@Scissors = intoPoints player
score Scissors player@Paper = intoPoints player
score Paper player@Rock = intoPoints player
score opponent player
  | opponent == player = 3 + intoPoints player
  | otherwise = 6 + intoPoints player

part1 :: String -> (Choice, Choice)
part1 [opponent, player] =
  ( case opponent of
      'A' -> Rock
      'B' -> Paper
      'C' -> Scissors
      _ -> undefined,
    case player of
      'X' -> Rock
      'Y' -> Paper
      'Z' -> Scissors
      _ -> undefined
  )
part1 _ = undefined

lose :: Choice -> Choice
lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper

win :: Choice -> Choice
win Rock = Paper
win Paper = Scissors
win Scissors = Rock

part2 :: String -> (Choice, Choice)
part2 [opponent, player] =
  case opponent of
    'A' -> (Rock, f player Rock)
    'B' -> (Paper, f player Paper)
    'C' -> (Scissors, f player Scissors)
    _ -> undefined
  where
    f 'X' = lose
    f 'Y' = id
    f 'Z' = win
    f _ = undefined
part2 _ = undefined

main :: IO ()
main =
  interact $
    unlines
      . map show
      . zipWith ((sum .) . map . (uncurry score .)) [part1, part2]
      . replicate 2
      . map (map head . words)
      . lines
