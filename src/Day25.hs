decimal :: Char -> Int
decimal '2' = 2
decimal '1' = 1
decimal '0' = 0
decimal '-' = -1
decimal '=' = -2
decimal _ = undefined

decode :: String -> Int
decode digits =
  sum $
    zipWith (*) (map decimal digits) $
      map (5 ^) $
        reverse [0 .. pred $ length digits]

root :: Int -> Int -> Int
root n k
  | k < 0 = root n $ abs k
  | d == 0 = n
  | otherwise = root (succ n) d
  where
    d = k `div` 5

encode :: Int -> Int -> String
encode 0 _ = []
encode r0 n = c : encode r1 (n - (k * m))
  where
    r1 = pred r0
    k :: Int
    k = 5 ^ r1
    f
      | n < 0 =
          last . filter ((<= n) . (\x -> (x * k) - (k `div` 2)) . snd)
      | otherwise =
          head . filter ((n <=) . (\x -> (x * k) + (k `div` 2)) . snd)
    (c, m) = f $ zip "=-012" [-2, -1, 0, 1, 2]

main :: IO ()
main =
  interact $
    show
      . (\x -> encode (succ $ root 0 x) x)
      . sum
      . map decode
      . lines
