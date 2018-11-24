data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify n
  | n < 1     = Nothing
  | n == 1    = Just Deficient
  | ns > n    = Just Abundant
  | ns < n    = Just Deficient
  | otherwise = Just Perfect
 where ns = sum [x | x <- [1 .. n `quot` 2], n `mod` x == 0]
