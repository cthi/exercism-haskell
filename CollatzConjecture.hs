module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz i
  | i < 1     = Nothing
  | otherwise = Just $ fromIntegral $ length collatzList
    where
      collatzList = takeWhile (/= 1) $ iterate next i
      next n
        | even n    = n `quot` 2
        | otherwise = 3 * n + 1
