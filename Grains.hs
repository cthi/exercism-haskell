module Grains (square, total) where

import Control.Applicative(Alternative, empty)

square :: (Monad m, Alternative m) => Integer -> m Integer
square n
  | n < 1 = empty
  | n > 64 = empty
  | otherwise = return $ 2 ^ (n - 1)

-- total :: Integer
-- total = 2 ^ 64 - 1

total :: Integer
total = sum $ concatMap square [1..64]
