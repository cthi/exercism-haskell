module SumOfMultiples (sumOfMultiples) where

import Data.Set (Set)
import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] limit = 0
sumOfMultiples factors limit = sum multiples
  where 
    positiveFactors = filter (> 0) factors
    factorMultiples x = [x, x + x..limit-1]
    multiples = Set.fromList $ concat $ map factorMultiples positiveFactors 
