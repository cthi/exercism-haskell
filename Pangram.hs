module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.Set (Set)
import qualified Data.Set as Set

isPangram :: String -> Bool
isPangram text = lowerAlpha == alphabet 
  where
    lowerAlpha = Set.fromList $ map toLower $ filter isAlpha text
    alphabet = Set.fromList ['a' .. 'z']
