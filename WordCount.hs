module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.Map (Map, empty, insert, lookup)
import qualified Data.Map as Map

wordCount :: String -> Map String Int
wordCount xs = foldr addWord Map.empty $ map dropSurroundingQuote (words nopunc)
  where 
    stripSpecialChars a = if isAlphaNum a || a == '\'' then a else ' '

    nopunc = map (toLower . stripSpecialChars) xs 

    dropSurroundingQuote ('\'':rest) = dropQuoteEnd rest
    dropSurroundingQuote x = x

    dropQuoteEnd ('\'':[]) = []
    dropQuoteEnd (x:[]) = (x:[])
    dropQuoteEnd (x:xs) = (x:dropQuoteEnd xs)

addWord :: String -> Map String Int -> Map String Int
addWord word oldMap = Map.insert word (1 + current) oldMap
  where current = Map.findWithDefault 0 word oldMap
