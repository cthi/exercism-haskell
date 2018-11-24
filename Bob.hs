module Bob (responseFor) where

import Data.Char(isAlpha, isSpace, isUpper)
import Data.List.Extra(trim)

responseFor :: String -> String
responseFor xs
  | isEmpty                 = "Fine. Be that way!"
  | isYelling && isQuestion = "Calm down, I know what I'm doing!"
  | isQuestion              = "Sure."
  | isYelling               = "Whoa, chill out!"
  | otherwise               = "Whatever."
  where
    isQuestion = lastMay (trim xs) == Just '?'
    isYelling  = any isAlpha xs && all upperOrNotAlpha xs
      where upperOrNotAlpha c = isUpper c || not (isAlpha c)
    isEmpty    = all isSpace xs

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [x] = Just x
lastMay (_:xs) = lastMay xs
