module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = validate $ drop1 digits
  where 
    digits = filter isDigit xs

    drop1 ('1' : digits) = digits
    drop1 digits = digits

    is29 digit = digit `elem` ['2', '3', '4', '5', '6', '7', '8', '9']

    valid [True, _, _, True, _, _, _, _, _, _] = True
    valid _ = False

    validate digits
      | valid $ map is29 digits = Just digits
      | otherwise = Nothing
