module DNA (toRNA) where

toRNA :: String -> Either Char String 
toRNA = mapM rnaComplement
  where
    rnaComplement 'C' = Right 'G'
    rnaComplement 'G' = Right 'C'
    rnaComplement 'T' = Right 'A'
    rnaComplement 'A' = Right 'U'
    rnaComplement x   = Left x
