module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map line [start..stop]
  where
    line day =
      "On the "
      ++ dayStr day
      ++ " day of Christmas my true love gave to me: "
      ++ (concat $ drop (12 - day) presents)
    dayStr :: Int -> String
    dayStr 1 = "first"
    dayStr 2 = "second"
    dayStr 3 = "third"
    dayStr 4 = "fourth"
    dayStr 5 = "fifth"
    dayStr 6 = "sixth"
    dayStr 7 = "seventh"
    dayStr 8 = "eighth"
    dayStr 9 = "ninth"
    dayStr 10 = "tenth"
    dayStr 11 = "eleventh"
    dayStr 12 = "twelfth"
    presents = [ "twelve Drummers Drumming, " 
               , "eleven Pipers Piping, "
               , "ten Lords-a-Leaping, "
               , "nine Ladies Dancing, "
               , "eight Maids-a-Milking, "
               , "seven Swans-a-Swimming, "
               , "six Geese-a-Laying, "
               , "five Gold Rings, "
               , "four Calling Birds, "
               , "three French Hens, "
               , "two Turtle Doves, and "
               , "a Partridge in a Pear Tree." ]
