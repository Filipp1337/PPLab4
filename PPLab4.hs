data DayOfWeek
    = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Enum, Bounded)

data Month
    = January | February | March     | April   | May      | June
    | July    | August   | September | October | November | December
    deriving (Enum, Bounded, Show)

next :: (Eq a, Enum a, Bounded a) => a -> a
next x | x == maxBound = minBound
       | otherwise     = succ x

pad :: Int -> String
pad day = case show day of
    [c] -> [' ', c]
    cs  -> cs

month :: Month -> DayOfWeek -> Int -> String
month m startDay maxDay = show m ++ " 2022\n" ++ week ++ spaces Monday
  where
    week = "Mo Tu We Th Fr Sa Su\n"

    spaces currDay | startDay == currDay = days startDay 1
                   | otherwise           = "   " ++ spaces (next currDay)

    days Monday    n | n > maxDay = "\n"
    days _         n | n > maxDay = "\n\n"
    days Sunday  n              = pad n ++ "\n" ++ days  Monday    (succ n)
    days day       n              = pad n ++ " "  ++ days (next day) (succ n)

year = month January   Saturday      31
    ++ month February  Tuesday       28
    ++ month March     Tuesday       31
    ++ month April     Friday        30
    ++ month May       Sunday        31
    ++ month June      Wednesday     30
    ++ month July      Friday        31
    ++ month August    Monday        31
    ++ month September Thursday      30
    ++ month October   Saturday      31
    ++ month November  Tuesday       30
    ++ month December  Thursday      31

main = putStr year
