--Counting Sundays
--Problem 19
--
--You are given the following information, but you may prefer to do some research for yourself.
--
--    1 Jan 1900 was a Monday.
--    Thirty days has September,
--    April, June and November.
--    All the rest have thirty-one,
--    Saving February alone,
--    Which has twenty-eight, rain or shine.
--    And on leap years, twenty-nine.
--    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
--
--How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

import           EulerFunctions

main = print $ nextDayRepeat [dayOfWeekOnFirstDay,1,1,1901,0]
    where
        -- Day of the week on 1 Jan 1901; can be found by applying nextDay 365 times to the state of 1 Jan 1900 and extracting the day of the week.
        dayOfWeekOnFirstDay = (applyTimes nextDay [1,1,1,1900,0] 365) !! 0

-- Function that takes current day state and returns next day state
-- dweek is day of week (1 to 7), dmonth is day of month (1 to 28, 29, 30, or 31 depending on the month and leapyear status), month is the month (1 to 12), year is self-evident, and count increases by 1 if the input state is a sunday on the first of the month.
nextDay [dweek, dmonth, month, year, count] =
    [(mod dweek 7) + 1, (mod dmonth monthMod) + 1, checkMonthDay, checkEndYear, checkCount]
    where
        monthMod
            | elem month [9,4,6,11]        = 30
            | month == 2 && leapYear       = 29
            | month == 2 && (not leapYear) = 28
            | otherwise                    = 31
        leapYear
            | mod year 400 == 0            = True
            | mod year 100 == 0            = False
            | mod year 4   == 0            = True
            | otherwise                    = False
        checkMonthDay
            | dmonth == monthMod           = (mod month 12) + 1
            | otherwise                    = month
        checkEndYear
            | month == 12 && dmonth == 31  = year + 1
            | otherwise                    = year
        -- Adds 1 to count if it's a Sunday on the first of the month
        checkCount
            -- day of the week index starts at 0 (monday), so sunday is 6
            | dweek == 7 && dmonth == 1    = count + 1
            | otherwise                    = count

nextDayRepeat input
    | input !! 1 == 31 && input !! 2 == 12 && input !! 3 == 2000 = last input
    | otherwise = nextDayRepeat (nextDay input)
