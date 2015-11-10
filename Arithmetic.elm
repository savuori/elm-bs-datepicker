module Arithmetic where
import Date exposing (Day, day, toTime, fromTime, Date, month, dayOfWeek, year)
import Time exposing (Time)

monthToInt : Date.Month -> Int
monthToInt m = case m of
    Date.Jan -> 1
    Date.Feb -> 2
    Date.Mar -> 3
    Date.Apr -> 4
    Date.May -> 5
    Date.Jun -> 6
    Date.Jul -> 7
    Date.Aug -> 8
    Date.Sep -> 9
    Date.Oct -> 10
    Date.Nov -> 11
    Date.Dec -> 12


daysOfWeek : List Day
daysOfWeek = [Date.Mon, Date.Tue, Date.Wed, Date.Thu, Date.Fri, Date.Sat, Date.Sun]


weekdayTuples : List (Int, Day)
weekdayTuples = List.map2 (,) [1..7] daysOfWeek


dayToInt : Day -> Int
dayToInt day = fst(dayIntMapper day snd)


intToDay : Int -> Day
intToDay num =
  let limitedNum = ((num - 1) % 7) + 1
  in snd(dayIntMapper limitedNum fst)


dayIntMapper : b -> ((Int, Day) -> b) -> (Int, Day)
dayIntMapper key selector =
  let f tuple = (selector tuple) == key
  in
    case List.head(List.filter f weekdayTuples) of
      Nothing
        -> (-1, Date.Mon) -- shouldn't ever happen
      Just found
        -> found


hoursInDay : Time
hoursInDay = 24.0


daysToMillis : Time -> Time
daysToMillis d = d * hoursInDay * Time.hour


firstDayOfMonth : Date -> Date
firstDayOfMonth d =
  let diffDays = (day d) - 1 |> toFloat
      diffMillis = daysToMillis diffDays
  in
      (toTime d) - diffMillis |> fromTime


daysOfTheMonth : Date -> List Date
daysOfTheMonth date =
  let possibleDays = [0 .. 30]
      timeOfFirstDay = toTime (firstDayOfMonth date)
      calculateNextDays date = fromTime(timeOfFirstDay + (daysToMillis date))
      dayFilter (index, date) = (index + 1) == (day date)
  in
      possibleDays
      |> List.map calculateNextDays
      |> List.indexedMap (,)
      |> List.filter dayFilter
      |> List.map snd


padByStartOfWeek : Date.Day -> List Date -> List (Maybe Date)
padByStartOfWeek day dateList =
  let firstDay = List.head dateList
      padding = case firstDay of
                  Nothing
                    -> 0
                  Just fd
                    -> (7 - (dayToInt day) + (dayToInt (dayOfWeek fd))) % 7
      justDays = List.map (\date -> Just date) dateList
  in
      (List.repeat padding Nothing) ++ justDays


groupByWeek : List (Maybe Date) -> List (List (Maybe Date))-> List (List (Maybe Date))
groupByWeek dayList current =
  case dayList of
    []
      -> current
    days
      -> groupByWeek (List.drop 7 days) (current ++ [List.take 7 days])


comparableByDay : Date -> Int
comparableByDay date =
  (year date) * 10000 + (monthToInt (month date)) * 100 + (day date)


getPreviousMonth : Date -> Date
getPreviousMonth date =
  fromTime ((toTime (firstDayOfMonth date)) - (daysToMillis 1))


getNextMonth : Date -> Date
getNextMonth date =
  let curDay = (day date)
      daysToForward = (32 - curDay) |> toFloat
  in
      fromTime ((toTime date) + (daysToMillis daysToForward))
