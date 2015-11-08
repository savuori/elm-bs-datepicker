module DatePicker where

import Html exposing (Html, table, td, tr, th, div, text)
import Date exposing (Date, Day, fromTime, toTime, month, day, dayOfWeek)
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

dayToInt : Date.Day -> Int
dayToInt d = case d of
    Date.Mon -> 1
    Date.Tue -> 2
    Date.Wed -> 3
    Date.Thu -> 4
    Date.Fri -> 5
    Date.Sat -> 6
    Date.Sun -> 7

hInDay : Time
hInDay = 24.0

secInHour : Time
secInHour = 3600.0

daysToMillis : Time -> Time
daysToMillis d = d * hInDay * secInHour * 1000

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
                    -> 7 + (dayToInt day) - (dayToInt (dayOfWeek fd))
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

renderDay : Maybe Date -> Html
renderDay date =
  case date of
    Nothing
      -> td [] []
    Just d
      -> td [] [d |> day |> toString |> text]

renderRow : List (Maybe Date) -> Html
renderRow dayList =
  tr [] (List.map renderDay dayList)

renderTable : List (Maybe Date) -> Html
renderTable dayList =
  let daysByWeek = groupByWeek dayList []
  in
      table [] (List.map renderRow daysByWeek)


view : Date -> Html
view date = div [] [(daysOfTheMonth date) |> List.map day |> toString |> text
                   , (text (toString (month date)))]

main : Html
main = view (Date.fromTime 1424234330266)
