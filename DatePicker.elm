module DatePicker where

import Html exposing (Html, table, td, tr, th, div, text, span, thead, tbody, input)
import Html.Attributes exposing (style, src, class, colspan, type', placeholder, value)
import Html.Events exposing (onClick, onFocus)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Date exposing (Date, Day, fromTime, toTime, month, day, year, dayOfWeek)
import Time exposing (Time, hour, second)
import Signal exposing (Address)
import Date.Format exposing (format)


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

localizedMonth : String -> Date.Month -> String
localizedMonth locale m =
  case locale of
    "fi_FI" ->
      case m of
        Date.Jan -> "Tammikuu"
        Date.Feb -> "Helmikuu"
        Date.Mar -> "Maaliskuu"
        Date.Apr -> "Huhtikuu"
        Date.May -> "Toukokuu"
        Date.Jun -> "Kesäkuu"
        Date.Jul -> "Heinäkuu"
        Date.Aug -> "Elokuu"
        Date.Sep -> "Syyskuu"
        Date.Oct -> "Lokakuu"
        Date.Nov -> "Marraskuu"
        Date.Dec -> "Joulukuu"


localizedWeekday : String -> Date.Day -> String
localizedWeekday locale d =
  case locale of
    "fi_FI" ->
      case d of
        Date.Mon -> "Ma"
        Date.Tue -> "Ti"
        Date.Wed -> "Ke"
        Date.Thu -> "To"
        Date.Fri -> "Pe"
        Date.Sat -> "La"
        Date.Sun -> "Su"


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


renderDay : Address Action -> Model -> Maybe Date -> Html
renderDay address model date =
  case date of
    Nothing
      -> td [] []
    Just d
      ->
       let currentDay = model.dateNow |> comparableByDay
           selectedDay = model.selectedDate |> Maybe.withDefault model.dateNow |> comparableByDay
           renderedDay = d |> day
           classes = if | selectedDay == (comparableByDay d) -> "day active"
                        | otherwise -> "day"
       in
           td [ onClick address (SelectDate d)
              , class classes
              ]
              [renderedDay |> toString |> text]


renderRow : Address Action -> Model -> List (Maybe Date) -> Html
renderRow address model dayList =
  tr [] (List.map (renderDay address model) dayList)


renderHeaderRow : Day -> Html
renderHeaderRow startOfWeek =
  let days = daysOfWeek ++ daysOfWeek
      startIndex = dayToInt startOfWeek
      adjustedDays = days |> (List.drop (startIndex - 1)) |> (List.take 7)
      localizedDays = List.map (localizedWeekday "fi_FI") adjustedDays
      renderDay d = th [class "dow"] [text d]
  in
      tr []
        (List.map renderDay localizedDays)


renderTable : Address Action -> Model -> List (Maybe Date) -> Html
renderTable address model dayList =
  let daysByWeek = groupByWeek dayList []
  in
      tbody []
        ([renderHeaderRow model.firstDayOfWeek] ++
        (List.map (renderRow address model) daysByWeek))


renderBody : Address Action -> Model -> Html
renderBody address model =
  let allDays = daysOfTheMonth model.browseDate
      paddedList = padByStartOfWeek model.firstDayOfWeek allDays
  in
      renderTable address model paddedList


renderCurrentDate : Model -> Html
renderCurrentDate model =
  let y = (year model.browseDate)
      m = (localizedMonth "fi_FI" (month model.browseDate))
  in
      text (m ++ " " ++ (toString y))


renderHeader : Address Action -> Model -> Html
renderHeader address model =
  thead []
   [
    tr []
     [ th [class "prev", onClick address PreviousMonth] [text "«"]
     , th [class "datepicker-switch", colspan 5]
         [renderCurrentDate model]
     , th [class "next", onClick address NextMonth] [text "»"]
     ]
   ]

renderCalendar : Address Action -> Model -> Html
renderCalendar address model =
  let disp = if model.showPicker then "block" else "none"
  in
    div [ class "datepicker datepicker-dropdown dropdown-menu datepicker-orient-left datepicker-orient-top"
        , style [ ("display", disp)
                , ("top", "25px")
              --  , ("left", "120px")
                ]
        ]
      [ table [class "table-condensed"]
        [ renderHeader address model
        , renderBody address model
        ]
      ]

renderInput : Address Action -> Model -> Html
renderInput address model =
  let val = case model.selectedDate of
              Nothing
                -> []
              Just d
                -> [value (format "%d.%m.%Y" d)]
  in
    div [class "hero-unit"]
      [
        input ([ type' "text"
              , placeholder "click to pick a date"
              , onFocus address ShowPicker
              ] ++ val)
              []
      ]


renderWidget : Address Action -> Model -> Html
renderWidget address model =
  div [class "container", style [("position", "relative")]]
    [ renderInput address model
    , renderCalendar address model
    ]


type alias Model =
  { dateNow : Date
  , browseDate : Date
  , selectedDate : Maybe Date
  , firstDayOfWeek : Date.Day
  , showPicker : Bool
  }


getPreviousMonth : Date -> Date
getPreviousMonth date =
  fromTime ((toTime (firstDayOfMonth date)) - (daysToMillis 1))


getNextMonth : Date -> Date
getNextMonth date =
  let curDay = (day date)
      daysToForward = (32 - curDay) |> toFloat
  in
      fromTime ((toTime date) + (daysToMillis daysToForward))


model : Model
model =
  let curDate = (Date.fromTime (1446974630870 - (3600 * 24 * 1000 * 0)))
  in
    Model curDate curDate Nothing Date.Mon False


type Action = PreviousMonth | NextMonth | SelectDate Date | ShowPicker | HidePicker | NoOp

update : Action -> Model -> Model
update action model =
  case action of
    PreviousMonth
      -> { model | browseDate <- getPreviousMonth model.browseDate}
    NextMonth
      -> { model | browseDate <- getNextMonth model.browseDate}
    SelectDate date
      -> { model | selectedDate <- Just date, showPicker <- False}
    ShowPicker
      -> { model | showPicker <- True }
    HidePicker
      -> { model | showPicker <- False }
    NoOp
      -> model


picker : Signal.Mailbox Action
picker = Signal.mailbox NoOp


view : Address Action -> Model -> Html
view address model = lazy2 renderWidget address model


main : Signal Html
main = Signal.map (view picker.address) (Signal.foldp update model picker.signal)
