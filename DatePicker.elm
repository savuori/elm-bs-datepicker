module DatePicker where

import Html exposing (Html, table, td, tr, th, div, text, span, h2, img)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick)
import Date exposing (Date, Day, fromTime, toTime, month, day, year, dayOfWeek)
import Time exposing (Time)
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

localizedMonth : Date.Month -> String -> String
localizedMonth m locale =
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


daysOfTheWeek : List Day
daysOfTheWeek = [Date.Mon, Date.Tue, Date.Wed, Date.Thu, Date.Fri, Date.Sat, Date.Sun]

weekdayTuples : List (Int, Day)
weekdayTuples = List.map2 (,) [1..7] daysOfTheWeek

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
      Just tuple
        -> tuple


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
           renderedDay = d |> day
           bgColor = if currentDay == (comparableByDay d) then "#90A0E0" else "#D5E5F5"
       in
           td [ (style [("text-align", "right")
                     ,("padding", "3px 5px")
                     ,("background-color", bgColor)
                     ])
              , onClick address (SelectDate d)
              ]
              [d |> day |> toString |> text]


renderRow : Address Action -> Model -> List (Maybe Date) -> Html
renderRow address model dayList =
  tr [] (List.map (renderDay address model) dayList)


renderTable : Address Action -> Model -> List (Maybe Date) -> Html
renderTable address model dayList =
  let daysByWeek = groupByWeek dayList []
  in
      table [style [("padding-top", "0.5em")]]
         (List.map (renderRow address model) daysByWeek)

renderTableHeader : Address Action -> Model -> Html
renderTableHeader address model =
  div [style [("width", "4em"), ("margin-left", "auto"), ("margin-right", "auto"), ("position", "relative"), ("padding-bottom", "1em")]]
    [div []
      [ (h2 [style [("padding-left", "0.3em")]] [(model.browseDate) |> year |> toString |> text])
      , div []
         [ (img [onClick address PreviousMonth, style [("position", "absolute"), ("left", "-2em"), ("bottom", "0"), ("width", "24px")], src "arrowleft.png"] [])
         , (span [style [("text-align", "center"), ("position", "absolute"), ("bottom", "0.1em")]] [text (localizedMonth (month model.browseDate) "fi_FI")])
         , (img [onClick address NextMonth, style [("position", "absolute"), ("right", "-3em"), ("bottom", "0"), ("width", "24px") ], src "arrowright.png"] [])
         ]
      ]
    ]

renderSelected: Model -> Html
renderSelected model =
  let selectedText =
    case model.selectedDate of

      Nothing
        -> "Ei valittu"
      Just date
        -> (format "%d.%m.%Y" date)
  in
    div []
      [text ("Valittu päivä:" ++ selectedText)]

renderPicker : Address Action -> Model -> Day -> Html
renderPicker address model firstDayOfWeek =
  let allDays = daysOfTheMonth model.browseDate
      paddedList = padByStartOfWeek firstDayOfWeek allDays
  in
      div [style [("width", "12em")]]
        [ renderSelected model
        , renderTableHeader address model
        , renderTable address model paddedList
        ]

type Action = PreviousMonth | NextMonth | SelectDate Date | NoOp


type alias Model =
  { dateNow : Date
  , browseDate : Date
  , selectedDate : Maybe Date
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
    Model curDate curDate Nothing False

update : Action -> Model -> Model
update action model =
  case action of
    PreviousMonth
      -> { model | browseDate <- getPreviousMonth model.browseDate}
    NextMonth
      -> { model | browseDate <- getNextMonth model.browseDate}
    SelectDate date
      -> { model | selectedDate <- Just date}
    NoOp
      -> model

picker : Signal.Mailbox Action
picker = Signal.mailbox NoOp


view : Address Action -> Model -> Html
view address model = renderPicker address model Date.Mon


main : Signal Html
main = Signal.map (view picker.address) (Signal.foldp update model picker.signal)
