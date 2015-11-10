module DatePicker where

import Html exposing (Html, table, td, tr, th, div, text, thead, tbody, input)
import Html.Lazy exposing (lazy2)
import Html.Events exposing (onClick, onFocus)
import Html.Attributes exposing (class, colspan, style, value, type', placeholder)
import Date exposing (Date, Day, day, year, month, fromTime)
import Signal exposing (Address)
import Time exposing (Time, timestamp)
import Date.Format exposing (format)

import Localized

import Arithmetic exposing ( dayToInt
                           , daysOfWeek
                           , comparableByDay
                           , groupByWeek
                           , daysOfTheMonth
                           , padByStartOfWeek
                           , getNextMonth
                           , getPreviousMonth )


--- VIEW ---

renderDay : Address Action -> Model -> Maybe Date -> Html
renderDay address model date =
  case date of
    Nothing
      -> td [] []
    Just d
      ->
       let currentDay = model.currentDate |> comparableByDay
           selectedDay = model.selectedDate |> Maybe.withDefault model.currentDate |> comparableByDay
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
      localizedDays = List.map (Localized.weekday "en_US") adjustedDays
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
      m = (Localized.month "en_US" (month model.browseDate))
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
                -> [value (format "%m/%d/%Y" d)]
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

view : Address Action -> Model -> Html
view address model = lazy2 renderWidget address model

--- MODEL ---

type alias Model =
  { currentDate : Date
  , browseDate : Date
  , selectedDate : Maybe Date
  , firstDayOfWeek : Date.Day
  , showPicker : Bool
  }

model : Model
model =
  let curDate = (Date.fromTime 0)
  in
    Model curDate curDate Nothing Date.Mon False

--- UPDATE ---

type Action = PreviousMonth | NextMonth | SelectDate Date | ShowPicker | HidePicker | SampleAndDelegate (Time, Action) | NoOp


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
      -> { model | showPicker <- True, browseDate <- (Maybe.withDefault model.currentDate model.selectedDate) }
    HidePicker
      -> { model | showPicker <- False }
    SampleAndDelegate (time, act)
      -> update act { model | currentDate <- (fromTime time)} -- sample current time and do the action
    NoOp
      -> model


picker : Signal.Mailbox Action
picker = Signal.mailbox NoOp


signals : Signal Action
signals =  Signal.map SampleAndDelegate (timestamp picker.signal)


main : Signal Html
main = Signal.map (view picker.address) (Signal.foldp update model signals)
