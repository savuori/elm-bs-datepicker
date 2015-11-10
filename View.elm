module View where


import Html exposing (Html, td, th, tr, text, table, tbody, thead, div, input)
import Html.Attributes exposing (class, colspan, style, value, type', placeholder)
import Html.Events exposing (onClick, onFocus)
import Date exposing (day, Date, Day, year, month)
import Signal exposing (Address)
import Date.Format exposing (format)

import Arithmetic exposing ( comparableByDay
                           , daysOfWeek
                           , dayToInt
                           , localizedWeekday
                           , localizedMonth
                           , groupByWeek
                           , daysOfTheMonth
                           , padByStartOfWeek)
import Actions exposing (Action)
import Model exposing (Model)


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
           td [ onClick address (Actions.SelectDate d)
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
      localizedDays = List.map (localizedWeekday "en_US") adjustedDays
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
      m = (localizedMonth "en_US" (month model.browseDate))
  in
      text (m ++ " " ++ (toString y))


renderHeader : Address Action -> Model -> Html
renderHeader address model =
  thead []
   [
    tr []
     [ th [class "prev", onClick address Actions.PreviousMonth] [text "«"]
     , th [class "datepicker-switch", colspan 5]
         [renderCurrentDate model]
     , th [class "next", onClick address Actions.NextMonth] [text "»"]
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
              , onFocus address Actions.ShowPicker
              ] ++ val)
              []
      ]


renderWidget : Address Action -> Model -> Html
renderWidget address model =
  div [class "container", style [("position", "relative")]]
    [ renderInput address model
    , renderCalendar address model
    ]
