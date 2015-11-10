module DatePicker where

import Html exposing (Html, table, td, tr, th, div, text, span, thead, tbody, input)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Date exposing (Date, Day, fromTime, toTime, month, day, year, dayOfWeek)
import Signal exposing (Address)
import Time exposing (hour, second, minute, Time)

import Arithmetic exposing (daysToMillis, firstDayOfMonth, comparableByDay, getPreviousMonth, getNextMonth)
import View exposing (renderWidget)
import Model exposing (Model)
import Actions exposing (Action)


model : Model
model =
  let curDate = (Date.fromTime 0)
  in
    Model curDate curDate Nothing Date.Mon False


update : Action -> Model -> Model
update action model =
  case action of
    Actions.PreviousMonth
      -> { model | browseDate <- getPreviousMonth model.browseDate}
    Actions.NextMonth
      -> { model | browseDate <- getNextMonth model.browseDate}
    Actions.SelectDate date
      -> { model | selectedDate <- Just date, showPicker <- False}
    Actions.ShowPicker
      -> { model | showPicker <- True, browseDate <- (Maybe.withDefault model.currentDate model.selectedDate) }
    Actions.HidePicker
      -> { model | showPicker <- False }
    Actions.SetCurrentTime time
      -> { model | currentDate <- (fromTime time)}
    Actions.NoOp
      -> model


picker : Signal.Mailbox Action
picker = Signal.mailbox Actions.NoOp


view : Address Action -> Model -> Html
view address model = lazy2 renderWidget address model


signals : Signal Action
signals = Signal.merge picker.signal (Signal.map Actions.SetCurrentTime (Time.every second))


main : Signal Html
main = Signal.map (view picker.address) (Signal.foldp update model signals)
