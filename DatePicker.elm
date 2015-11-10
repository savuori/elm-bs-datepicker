module DatePicker where

import Html exposing (Html, table, td, tr, th, div, text, span, thead, tbody, input)
import Html.Attributes exposing (style, src, class, colspan, type', placeholder, value)
import Html.Events exposing (onClick, onFocus)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Date exposing (Date, Day, fromTime, toTime, month, day, year, dayOfWeek)
import Signal exposing (Address)
import Date.Format exposing (format)

import Arithmetic exposing (daysToMillis, firstDayOfMonth, comparableByDay, getPreviousMonth, getNextMonth)
import View exposing (renderWidget)
import Model exposing (Model)
import Actions exposing (Action)


model : Model
model =
  let curDate = (Date.fromTime (1446974630870 - (3600 * 24 * 1000 * 0)))
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
      -> { model | showPicker <- True }
    Actions.HidePicker
      -> { model | showPicker <- False }
    Actions.NoOp
      -> model


picker : Signal.Mailbox Action
picker = Signal.mailbox Actions.NoOp


view : Address Action -> Model -> Html
view address model = lazy2 renderWidget address model


main : Signal Html
main = Signal.map (view picker.address) (Signal.foldp update model picker.signal)
