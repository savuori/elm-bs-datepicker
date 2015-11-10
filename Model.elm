module Model where
import Date exposing (Date, Day)

type alias Model =
  { currentDate : Date
  , browseDate : Date
  , selectedDate : Maybe Date
  , firstDayOfWeek : Date.Day
  , showPicker : Bool
  }
