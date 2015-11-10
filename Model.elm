module Model where
import Date exposing (Date, Day)

type alias Model =
  { dateNow : Date
  , browseDate : Date
  , selectedDate : Maybe Date
  , firstDayOfWeek : Date.Day
  , showPicker : Bool
  }
