module Localized where

import Date exposing (Date)

month : String -> Date.Month -> String
month locale m =
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
    "en_US" ->
      case m of
        Date.Jan -> "January"
        Date.Feb -> "February"
        Date.Mar -> "March"
        Date.Apr -> "April"
        Date.May -> "May"
        Date.Jun -> "June"
        Date.Jul -> "July"
        Date.Aug -> "August"
        Date.Sep -> "September"
        Date.Oct -> "October"
        Date.Nov -> "November"
        Date.Dec -> "December"

weekday : String -> Date.Day -> String
weekday locale d =
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
    "en_US" ->
      case d of
        Date.Mon -> "Mo"
        Date.Tue -> "Tu"
        Date.Wed -> "We"
        Date.Thu -> "Th"
        Date.Fri -> "Fr"
        Date.Sat -> "Sa"
        Date.Sun -> "Su"
