module Actions where

import Date exposing (Date)
import Time exposing (Time)

type Action = PreviousMonth | NextMonth | SelectDate Date | ShowPicker | HidePicker | SampleAndDelegate (Time, Action) | NoOp
