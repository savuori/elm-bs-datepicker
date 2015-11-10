module Actions where
import Date exposing (Date)

type Action = PreviousMonth | NextMonth | SelectDate Date | ShowPicker | HidePicker | NoOp
