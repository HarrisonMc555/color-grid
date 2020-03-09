module Update exposing (..)

import Array
import Array2D exposing (Array2D)
import Browser
import Css exposing (Color, Style)
import Hex
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import List.Extra
import Model exposing (..)
import Util exposing (..)


type Msg
    = NewColor Int Int Color
    | AddRow
    | AddColumn
    | RemoveRow
    | RemoveColumn


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewColor row column color ->
            Array2D.set row column color model.colors
                |> asColorsIn model

        AddRow ->
            Array2D.appendRow Array.empty defaultColor model.colors
                |> asColorsIn model

        AddColumn ->
            Array2D.appendColumn Array.empty defaultColor model.colors
                |> asColorsIn model

        RemoveRow ->
            Array2D.deleteRow (Array2D.rows model.colors - 1) model.colors
                |> asColorsIn model

        RemoveColumn ->
            Array2D.deleteColumn (Array2D.columns model.colors - 1) model.colors
                |> asColorsIn model


defaultColor : Color
defaultColor =
    Css.hex "000000"
