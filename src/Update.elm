module Update exposing (Msg(..), update)

import Array
import Array2D
import Color exposing (Color)
import Model exposing (Model, asColorsIn, asMessageIn)


type Msg
    = NewColor Int Int Color
    | AddRow
    | AddColumn
    | RemoveRow
    | RemoveColumn


update : Msg -> Model -> Model
update msg model =
    let
        newModel =
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
    in
    Debug.toString msg |> asMessageIn newModel


defaultColor : Color
defaultColor =
    Color.black
