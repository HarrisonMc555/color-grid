module View exposing (view)

import Array2D exposing (Array2D)
import Color exposing (Color)
import Css exposing (Style)
import Html.Styled exposing (Html, br, button, div, input, node, text)
import Html.Styled.Attributes exposing (css, href, rel, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Logic
import Model exposing (Model, Size)
import Update exposing (Msg(..))
import Util


view : Model -> Html Msg
view model =
    let
        elements =
            [ text <| "Message: \"" ++ model.message ++ "\""
            , br [] []
            , rowText model
            , addRowButton
            , removeRowButton
            , br [] []
            , columnText model
            , addColumnButton
            , removeColumnButton
            , selectColorButtonsGrid model
            , colorGrid model
            , cssLink "normalize.css"
            , cssLink "skeleton.css"
            , cssLink "custom.css"
            ]
    in
    div [] elements


grid : Array2D elem -> (Int -> Int -> elem -> Html Msg) -> Html Msg
grid array format =
    let
        autos =
            "auto"
                |> List.repeat (Array2D.columns array)
                |> String.join " "

        elements =
            Array2D.indexedMap format array |> Util.flatten
    in
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" autos
            , Css.before
                [ Css.property "content" ""
                , Css.display Css.block
                , Css.paddingBottom (Css.pct 130)
                ]
            ]
        ]
        elements


colorGrid : Model -> Html Msg
colorGrid model =
    let
        format _ _ color =
            tile model color
    in
    grid (Logic.interpolateColorGrid model.colors model.numInBetweenRows model.numInBetweenColumns) format


tile : Model -> Color -> Html Msg
tile model color =
    div
        [ css
            [ tileSize model.tileSize
            , tileColor color
            ]
        ]
        []


tileSize : Size -> Style
tileSize size =
    Css.batch
        [ Css.width (Css.px size.width)
        , Css.height (Css.px size.height)
        ]


tileColor : Color -> Style
tileColor color =
    Css.backgroundColor <| Util.cssColorFromColor color


selectColorButtonsGrid : Model -> Html Msg
selectColorButtonsGrid model =
    let
        format row column _ =
            selectColorButton model row column
    in
    grid model.colors format


selectColorButton : Model -> Int -> Int -> Html Msg
selectColorButton model row column =
    let
        firstColor =
            Array2D.get row column model.colors

        color =
            Maybe.withDefault Color.black firstColor
    in
    input
        [ type_ "color"
        , value <| Util.hexStringFromColor color
        , onInput <| colorMsg row column
        ]
        []


colorMsg : Int -> Int -> String -> Msg
colorMsg row column string =
    Css.hex string
        |> Util.colorFromCssColor
        |> NewColor row column


rowText : Model -> Html Msg
rowText model =
    model.colors
        |> Array2D.rows
        |> String.fromInt
        |> text


addRowButton : Html Msg
addRowButton =
    button [ onClick AddRow ] [ text "Add row" ]


removeRowButton : Html Msg
removeRowButton =
    button [ onClick RemoveRow ] [ text "Remove row" ]


columnText : Model -> Html Msg
columnText model =
    model.colors
        |> Array2D.columns
        |> String.fromInt
        |> text


addColumnButton : Html Msg
addColumnButton =
    button [ onClick AddColumn ] [ text "Add column" ]


removeColumnButton : Html Msg
removeColumnButton =
    button [ onClick RemoveColumn ] [ text "Remove column" ]


cssLink : String -> Html Msg
cssLink filename =
    node "link" [ rel "stylesheet", href ("../static/css/" ++ filename) ] []
