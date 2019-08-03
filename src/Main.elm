-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (..)
import Array2D exposing (Array2D)
import Css
import Css exposing (Style, Color)
import Hex


main =
  Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


-- MODEL

type alias Model =
    -- { colors : Array2D Color
    { color : Color
    , gridDimensions : Dimensions
    , tileSize : Size
    }

type alias Dimensions =
    { numRows : Int
    , numColumns : Int
    }

type alias Size =
    { width : Length
    , height : Length
    }

type alias Length = Float

init : Model
init =
    -- { colors = Array2D.empty
    { color = Css.hex "60c71c"
    , gridDimensions = initGridDimensions
    , tileSize = initTileSize
    }

initGridDimensions : Dimensions
initGridDimensions =
    { numRows = 1
    , numColumns = 1
    }

initTileSize : Size
initTileSize =
    { width = 100
    , height = 100
    }


-- UPDATE

type Msg
    = NewColor Color
    | AddRow
    | AddColumn

update : Msg -> Model -> Model
update msg model =
  case msg of
      NewColor color ->
          color
              |> asColorIn model

      AddRow ->
          model.gridDimensions.numRows + 1
              |> asNumRowsIn model.gridDimensions
              |> asGridDimensionsIn model

      AddColumn ->
          model.gridDimensions.numColumns + 1
              |> asNumColumnsIn model.gridDimensions
              |> asGridDimensionsIn model


-- VIEW

view : Model -> Html Msg
view model =
    div [] [ selectColorButton model
           , rowText model
           , addRowButton
           , columnText model
           , addColumnButton
           , grid model
           ]

grid : Model -> Html Msg
grid model =
    let numColumns = model.gridDimensions.numColumns
    in div [ css [ Css.property "display" "grid"
                 , Css.property "grid-template-columns"
                     (String.join " " (List.repeat numColumns "auto"))
                 , Css.before [ Css.property "content" ""
                              , Css.display Css.block
                              , Css.paddingBottom (Css.pct 130)
                              ]
                 ]
           ] (tiles model)

tiles : Model -> List (Html Msg)
tiles model =
    let tile = div [ css [ tileSize model.tileSize
                         , tileColor model.color
                         ]
                   ]
               []
        numTiles = model.gridDimensions.numRows *
                   model.gridDimensions.numColumns
    in List.repeat numTiles tile

tileSize : Size -> Style
tileSize size =
    Css.batch [ Css.width (Css.px size.width)
              , Css.height (Css.px size.height)
              ]

tileColor : Color -> Style
tileColor color =
    Css.backgroundColor color

selectColorButton : Model -> Html Msg
selectColorButton model =
    input [ type_ "color"
          , value (fromColor model.color)
          , onInput colorMsg
          ]
        []

colorMsg : String -> Msg
colorMsg string =
    NewColor (Css.hex string)

rowText : Model -> Html Msg
rowText model =
    text (String.fromInt model.gridDimensions.numRows)

addRowButton : Html Msg
addRowButton =
    button [ onClick AddRow ] [ text "Add row" ]

columnText : Model -> Html Msg
columnText model =
    text (String.fromInt model.gridDimensions.numColumns)

addColumnButton : Html Msg
addColumnButton =
    button [ onClick AddColumn ] [ text "Add column" ]


-- HELPERS

setWidth : Length -> Size -> Size
setWidth width length =
    { length | width = width }

setHeight : Length -> Size -> Size
setHeight height length =
    { length | height = height }

asWidthIn : Size -> Length -> Size
asWidthIn =
    flip setWidth

asHeightIn : Size -> Length -> Size
asHeightIn =
    flip setHeight

setSize : Size -> Model -> Model
setSize size model =
    { model | tileSize = size }

asSizeIn : Model -> Size -> Model
asSizeIn =
    flip setSize

setColor : Color -> Model -> Model
setColor color model =
    { model | color = color }

asColorIn : Model -> Color -> Model
asColorIn =
    flip setColor

setNumRows : Int -> Dimensions -> Dimensions
setNumRows numRows dimensions
    = { dimensions | numRows = numRows }

asNumRowsIn : Dimensions -> Int -> Dimensions
asNumRowsIn =
    flip setNumRows

setNumColumns : Int -> Dimensions -> Dimensions
setNumColumns numColumns dimensions
    = { dimensions | numColumns = numColumns }

asNumColumnsIn : Dimensions -> Int -> Dimensions
asNumColumnsIn =
    flip setNumColumns

setGridDimensions : Dimensions -> Model -> Model
setGridDimensions dimensions model =
    { model | gridDimensions = dimensions }

asGridDimensionsIn : Model -> Dimensions -> Model
asGridDimensionsIn =
    flip setGridDimensions

flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a

fromColor : Color -> String
fromColor color =
    let numbers =
            [ color.red
            , color.green
            , color.blue
            ]
    in "#" ++ String.concat (List.map hexStr numbers)

hexStr : Int -> String
hexStr number =
    String.pad 2 '0' (Hex.toString number)
