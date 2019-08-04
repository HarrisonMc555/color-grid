import Browser
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (..)
import Array2D exposing (Array2D)
import Css
import Css exposing (Style, Color)
import Hex
import List.Extra


main =
  Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


-- MODEL

type alias Model =
    { colors : Array2D Color
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
    { colors = Array2D.fromList
          [ [ Css.hex "777777"
            , Css.hex "0000ff"
            ]
          , [ Css.hex "00ff00"
            , Css.hex "ff0000"
            ]
          ]
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
          model
          -- color
          --     |> asColorIn model

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
    let elements = selectColorButtons model ++
                   [ rowText model
                   , addRowButton
                   , columnText model
                   , addColumnButton
                   , colorGrid model
                   , cssLink "normalize.css"
                   , cssLink "skeleton.css"
                   ]
    in div [] elements

grid : Array2D elem -> (elem -> Html Msg) -> Html Msg
grid array format =
    div [ css [ Css.property "display" "grid"
              , Css.property "grid-template-columns"
                  (String.join " "
                       (List.repeat (Array2D.columns array) "auto"))
              , Css.before [ Css.property "content" ""
                           , Css.display Css.block
                           , Css.paddingBottom (Css.pct 130)
                           ]
              ]
        ] (List.map format (flatten array))

colorGrid : Model -> Html Msg
colorGrid model =
    grid model.colors (tile model)

tile : Model -> Color -> Html Msg
tile model color =
    div [ css [ tileSize model.tileSize
              , tileColor color
              ]
        ] []

tileSize : Size -> Style
tileSize size =
    Css.batch [ Css.width (Css.px size.width)
              , Css.height (Css.px size.height)
              ]

tileColor : Color -> Style
tileColor color =
    Css.backgroundColor color

selectColorButtons : Model -> List (Html Msg)
selectColorButtons model =
    List.repeat (numTiles model) (selectColorButton model)

selectColorButton : Model -> Html Msg
selectColorButton model =
    let firstColor = Array2D.get 0 0 model.colors
        color = Maybe.withDefault (Css.hex "000000") firstColor
    in input [ type_ "color"
             , value (fromColor color)
             , onInput colorMsg
             ] []

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

cssLink : String -> Html Msg
cssLink filename =
    node "link" [ rel "stylesheet", href ("../static/css/" ++ filename) ] []


-- HELPERS

numTiles : Model -> Int
numTiles model =
    let d = model.gridDimensions
    in d.numRows * d.numColumns

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

-- setColor : Color -> Model -> Model
-- setColor color model =
--     { model | color = color }

-- asColorIn : Model -> Color -> Model
-- asColorIn =
--     flip setColor

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

flatten : Array2D item -> List item
flatten array =
    let rows = List.range 0 (Array2D.rows array - 1)
        columns = List.range 0 (Array2D.columns array - 1)
        indices = cartesian rows columns
        get (i, j) = Array2D.get i j array
    in List.filterMap get indices

cartesian : List a -> List b -> List (a, b)
cartesian xs ys =
  List.Extra.lift2 tuple xs ys

tuple : a -> b -> (a, b)
tuple a b = (a, b)
