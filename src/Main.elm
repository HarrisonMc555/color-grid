import Browser
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (..)
import Array2D exposing (Array2D)
import Css
import Css exposing (Style, Color)
import Hex
import List.Extra
import Array


main =
  Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


-- MODEL

type alias Model =
    { colors : Array2D Color
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
    , tileSize = initTileSize
    }

initTileSize : Size
initTileSize =
    { width = 100
    , height = 100
    }


-- UPDATE

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

-- VIEW

view : Model -> Html Msg
view model =
    let elements = [ rowText model
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
                   ]
    in div [] elements

grid : Array2D elem -> (Int -> Int -> elem -> Html Msg) -> Html Msg
grid array format =
    let autos = "auto"
                  |> List.repeat (Array2D.columns array)
                  |> String.join " "
        elements = Array2D.indexedMap format array |> flatten
    in div [ css [ Css.property "display" "grid"
                 , Css.property "grid-template-columns" autos
                 , Css.before [ Css.property "content" ""
                              , Css.display Css.block
                              , Css.paddingBottom (Css.pct 130)
                              ]
                 ]
           ] elements

colorGrid : Model -> Html Msg
colorGrid model =
    let format row column color = tile model color
    in grid model.colors format

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

selectColorButtonsGrid : Model -> Html Msg
selectColorButtonsGrid model =
    let format row column color = selectColorButton model row column
    in grid model.colors format

selectColorButton : Model -> Int -> Int -> Html Msg
selectColorButton model row column =
    let firstColor = Array2D.get row column model.colors
        color = Maybe.withDefault (Css.hex "000000") firstColor
    in input [ type_ "color"
             , value (fromColor color)
             , onInput (colorMsg row column)
             ] []

colorMsg : Int -> Int -> String -> Msg
colorMsg row column string =
    NewColor row column (Css.hex string)

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


-- HELPERS

numTiles : Model -> Int
numTiles model =
    Array2D.rows model.colors * Array2D.columns model.colors

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

setColors : Array2D Color -> Model -> Model
setColors colors model =
    { model | colors = colors }

asColorsIn : Model -> Array2D Color -> Model
asColorsIn =
    flip setColors

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
        numberStrings =
            List.map hexStr numbers |> String.concat
    in "#" ++ numberStrings

hexStr : Int -> String
hexStr number =
    String.pad 2 '0' (Hex.toString number)

flatten : Array2D item -> List item
flatten array =
    let get (i, j) = Array2D.get i j array
    in List.filterMap get (indices array)

cartesian : List a -> List b -> List (a, b)
cartesian xs ys =
  List.Extra.lift2 tuple xs ys

tuple : a -> b -> (a, b)
tuple a b = (a, b)

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) =
    f a b

indices : Array2D elem_ -> List (Int, Int)
indices array =
    let rows = List.range 0 (Array2D.rows array - 1)
        columns = List.range 0 (Array2D.columns array - 1)
    in cartesian rows columns
    
