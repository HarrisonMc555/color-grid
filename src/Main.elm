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
    , dimensions : Dimensions
    }

type alias Dimensions =
    { width : Dimension
    , height : Dimension
    }

type alias Dimension = Float

init : Model
init =
    -- { colors = Array2D.empty
    { color = Css.hex "60c71c"
    , dimensions = initDimensions
    }

initDimensions : Dimensions
initDimensions =
    { width = 100
    , height = 100
    }


-- UPDATE

type Msg
    = Width Dimension
    | Height Dimension
    | IncrementWidth
    | IncrementHeight
    | NewColor Color

update : Msg -> Model -> Model
update msg model =
  case msg of
    Width width ->
        width
            |> asWidthIn model.dimensions
            |> asDimensionsIn model

    Height height ->
        height
            |> asHeightIn model.dimensions
            |> asDimensionsIn model

    IncrementWidth ->
        model.dimensions.width + 1
            |> asWidthIn model.dimensions
            |> asDimensionsIn model

    IncrementHeight ->
        model.dimensions.height + 1
            |> asHeightIn model.dimensions
            |> asDimensionsIn model

    NewColor color ->
        color
            |> asColorIn model


-- VIEW

view : Model -> Html Msg
view model =
  div [] [ tile model
         , increaseWidthButton model
         , selectColorButton model
         ]

tile : Model -> Html Msg
tile model =
    div [ css [ tileDimensions model.dimensions
              , tileColor model.color
              ]
        ]
        []

tileDimensions : Dimensions -> Style
tileDimensions dimensions =
    Css.batch [ Css.width (Css.px dimensions.width)
              , Css.height (Css.px dimensions.height)
              ]

tileColor : Color -> Style
tileColor color =
    Css.backgroundColor color


increaseWidthButton : Model -> Html Msg
increaseWidthButton model =
    button [ onClick IncrementWidth ] [ text "Increase Width" ]

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


-- HELPERS

setWidth : Dimension -> Dimensions -> Dimensions
setWidth width dimension =
    { dimension | width = width }

setHeight : Dimension -> Dimensions -> Dimensions
setHeight height dimension =
    { dimension | height = height }

asWidthIn : Dimensions -> Dimension -> Dimensions
asWidthIn =
    flip setWidth

asHeightIn : Dimensions -> Dimension -> Dimensions
asHeightIn =
    flip setHeight

setDimensions : Dimensions -> Model -> Model
setDimensions dimensions model =
    { model | dimensions = dimensions }

asDimensionsIn : Model -> Dimensions -> Model
asDimensionsIn =
    flip setDimensions

setColor : Color -> Model -> Model
setColor color model =
    { model | color = color }

asColorIn : Model -> Color -> Model
asColorIn =
    flip setColor

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
