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
    , tileSize : Size
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
    , tileSize = initTileSize
    }

initTileSize : Size
initTileSize =
    { width = 100
    , height = 100
    }


-- UPDATE

type Msg
    = NewColor Color

update : Msg -> Model -> Model
update msg model =
  case msg of
    NewColor color ->
        color
            |> asColorIn model


-- VIEW

view : Model -> Html Msg
view model =
  div [] [ tile model
         , selectColorButton model
         ]

tile : Model -> Html Msg
tile model =
    div [ css [ tileSize model.tileSize
              , tileColor model.color
              ]
        ]
        []

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
