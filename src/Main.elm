-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html.Styled exposing (Html, button, div, text, br, toUnstyled)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Attributes exposing (css)
import Color exposing (Color)
import Array2D exposing (Array2D)
import Css
import Css exposing (Style)


main =
  Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


-- MODEL

type alias Model =
    { colors : Array2D Color
    , dimensions : Dimensions
    }

type alias Dimensions =
    { width : Dimension
    , height : Dimension
    }

type alias Dimension = Float

init : Model
init =
    { colors = Array2D.empty
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
    -- | Color ...

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

    -- Color ... ->


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [ css [ tileDimensions model.dimensions
                , tileColor
                ]
          ]
          []
    ]

tileDimensions : Dimensions -> Style
tileDimensions dimensions =
    Css.batch [ Css.width (Css.px dimensions.width)
              , Css.height (Css.px dimensions.height)
              ]

tileColor : Style
tileColor =
    Css.backgroundColor (Css.hex "60c71c")


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

flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a
