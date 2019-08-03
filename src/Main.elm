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

type alias Model = Int
type alias ColorModel =
    {
        colors : Array2D Color
    }

init : Model
init =
  0


-- UPDATE

type Msg = Increment | Decrement | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

    Reset ->
      0


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    , br [] []
    , button [ onClick Reset ] [ text "reset" ]
    , div [ css [ blockDimensions, blockColor ] ] []
    ]

blockDimensions : Style
blockDimensions =
    Css.batch [ Css.width (Css.px 100)
              , Css.height (Css.px 100)
              ]

blockColor : Style
blockColor =
    Css.backgroundColor (Css.hex "60c71c")
