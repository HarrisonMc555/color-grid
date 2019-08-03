-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, text, br)
import Html.Events exposing (onClick)
import Color exposing (Color)
import Array2D exposing (Array2D)


main =
  Browser.sandbox { init = init, update = update, view = view }


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
    , div [ "style" "background-color: #60c71c; width: 100px; height: 100px;" ] []
    , div [ "style" "width: 100px; height: 100px;" ] []
    ]
