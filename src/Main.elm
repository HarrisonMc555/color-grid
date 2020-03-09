module Main exposing (main)

import Array2D exposing (Array2D)
import Browser
import Css exposing (Color)
import Html.Styled exposing (toUnstyled)
import Model exposing (Model, Size)
import Update exposing (Msg, update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


init : Model
init =
    { colors = initColors
    , numInBetweenRows = 2
    , numInBetweenColumns = 3
    , tileSize = initTileSize
    }


initTileSize : Size
initTileSize =
    { width = 100
    , height = 100
    }


initColors : Array2D Color
initColors =
    Array2D.fromList
        [ [ Css.hex "777777"
          , Css.hex "0000ff"
          ]
        , [ Css.hex "00ff00"
          , Css.hex "ff0000"
          ]
        ]
