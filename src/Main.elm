module Main exposing (main)

import Array
import Array2D exposing (Array2D)
import Browser
import Css exposing (Color, Style)
import Hex
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import List.Extra
import Model exposing (..)
import Update exposing (..)
import Util exposing (..)
import View exposing (..)


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
