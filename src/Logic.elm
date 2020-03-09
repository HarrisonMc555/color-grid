module Logic exposing (interpolateColorGrid)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Color exposing (toHsla)
import Css exposing (Color)
import Util


interpolateColorGrid : Array2D Color -> Int -> Int -> Array2D Color
interpolateColorGrid pinnedGrid numInBetweenRows numInBetweenColumns =
    let
        rowPairs =
            Util.windows2 <| Util.array2dToList pinnedGrid

        lastRow =
            Array2D.getRow (Array2D.rows pinnedGrid - 1) pinnedGrid
                |> Maybe.withDefault Array.empty
    in
    pinnedGrid


interpolateRow : Array Color -> Int -> Int -> Array Color
interpolateRow pinnedRow numInBetweenRows numInBetweenColumns =
    pinnedRow



-- let
--     elementPairs =
-- Util.arrayFlatMap
