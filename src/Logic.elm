module Logic exposing (interpolateColorGrid, interpolateInsideRow)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Color exposing (Color)
import List.Extra
import Maybe.Extra
import Util


type alias Hsla =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }


type alias MaybeHsla =
    { hue : Maybe Float
    , saturation : Maybe Float
    , lightness : Maybe Float
    , alpha : Maybe Float
    }


type alias CalcDelta =
    Int -> Float -> Float -> Float


interpolateColorGrid : Array2D Color -> Int -> Int -> Array2D Color
interpolateColorGrid pinnedGrid numInBetweenRows numInBetweenColumns =
    let
        hslaGrid =
            Array2D.map Color.toHsla pinnedGrid

        hueGrid =
            Array2D.map .hue hslaGrid

        saturationGrid =
            Array2D.map .saturation hslaGrid

        lightnessGrid =
            Array2D.map .lightness hslaGrid

        alphaGrid =
            Array2D.map .alpha hslaGrid
    in
    recombineHslaGrid hueGrid saturationGrid lightnessGrid alphaGrid


recombineHslaGrid :
    Array2D Float
    -> Array2D Float
    -> Array2D Float
    -> Array2D Float
    -> Array2D Color
recombineHslaGrid hueGrid saturationGrid lightnessGrid alphaGrid =
    Array2D.empty



-- let
--     rowPairs =
--         Util.windows2 <| Util.array2dToList pinnedGrid
--     lastRow =
--         Array2D.getRow (Array2D.rows pinnedGrid - 1) pinnedGrid
--             |> Maybe.withDefault Array.empty
-- in
-- pinnedGrid


interpolateInsideRow : (Int -> Float -> Float -> Float) -> Int -> List Float -> List Float
interpolateInsideRow calcDelta numBetween row =
    let
        pairs =
            Util.windows2 row

        deltas =
            List.map (Util.uncurry (calcDelta numBetween)) pairs

        lastElementAsList =
            List.Extra.last row
                |> Maybe.Extra.toList

        interpolated =
            List.map2 (interpolateValue numBetween) row deltas
                |> List.concat
    in
    interpolated ++ lastElementAsList


interpolateColorRow : Array Color -> Int -> Int -> Array Color
interpolateColorRow pinnedRow numInBetweenRows numInBetweenColumns =
    pinnedRow


interpolateArray : Int -> List Float -> List Float -> List (List Float)
interpolateArray numBetween pinned delta =
    List.map2 (interpolateValue numBetween) pinned delta
        |> List.Extra.transpose


interpolateValue : Int -> Float -> Float -> List Float
interpolateValue numBetween pinned delta =
    let
        interpolated =
            List.range 1 numBetween
                |> List.map toFloat
                |> List.map (\i -> pinned + i * delta)
    in
    pinned :: interpolated
