module Logic exposing
    ( cyclicalCalcDelta
    , interpolateBetweenRows
    , interpolateColorGrid
    , interpolateGridHelper
    , interpolateInsideRow
    , linearCalcDelta
    )

import Array2D exposing (Array2D)
import Color exposing (Color)
import List.Extra
import Maybe.Extra
import Util


interpolateColorGrid : Array2D Color -> Int -> Int -> Array2D Color
interpolateColorGrid pinnedGrid numBetweenRows numBetweenColumns =
    let
        hslaGrid =
            Array2D.map Color.toHsla pinnedGrid

        interpolate calcDelta =
            interpolateGrid calcDelta numBetweenRows numBetweenColumns

        hueGrid =
            Array2D.map .hue hslaGrid
                |> interpolate (cyclicalCalcDelta 1.0)
                |> Array2D.map (\f -> Util.fractionalRem f 1.0)

        saturationGrid =
            Array2D.map .saturation hslaGrid
                |> interpolate linearCalcDelta

        lightnessGrid =
            Array2D.map .lightness hslaGrid
                |> interpolate linearCalcDelta

        alphaGrid =
            Array2D.map .alpha hslaGrid
                |> interpolate linearCalcDelta
                |> Array2D.map (\_ -> 1.0)
    in
    recombineHslaGrid hueGrid saturationGrid lightnessGrid alphaGrid


recombineHslaGrid :
    Array2D Float
    -> Array2D Float
    -> Array2D Float
    -> Array2D Float
    -> Array2D Color
recombineHslaGrid hueGrid saturationGrid lightnessGrid alphaGrid =
    let
        unwrap =
            Maybe.withDefault 0

        reconstruct x y _ =
            let
                get =
                    unwrap << Array2D.get x y

                hue =
                    get hueGrid

                saturation =
                    get saturationGrid

                lightness =
                    get lightnessGrid

                alpha =
                    get alphaGrid
            in
            Color.hsla hue saturation lightness alpha
    in
    Array2D.indexedMap reconstruct hueGrid


linearCalcDelta : Int -> Float -> Float -> Float
linearCalcDelta numBetween start end =
    (end - start) / toFloat (numBetween + 1)


cyclicalCalcDelta : Float -> Int -> Float -> Float -> Float
cyclicalCalcDelta max numBetween start end =
    let
        diff1 =
            end - start

        diff2 =
            if diff1 < 0 then
                diff1 + max

            else
                diff1 - max

        diff =
            if abs diff1 < abs diff2 then
                diff1

            else
                diff2
    in
    diff / (toFloat numBetween + 1)


interpolateGrid :
    (Int -> Float -> Float -> Float)
    -> Int
    -> Int
    -> Array2D Float
    -> Array2D Float
interpolateGrid calcDelta numBetweenRows numBetweenColumns grid =
    let
        rows =
            Util.array2DToList grid

        interpolatedGrid =
            interpolateGridHelper calcDelta numBetweenRows numBetweenColumns rows
    in
    Array2D.fromList interpolatedGrid


interpolateGridHelper :
    (Int -> Float -> Float -> Float)
    -> Int
    -> Int
    -> List (List Float)
    -> List (List Float)
interpolateGridHelper calcDelta numBetweenRows numBetweenColumns pinnedRows =
    let
        interpolatedRows =
            interpolateRows calcDelta numBetweenColumns pinnedRows
    in
    interpolateBetweenRows calcDelta numBetweenRows interpolatedRows


interpolateRows :
    (Int -> Float -> Float -> Float)
    -> Int
    -> List (List Float)
    -> List (List Float)
interpolateRows calcDelta numBetween pinnedRows =
    let
        interpolate : List Float -> List Float
        interpolate =
            interpolateInsideRow calcDelta numBetween
    in
    List.map interpolate pinnedRows


interpolateBetweenRows :
    (Int -> Float -> Float -> Float)
    -> Int
    -> List (List Float)
    -> List (List Float)
interpolateBetweenRows calcDelta numBetween rows =
    let
        pairsOfRows =
            Util.windows2 rows

        getDelta : Float -> Float -> Float
        getDelta =
            calcDelta numBetween

        getDeltaRow : ( List Float, List Float ) -> List Float
        getDeltaRow ( first, second ) =
            List.map2 getDelta first second

        deltas : List (List Float)
        deltas =
            pairsOfRows
                |> List.map getDeltaRow

        lastRowAs2DList : List (List Float)
        lastRowAs2DList =
            List.Extra.last rows
                |> Maybe.Extra.toList

        interpolated : List (List Float)
        interpolated =
            List.map2 (interpolateRow numBetween) rows deltas
                |> List.concat
    in
    interpolated ++ lastRowAs2DList


interpolateInsideRow : (Int -> Float -> Float -> Float) -> Int -> List Float -> List Float
interpolateInsideRow calcDelta numBetween row =
    let
        pairs =
            Util.windows2 row

        getDelta =
            Util.uncurry <| calcDelta numBetween

        deltas =
            pairs |> List.map getDelta

        lastElementAsList =
            List.Extra.last row
                |> Maybe.Extra.toList

        interpolated =
            List.map2 (interpolateValue numBetween) row deltas
                |> List.concat
    in
    interpolated ++ lastElementAsList


interpolateValue : Int -> Float -> Float -> List Float
interpolateValue numBetween pinned delta =
    let
        addOffset offset =
            pinned + toFloat offset * delta

        interpolated =
            List.range 1 numBetween
                |> List.map addOffset
    in
    pinned :: interpolated


interpolateRow : Int -> List Float -> List Float -> List (List Float)
interpolateRow numBetween pinnedRow deltas =
    let
        addDelta offset pinned delta =
            pinned + delta * toFloat offset

        addOffset offset =
            List.map2 (addDelta offset) pinnedRow deltas

        interpolated =
            List.range 1 numBetween
                |> List.map addOffset
    in
    pinnedRow :: interpolated
