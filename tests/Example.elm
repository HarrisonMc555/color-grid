module Example exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Logic
import Test exposing (..)
import Util


suite : Test
suite =
    describe "Test Util functions"
        [ describe "windows2"
            [ test "empty list makes empty list" <|
                \_ ->
                    Util.windows2 []
                        |> Expect.equal []
            , test "single-element list makes empty list" <|
                \_ ->
                    Util.windows2 [ "a" ]
                        |> Expect.equal []
            , test "two-element list makes single-element list with pair" <|
                \_ ->
                    Util.windows2 [ "a", "b" ]
                        |> Expect.equal [ ( "a", "b" ) ]
            , test "many elements work" <|
                \_ ->
                    let
                        flat =
                            [ "a", "b", "c", "d", "e" ]

                        pairs =
                            [ ( "a", "b" ), ( "b", "c" ), ( "c", "d" ), ( "d", "e" ) ]
                    in
                    Util.windows2 flat
                        |> Expect.equal pairs
            ]
        ]


interpolate : Test
interpolate =
    describe "Interpolate"
        [ describe "interpolateInsideRow"
            [ test "empty list" <|
                \_ ->
                    Logic.interpolateInsideRow defaultCalcDelta 3 []
                        |> Expect.equal []
            , test "singleton list" <|
                \_ ->
                    Logic.interpolateInsideRow defaultCalcDelta 3 [ 15 ]
                        |> Expect.equal [ 15 ]
            , test "two-element list" <|
                \_ ->
                    Logic.interpolateInsideRow defaultCalcDelta 2 [ 1.0, 4.0 ]
                        |> Expect.equal [ 1.0, 2.0, 3.0, 4.0 ]
            , test "defaultCalcDelta" <|
                \_ ->
                    defaultCalcDelta 2 1.0 4.0
                        |> Expect.within (Absolute 0.00000001) 1.0
            ]
        ]


defaultCalcDelta : Int -> Float -> Float -> Float
defaultCalcDelta numBetween min max =
    (max - min) / toFloat (numBetween + 1)
