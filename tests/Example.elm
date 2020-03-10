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
                    Logic.interpolateInsideRow Logic.linearCalcDelta 3 []
                        |> Expect.equal []
            , test "singleton list" <|
                \_ ->
                    Logic.interpolateInsideRow Logic.linearCalcDelta 3 [ 15 ]
                        |> Expect.equal [ 15 ]
            , test "two-element list" <|
                \_ ->
                    Logic.interpolateInsideRow Logic.linearCalcDelta 2 [ 1, 4 ]
                        |> Expect.equal [ 1, 2, 3, 4 ]
            , test "Logic.linearCalcDelta" <|
                \_ ->
                    Logic.linearCalcDelta 2 1 4
                        |> Expect.within (Absolute 0.000001) 1
            ]
        , describe "interpolateBetweenRows"
            [ test "empty list" <|
                \_ ->
                    Logic.interpolateBetweenRows Logic.linearCalcDelta 2 []
                        |> Expect.equal []
            , test "two rows" <|
                \_ ->
                    Logic.interpolateBetweenRows Logic.linearCalcDelta
                        2
                        [ [ 1, 2, 3 ]
                        , [ 4, 8, 12 ]
                        ]
                        |> Expect.equal
                            [ [ 1, 2, 3 ]
                            , [ 2, 4, 6 ]
                            , [ 3, 6, 9 ]
                            , [ 4, 8, 12 ]
                            ]
            ]
        , describe "Logic.cyclicalCalcDelta"
            [ test "top row" <|
                \_ ->
                    Logic.cyclicalCalcDelta 360 1 20 330
                        |> Expect.within (Absolute 0.0000001) -25
            , test "bottom row" <|
                \_ ->
                    Logic.cyclicalCalcDelta 360 1 50 90
                        |> Expect.within (Absolute 0.0000001) 20
            , test "left column" <|
                \_ ->
                    Logic.cyclicalCalcDelta 360 2 20 50
                        |> Expect.within (Absolute 0.0000001) 10
            , test "right column" <|
                \_ ->
                    Logic.cyclicalCalcDelta 360 2 330 90
                        |> Expect.within (Absolute 0.0000001) 40
            ]
        , describe "fractionalRemBy"
            [ test "pos % pos" <|
                \_ ->
                    Util.fractionalRem 5.5 2
                        |> Expect.within (Absolute 0.0000001) 1.5
            ]
        , describe "interpolateGridHelper"
            [ test "empty grid" <|
                \_ ->
                    Logic.interpolateGridHelper Logic.linearCalcDelta 2 3 []
                        |> Expect.equal []
            , test "three rows, three columns" <|
                \_ ->
                    Logic.interpolateGridHelper Logic.linearCalcDelta
                        3
                        3
                        [ [ 10, 18 ]
                        , [ 26, 22 ]
                        ]
                        |> Expect.equal
                            [ [ 10, 12, 14, 16, 18 ]
                            , [ 14, 15.25, 16.5, 17.75, 19 ]
                            , [ 18, 18.5, 19, 19.5, 20 ]
                            , [ 22, 21.75, 21.5, 21.25, 21 ]
                            , [ 26, 25, 24, 23, 22 ]
                            ]
            , test "cyclical" <|
                \_ ->
                    Logic.interpolateGridHelper (Logic.cyclicalCalcDelta 360)
                        2
                        1
                        [ [ 20, 330 ]
                        , [ 50, 90 ]
                        ]
                        |> List.map (List.map (\f -> Util.fractionalRem f 360))
                        |> Expect.equal
                            [ [ 20, 355, 330 ]
                            , [ 30, 20, 10 ]
                            , [ 40, 45, 50 ]
                            , [ 50, 70, 90 ]
                            ]
            ]
        ]
