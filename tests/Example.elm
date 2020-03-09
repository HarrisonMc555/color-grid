module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Util exposing (..)


suite : Test
suite =
    describe "Test Util functions"
        [ describe "windows2"
            [ test "empty list makes empty list" <|
                \_ ->
                    Expect.equal [] (windows2 [])
            , test "single-element list makes empty list" <|
                \_ ->
                    Expect.equal [] (windows2 [ "a" ])
            , test "two-element list makes single-element list with pair" <|
                \_ ->
                    Expect.equal [ ( "a", "b" ) ] (windows2 [ "a", "b" ])
            , test "many elements work" <|
                \_ ->
                    let
                        flat =
                            [ "a", "b", "c", "d", "e" ]

                        pairs =
                            [ ( "a", "b" ), ( "b", "c" ), ( "c", "d" ), ( "d", "e" ) ]
                    in
                    Expect.equal [ ( "a", "b" ) ] (windows2 [ "a", "b" ])
            ]
        ]
