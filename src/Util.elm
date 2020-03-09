module Util exposing
    ( cartesian
    , flatten
    , flip
    , fromColor
    , hexStr
    , indices
    , tuple
    , uncurry
    , windows2
    )

import Array2D exposing (Array2D)
import Css exposing (Color)
import Hex
import List.Extra


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


fromColor : Color -> String
fromColor color =
    let
        numbers =
            [ color.red
            , color.green
            , color.blue
            ]

        numberStrings =
            List.map hexStr numbers |> String.concat
    in
    "#" ++ numberStrings


hexStr : Int -> String
hexStr number =
    String.pad 2 '0' (Hex.toString number)


flatten : Array2D item -> List item
flatten array =
    let
        get ( i, j ) =
            Array2D.get i j array
    in
    List.filterMap get (indices array)


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.Extra.lift2 tuple xs ys


tuple : a -> b -> ( a, b )
tuple a b =
    ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


indices : Array2D elem_ -> List ( Int, Int )
indices array =
    let
        rows =
            List.range 0 (Array2D.rows array - 1)

        columns =
            List.range 0 (Array2D.columns array - 1)
    in
    cartesian rows columns


windows2 : List a -> List ( a, a )
windows2 l =
    case l of
        x :: y :: rest ->
            ( x, y ) :: windows2 (y :: rest)

        _ ->
            []
