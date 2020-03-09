module Util exposing
    ( array2dToList
    , arrayFlatMap
    , arrayWindows2
    , cartesian
    , flatten
    , flip
    , fromColor
    , hexStr
    , tuple
    , uncurry
    , windows2
    )

import Array exposing (Array)
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
    List.filterMap get (array2DIndices array)


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.Extra.lift2 tuple xs ys


tuple : a -> b -> ( a, b )
tuple a b =
    ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


array2DIndices : Array2D elem_ -> List ( Int, Int )
array2DIndices array =
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


arrayWindows2 : Array a -> Array ( a, a )
arrayWindows2 array =
    let
        indices =
            List.range 0 (Array.length array - 1)

        getPair index =
            case ( Array.get index array, Array.get (index + 1) array ) of
                ( Just a, Just b ) ->
                    Just ( a, b )

                _ ->
                    Nothing
    in
    List.filterMap getPair indices |> Array.fromList


array2dToList : Array2D a -> List (List a)
array2dToList array2d =
    let
        rowIndices =
            List.range 0 (Array2D.rows array2d - 1)

        getRow index =
            Array2D.getRow index array2d |> Maybe.map Array.toList
    in
    List.filterMap getRow rowIndices


arrayFlatMap : Array a -> (a -> Array b) -> Array b
arrayFlatMap array f =
    let
        acc x xs =
            Array.append xs <| f x
    in
    Array.foldl acc Array.empty array
