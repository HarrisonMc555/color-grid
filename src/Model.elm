module Model exposing
    ( Model
    , Size
    , asColorsIn
    , asHeightIn
    , asNumColumnsIn
    , asNumRowsIn
    , asSizeIn
    , asWidthIn
    , numTiles
    )

import Array2D exposing (Array2D)
import Css exposing (Color)
import Util


type alias Model =
    { colors : Array2D Color
    , numInBetweenRows : Int
    , numInBetweenColumns : Int
    , tileSize : Size
    }


type alias Dimensions =
    { numRows : Int
    , numColumns : Int
    }


type alias Size =
    { width : Length
    , height : Length
    }


type alias Length =
    Float


numTiles : Model -> Int
numTiles model =
    Array2D.rows model.colors * Array2D.columns model.colors


setWidth : Length -> Size -> Size
setWidth width length =
    { length | width = width }


setHeight : Length -> Size -> Size
setHeight height length =
    { length | height = height }


asWidthIn : Size -> Length -> Size
asWidthIn =
    Util.flip setWidth


asHeightIn : Size -> Length -> Size
asHeightIn =
    Util.flip setHeight


setSize : Size -> Model -> Model
setSize size model =
    { model | tileSize = size }


asSizeIn : Model -> Size -> Model
asSizeIn =
    Util.flip setSize


setColors : Array2D Color -> Model -> Model
setColors colors model =
    { model | colors = colors }


asColorsIn : Model -> Array2D Color -> Model
asColorsIn =
    Util.flip setColors


setNumRows : Int -> Dimensions -> Dimensions
setNumRows numRows dimensions =
    { dimensions | numRows = numRows }


asNumRowsIn : Dimensions -> Int -> Dimensions
asNumRowsIn =
    Util.flip setNumRows


setNumColumns : Int -> Dimensions -> Dimensions
setNumColumns numColumns dimensions =
    { dimensions | numColumns = numColumns }


asNumColumnsIn : Dimensions -> Int -> Dimensions
asNumColumnsIn =
    Util.flip setNumColumns
