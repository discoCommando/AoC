module Board exposing (..)

import Array exposing (..)
import Helpers


type alias Position =
    { x : Int, y : Int }


type alias ABoard a =
    { width : Int
    , height : Int
    , array : Array (Array a)
    }


map : (a -> b) -> ABoard a -> ABoard b
map f =
    updateArray (Array.map (Array.map f))


indexedMap : (Position -> a -> b) -> ABoard a -> ABoard b
indexedMap f =
    updateArray <| Array.indexedMap (\y -> Array.indexedMap (\x -> f (Position x y)))


get : Position -> ABoard a -> a
get position board =
    case board |> .array |> Array.get (.y position) of
        Nothing ->
            Helpers.crash "get y" ( position, board )

        Just row ->
            case row |> Array.get (.x position) of
                Nothing ->
                    Helpers.crash "get x" ( position, board )

                Just v ->
                    v


set : Position -> a -> ABoard a -> ABoard a
set position v board =
    case board |> .array |> Array.get (.y position) of
        Nothing ->
            Helpers.crash "set y" ( position, board )

        Just row ->
            case row |> Array.get (.x position) of
                Nothing ->
                    Helpers.crash "set x" ( position, board )

                Just _ ->
                    board |> updateArray (Array.set (.y position) (row |> Array.set (.x position) v))


update : Position -> (a -> a) -> ABoard a -> ABoard a
update position f board =
    set position (get position board |> f) board


type Direction
    = North
    | South
    | West
    | East


directions : List Direction
directions =
    [ North, South, West, East ]


type Turn
    = Right
    | Left


performTurn : Turn -> Direction -> Direction
performTurn turn direction =
    case turn of
        Left ->
            case direction of
                North ->
                    West

                West ->
                    South

                South ->
                    East

                East ->
                    West

        Right ->
            case direction of
                North ->
                    East

                West ->
                    North

                South ->
                    West

                East ->
                    South


performMove : Direction -> Position -> Position
performMove direction position =
    position
        |> (case direction of
                North ->
                    updateY (\y -> y - 1)

                South ->
                    updateY ((+) 1)

                West ->
                    updateX (\x -> x - 1)

                East ->
                    updateX ((+) 1)
           )


type SafeToGo
    = Safe
    | Unsafe


unsafeIf : (a -> Bool) -> (a -> SafeToGo)
unsafeIf f a =
    if f a then
        Unsafe

    else
        Safe


getPossiblePositions : Position -> (a -> SafeToGo) -> ABoard a -> List Position
getPossiblePositions position f board =
    directions
        |> List.map (\d -> performMove d position)
        |> List.filter
            (\np ->
                [ .x >> (<=) 0
                , .x >> (>) board.width
                , .y >> (<=) 0
                , .y >> (>) board.height
                , get >> (|>) board >> f >> (==) Safe
                ]
                    |> List.map ((|>) np)
                    |> List.all identity
            )



-- HELPERS


setWidth : Int -> { r | width : Int } -> { r | width : Int }
setWidth val r =
    { r | width = val }


updateWidth : (Int -> Int) -> { r | width : Int } -> { r | width : Int }
updateWidth f ({ width } as r) =
    { r | width = f width }


setHeight : Int -> { r | height : Int } -> { r | height : Int }
setHeight val r =
    { r | height = val }


updateHeight : (Int -> Int) -> { r | height : Int } -> { r | height : Int }
updateHeight f ({ height } as r) =
    { r | height = f height }


setArray : Array (Array a) -> { r | array : Array (Array a) } -> { r | array : Array (Array a) }
setArray val r =
    { r | array = val }


updateArray : (Array (Array a) -> Array (Array b)) -> { r | array : Array (Array a) } -> { r | array : Array (Array b) }
updateArray f ({ array } as r) =
    { r | array = f array }


setX : Int -> { r | x : Int } -> { r | x : Int }
setX val r =
    { r | x = val }


updateX : (Int -> Int) -> { r | x : Int } -> { r | x : Int }
updateX f ({ x } as r) =
    { r | x = f x }


setY : Int -> { r | y : Int } -> { r | y : Int }
setY val r =
    { r | y = val }


updateY : (Int -> Int) -> { r | y : Int } -> { r | y : Int }
updateY f ({ y } as r) =
    { r | y = f y }
