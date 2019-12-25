module Board exposing (..)

import Array exposing (..)
import Helpers
import Set exposing (..)


type alias Position =
    { x : Int, y : Int }


type alias Visited =
    Set ( Int, Int )


visit : Position -> Visited -> Visited
visit position =
    Set.insert ( position.x, position.y )


isVisited : Position -> Visited -> Bool
isVisited position =
    Set.member ( position.x, position.y )


type alias ABoard a =
    { width : Int
    , height : Int
    , array : Array (Array a)
    }


init : (Char -> a) -> String -> ABoard a
init f =
    String.lines
        >> List.map (String.toList >> List.map f >> Array.fromList)
        >> Array.fromList
        >> (\arr ->
                { width = Array.get 0 arr |> Maybe.map Array.length |> Maybe.withDefault 0
                , height = Array.length arr
                , array = arr
                }
           )


find : (a -> Bool) -> ABoard a -> List ( Position, a )
find f board =
    foldl
        (\p a ->
            if f a then
                (::) ( p, a )

            else
                identity
        )
        []
        board


map : (a -> b) -> ABoard a -> ABoard b
map f =
    updateArray (Array.map (Array.map f))


foldl : (Position -> a -> b -> b) -> b -> ABoard a -> b
foldl f acc b =
    foldHelper f acc (Position 0 0) b


foldHelper : (Position -> a -> b -> b) -> b -> Position -> ABoard a -> b
foldHelper f acc p board =
    if board.height <= p.y then
        acc

    else if board.width <= p.x then
        foldHelper f acc (Position 0 (p.y + 1)) board

    else
        foldHelper f (f p (get p board) acc) (p |> updateX ((+) 1)) board


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


updateArray : (Array (Array a) -> Array (Array b)) -> ABoard a -> ABoard b
updateArray f r =
    { width = r.width, height = r.height, array = f r.array }


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
