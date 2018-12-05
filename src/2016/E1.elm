module E1 exposing (..)

import Char
import Dict
import Html


main =
    Html.div []
        [ Html.text <| toString result
        , Html.text <| toString (abs result.verticalLength + abs result.horizontalLength)
        , Html.text <| toString result2
        ]


type Turn
    = R
    | L


type Direction
    = N
    | E
    | W
    | S


type alias Move =
    { turn : Turn
    , length : Int
    }


type alias State =
    { currentDirection : Direction
    , verticalLength : Int
    , horizontalLength : Int
    }


parseMove s =
    let
        turn =
            case s |> String.toList |> List.head of
                Just 'R' ->
                    R

                _ ->
                    L

        length =
            case s |> String.toList |> List.drop 1 |> String.fromList |> String.toInt |> Result.toMaybe of
                Just x ->
                    x

                _ ->
                    0
    in
    Move turn length


parseInput =
    input |> String.split ", " |> List.map parseMove


takeTurn direction turn =
    case direction of
        N ->
            case turn of
                R ->
                    E

                L ->
                    W

        E ->
            case turn of
                R ->
                    S

                L ->
                    N

        W ->
            case turn of
                R ->
                    N

                L ->
                    S

        S ->
            case turn of
                R ->
                    W

                L ->
                    E


makeVector direction =
    case direction of
        N ->
            ( 1, 0 )

        E ->
            ( 0, 1 )

        W ->
            ( 0, -1 )

        S ->
            ( -1, 0 )


makeMove move state =
    let
        newDirection =
            takeTurn state.currentDirection move.turn

        newState =
            { state | currentDirection = newDirection }

        vector =
            makeVector newDirection
    in
    { newState | verticalLength = Tuple.first vector * move.length + newState.verticalLength, horizontalLength = Tuple.second vector * move.length + newState.horizontalLength } |> Debug.log (toString move)


result =
    parseInput |> List.foldl makeMove { currentDirection = N, verticalLength = 0, horizontalLength = 0 }


type alias State2 =
    { state1 : State
    , dict : Dict.Dict ( Int, Int ) ()
    , result : Maybe Int
    }


makeMoveAllTheWay direction length state2 =
    case length of
        0 ->
            state2

        x ->
            let
                state1 =
                    state2.state1

                vector =
                    makeVector direction

                newState1 =
                    { state1 | verticalLength = Tuple.first vector * 1 + state1.verticalLength, horizontalLength = Tuple.second vector * 1 + state1.horizontalLength }
            in
            case state2.dict |> Dict.get ( newState1.verticalLength, newState1.horizontalLength ) of
                Just _ ->
                    { state2 | result = Just <| abs newState1.verticalLength + abs newState1.horizontalLength }

                Nothing ->
                    makeMoveAllTheWay direction (length - 1) { state2 | dict = state2.dict |> Dict.insert ( newState1.verticalLength, newState1.horizontalLength ) (), state1 = newState1 }


makeMove2 move state2 =
    let
        newState1 =
            makeMove { move | length = 0 } state2.state1
    in
    case state2.result of
        Just x ->
            state2

        Nothing ->
            makeMoveAllTheWay newState1.currentDirection move.length { state2 | state1 = newState1 }


result2 =
    parseInput |> List.foldl makeMove2 { state1 = { currentDirection = N, verticalLength = 0, horizontalLength = 0 }, dict = Dict.empty |> Dict.insert ( 0, 0 ) (), result = Nothing }


input =
    "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1"
