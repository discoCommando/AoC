module Intcode exposing (..)

import Array exposing (Array)
import Helpers


type alias Cell =
    { index : Int, mode : Mode }


type Action
    = Add Cell Cell Cell
    | Mult Cell Cell Cell
    | Halt
    | Input Cell
    | Output Cell
    | IfTrue Cell Cell
    | IfFalse Cell Cell
    | LessThan Cell Cell Cell
    | Equals Cell Cell Cell


type Mode
    = Position
    | Immediate


nextMode : Mode -> List Mode
nextMode m =
    m
        :: (case m of
                Position ->
                    nextMode Immediate

                Immediate ->
                    []
           )


allModes : List Mode
allModes =
    nextMode Position


encodeMode : Mode -> Int
encodeMode m =
    case m of
        Position ->
            0

        Immediate ->
            1


getCell : Mode -> Int -> Array Int -> Cell
getCell mode index array =
    Helpers.uG index array
        |> Cell
        |> (|>) mode


immediateCell : Int -> Cell
immediateCell i =
    Cell i Immediate


getAction : Int -> Array Int -> Action
getAction index array =
    let
        value =
            array |> Helpers.uG index

        opcode =
            value |> Basics.modBy 100

        mode1 =
            value // 100 |> modBy 10 |> decodeMode

        mode2 =
            value // 1000 |> modBy 10 |> decodeMode

        mode3 =
            value // 10000 |> modBy 10 |> decodeMode
    in
    case opcode of
        1 ->
            Add
                (getCell mode1 (index + 1) array)
                (getCell mode2 (index + 2) array)
                (getCell Immediate (index + 3) array)

        2 ->
            Mult
                (getCell mode1 (index + 1) array)
                (getCell mode2 (index + 2) array)
                (getCell Immediate (index + 3) array)

        99 ->
            Halt

        3 ->
            Input
                (getCell Immediate (index + 1) array)

        4 ->
            Output
                (getCell mode1 (index + 1) array)

        5 ->
            IfTrue
                (getCell mode1 (index + 1) array)
                (getCell mode2 (index + 2) array)

        6 ->
            IfFalse
                (getCell mode1 (index + 1) array)
                (getCell mode2 (index + 2) array)

        7 ->
            LessThan
                (getCell mode1 (index + 1) array)
                (getCell mode2 (index + 2) array)
                (getCell Immediate (index + 3) array)

        8 ->
            Equals
                (getCell mode1 (index + 1) array)
                (getCell mode2 (index + 2) array)
                (getCell Immediate (index + 3) array)

        _ ->
            Debug.todo <| "opcode " ++ Debug.toString opcode


decodeMode : Int -> Mode
decodeMode i =
    allModes
        |> List.map (\x -> ( x, encodeMode x ))
        |> List.filter (\( x, c ) -> c == i)
        |> List.head
        |> Helpers.uM
        |> Tuple.first


get : Cell -> Array Int -> Int
get { mode, index } array =
    case mode of
        Immediate ->
            index

        Position ->
            Helpers.uG index array



--            get (Cell (Helpers.uG index array) Immediate) array
--nextPosition : Action -> Int
--nextPosition action =
--    case action of
--        Add _ _ _ -> 4
--        Mult _ _ _ -> 4
--        Input _ -> 2
--        Output _ -> 2
--        _ -> Debug.todo "nextPosition"


binaryOperation : Cell -> Cell -> Cell -> (Int -> Int -> Int) -> State -> State
binaryOperation c1 c2 out op state =
    let
        v1 =
            get c1 state.array

        v2 =
            get c2 state.array

        vout =
            out.index
    in
    { state | array = Array.set vout (op v1 v2) state.array }


jumpIf : Cell -> Cell -> (Int -> Bool) -> State -> Index
jumpIf c out op state =
    let
        v1 =
            get c state.array

        v2 =
            get out state.array
    in
    if op v1 then
        Constant v2

    else
        Plus 3


type alias State =
    { array : Array Int
    , input : Int
    , output : Int
    }


type Index
    = Constant Int
    | Plus Int


operation : Action -> Maybe (State -> ( Index, State ))
operation a =
    case a of
        Add c1 c2 o ->
            binaryOperation c1 c2 o (+) >> Tuple.pair (Plus 4) |> Just

        Mult c1 c2 o ->
            binaryOperation c1 c2 o (*) >> Tuple.pair (Plus 4) |> Just

        Halt ->
            Nothing

        Input c ->
            (\state ->
                let
                    v1 =
                        get c state.array
                in
                { state | array = Array.set v1 state.input state.array }
            )
                >> Tuple.pair (Plus 2)
                |> Just

        Output c ->
            (\state ->
                let
                    v1 =
                        get c state.array

                    _ =
                        Debug.log "output" output

                    output =
                        --                        Helpers.uG v1 state.array
                        v1
                in
                { state | output = output }
            )
                >> Tuple.pair (Plus 2)
                |> Just

        IfTrue c o ->
            (\state ->
                Tuple.pair
                    (jumpIf c
                        o
                        (\i ->
                            if i == 0 then
                                False

                            else
                                True
                        )
                        state
                    )
                    state
            )
                |> Just

        IfFalse c o ->
            (\state ->
                Tuple.pair
                    (jumpIf c
                        o
                        (\i ->
                            if i == 0 then
                                True

                            else
                                False
                        )
                        state
                    )
                    state
            )
                |> Just

        LessThan c1 c2 o ->
            binaryOperation c1
                c2
                o
                (\v1 v2 ->
                    if v1 < v2 then
                        1

                    else
                        0
                )
                >> Tuple.pair (Plus 4)
                |> Just

        Equals c1 c2 o ->
            binaryOperation c1
                c2
                o
                (\v1 v2 ->
                    if v1 == v2 then
                        1

                    else
                        0
                )
                >> Tuple.pair (Plus 4)
                |> Just


walk : Int -> State -> State
walk i state =
    let
        action =
            getAction i state.array |> Debug.log "action"

        moperation =
            operation action
    in
    case moperation of
        Nothing ->
            state

        Just stateF ->
            let
                ( index, nextState ) =
                    stateF state

                newIndex =
                    case index of
                        Plus x ->
                            i + x

                        Constant x ->
                            x

                _ =
                    Debug.log "offset" index
            in
            walk newIndex nextState
