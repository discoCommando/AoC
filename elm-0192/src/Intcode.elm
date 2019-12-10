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
    | SetRelativeBase Cell


type Mode
    = Position
    | Immediate
    | Relative


nextMode : Mode -> List Mode
nextMode m =
    m
        :: (case m of
                Position ->
                    nextMode Immediate

                Immediate ->
                    nextMode Relative

                Relative ->
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

        Relative ->
            2


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

        9 ->
            let
                _ =
                    Debug.log "aaa" ( [ value, opcode ], [ mode1, mode2, mode3 ] )

                _ =
                    Debug.log "bbb"
                        (getCell mode1 (index + 1) array)
            in
            SetRelativeBase
                (getCell mode1 (index + 1) array)

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


extendState : Int -> State -> State
extendState i s =
    Array.repeat (i - Array.length s.array) 0
        |> Array.append s.array
        |> (\a -> { s | array = a })


get : Cell -> State -> Int
get { mode, index } state =
    case mode of
        Immediate ->
            index

        Position ->
            Array.get index state.array |> Maybe.withDefault 0

        Relative ->
            Array.get (state.relativeBase + index) state.array |> Maybe.withDefault 0



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
            get c1 state

        v2 =
            get c2 state

        vout =
            out.index

        extendedState =
            extendState vout state
    in
    { extendedState | array = Array.set vout (op v1 v2) extendedState.array }


jumpIf : Cell -> Cell -> (Int -> Bool) -> State -> Index
jumpIf c out op state =
    let
        v1 =
            get c state

        v2 =
            get out state
    in
    if op v1 then
        Constant v2

    else
        Plus 3


type Status
    = Going Int
    | Halted


type alias State =
    { array : Array Int
    , input : List Int
    , output : List Int
    , status : Status
    , relativeBase : Int
    }


type Index
    = Constant Int
    | Plus Int


type OperationResult
    = Halt_
    | Next Index State
    | WaitForInput


operation : Action -> State -> OperationResult
operation a state =
    case a of
        Add c1 c2 o ->
            binaryOperation c1 c2 o (+) state |> Next (Plus 4)

        Mult c1 c2 o ->
            binaryOperation c1 c2 o (*) state |> Next (Plus 4)

        Halt ->
            Halt_

        Input c ->
            case state.input of
                [] ->
                    WaitForInput

                input :: rest ->
                    let
                        v1 =
                            get c state
                    in
                    { state | array = Array.set v1 input state.array, input = rest }
                        |> Next (Plus 2)

        Output c ->
            let
                v1 =
                    get c state

                output =
                    v1
            in
            { state | output = output :: state.output }
                |> Next (Plus 2)

        IfTrue c o ->
            jumpIf c
                o
                (\i ->
                    if i == 0 then
                        False

                    else
                        True
                )
                state
                |> Next
                |> (|>) state

        IfFalse c o ->
            jumpIf c
                o
                (\i ->
                    if i == 0 then
                        True

                    else
                        False
                )
                state
                |> Next
                |> (|>) state

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
                state
                |> Next (Plus 4)

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
                state
                |> Next (Plus 4)

        SetRelativeBase c1 ->
            let
                _ =
                    Debug.log "dupa" ( state, c1 )
            in
            get c1 state
                |> (\v ->
                        { state | relativeBase = state.relativeBase + v }
                   )
                |> Next (Plus 2)


printState : State -> String
printState state =
    [ "relative base: "
    , state.relativeBase |> Debug.toString
    , "input: "
    , state.input |> Debug.toString
    , "output: "
    , state.output |> Debug.toString
    , "statys: "
    , state.status |> Debug.toString
    , "array: "
    , state.array |> Array.indexedMap Tuple.pair |> Debug.toString
    ]
        |> String.join " "


walk : State -> State
walk state =
    let
        _ =
            Debug.log "state" (printState state)
    in
    case state.status of
        Halted ->
            { state | output = state.input, input = [] }

        Going i ->
            let
                action =
                    getAction i state.array |> Debug.log "accction"

                moperation =
                    operation action state |> Debug.log "moperatoin"
            in
            case moperation of
                WaitForInput ->
                    state

                Halt_ ->
                    { state | status = Halted }

                Next index nextState ->
                    let
                        newIndex =
                            case index of
                                Plus x ->
                                    i + x

                                Constant x ->
                                    x
                    in
                    walk { nextState | status = Going newIndex }
