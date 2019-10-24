module E16 exposing (..)

import Array exposing (Array)
import Bitwise
import Char
import Dict exposing (Dict)
import Helpers as H
import Html exposing (..)
import Html.Events exposing (onClick)
import List
import List.Extra
import Parser exposing ((|.), (|=), Parser, andThen, keep, keyword, succeed, symbol)
import Regex
import Set exposing (Set)
import String


type OperationType
    = Addr --(add register) stores into register C the result of adding register A and register B.
    | Addi --(add immediate) stores into register C the result of adding register A and value B.
    | Mulr --(multiply register) stores into register C the result of multiplying register A and register B.
    | Muli --(multiply immediate) stores into register C the result of multiplying register A and value B.
    | Banr --(bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
    | Bani --(bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
    | Borr --(bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
    | Bori --(bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
    | Setr --(set register) copies the contents of register A into register C. (Input B is ignored.)
    | Seti --(set immediate) stores value A into register C. (Input B is ignored.)
    | Gtir --(greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
    | Gtri --(greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
    | Gtrr --(greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
    | Eqir --(equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
    | Eqri --(equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
    | Eqrr --(equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.


allOperations =
    [ Addr
    , Addi
    , Mulr
    , Muli
    , Banr
    , Bani
    , Borr
    , Bori
    , Setr
    , Seti
    , Gtir
    , Gtri
    , Gtrr
    , Eqir
    , Eqri
    , Eqrr
    ]


operationTypeToString : OperationType -> String
operationTypeToString operationType =
    case operationType of
        Addr ->
            "Addr"

        Addi ->
            "Addi"

        Mulr ->
            "Mulr"

        Muli ->
            "Muli"

        Banr ->
            "Banr"

        Bani ->
            "Bani"

        Borr ->
            "Borr"

        Bori ->
            "Bori"

        Setr ->
            "Setr"

        Seti ->
            "Seti"

        Gtir ->
            "Gtir"

        Gtri ->
            "Gtri"

        Gtrr ->
            "Gtrr"

        Eqir ->
            "Eqir"

        Eqri ->
            "Eqri"

        Eqrr ->
            "Eqrr"


stringToOperationType : String -> OperationType
stringToOperationType string =
    case string of
        "Addr" ->
            Addr

        "Addi" ->
            Addi

        "Mulr" ->
            Mulr

        "Muli" ->
            Muli

        "Banr" ->
            Banr

        "Bani" ->
            Bani

        "Borr" ->
            Borr

        "Bori" ->
            Bori

        "Setr" ->
            Setr

        "Seti" ->
            Seti

        "Gtir" ->
            Gtir

        "Gtri" ->
            Gtri

        "Gtrr" ->
            Gtrr

        "Eqir" ->
            Eqir

        "Eqri" ->
            Eqri

        "Eqrr" ->
            Eqrr

        _ ->
            Debug.crash ""


type alias Operation =
    { opcode : Int
    , inputA : Int
    , inputB : Int
    , outputA : Int
    }


type alias Registers =
    Dict Int Int


type alias Test =
    { before : Dict Int Int
    , operation : Operation
    , after : Dict Int Int
    }


parseRegisters : Parser (Dict Int Int)
parseRegisters =
    succeed (\a b c d -> [ a, b, c, d ] |> List.indexedMap (,) |> Dict.fromList)
        |. keyword "["
        |= H.pInt
        |. keyword ", "
        |= H.pInt
        |. keyword ", "
        |= H.pInt
        |. keyword ", "
        |= H.pInt
        |. keyword "]"


parseOperation : Parser Operation
parseOperation =
    succeed Operation
        |= H.pInt
        |. H.spaces
        |= H.pInt
        |. H.spaces
        |= H.pInt
        |. H.spaces
        |= H.pInt


parseTest : Parser Test
parseTest =
    succeed Test
        |. keyword "Before:"
        |. H.spaces
        |= parseRegisters
        |. Parser.symbol "\n"
        |= parseOperation
        |. Parser.symbol "\n"
        |. keyword "After:"
        |. H.spaces
        |= parseRegisters
        |. Parser.symbol "\n"
        |. Parser.symbol "\n"


getUnsafe : Int -> Dict Int Int -> Int
getUnsafe int intIntDict =
    intIntDict |> Dict.get int |> H.uM


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1
    else
        0


interpretOperation : Operation -> OperationType -> Dict Int Int -> Dict Int Int
interpretOperation { opcode, inputA, inputB, outputA } operationType initialRegister =
    case operationType of
        Addr ->
            initialRegister |> Dict.insert outputA (getUnsafe inputA initialRegister + getUnsafe inputB initialRegister)

        Addi ->
            initialRegister |> Dict.insert outputA (getUnsafe inputA initialRegister + inputB)

        Mulr ->
            initialRegister |> Dict.insert outputA (getUnsafe inputA initialRegister * getUnsafe inputB initialRegister)

        Muli ->
            initialRegister |> Dict.insert outputA (getUnsafe inputA initialRegister * inputB)

        Banr ->
            initialRegister |> Dict.insert outputA (Bitwise.and (getUnsafe inputA initialRegister) (getUnsafe inputB initialRegister))

        Bani ->
            initialRegister |> Dict.insert outputA (Bitwise.and (getUnsafe inputA initialRegister) inputB)

        Borr ->
            initialRegister |> Dict.insert outputA (Bitwise.or (getUnsafe inputA initialRegister) (getUnsafe inputB initialRegister))

        Bori ->
            initialRegister |> Dict.insert outputA (Bitwise.or (getUnsafe inputA initialRegister) inputB)

        Setr ->
            initialRegister |> Dict.insert outputA (getUnsafe inputA initialRegister)

        Seti ->
            initialRegister |> Dict.insert outputA inputA

        Gtir ->
            initialRegister |> Dict.insert outputA (boolToInt (inputA > getUnsafe inputB initialRegister))

        Gtri ->
            initialRegister |> Dict.insert outputA (boolToInt (getUnsafe inputA initialRegister > inputB))

        Gtrr ->
            initialRegister |> Dict.insert outputA (boolToInt (getUnsafe inputA initialRegister > getUnsafe inputB initialRegister))

        Eqir ->
            initialRegister |> Dict.insert outputA (boolToInt (inputA == getUnsafe inputB initialRegister))

        Eqri ->
            initialRegister |> Dict.insert outputA (boolToInt (getUnsafe inputA initialRegister == inputB))

        Eqrr ->
            initialRegister |> Dict.insert outputA (boolToInt (getUnsafe inputA initialRegister == getUnsafe inputB initialRegister))


isOperation : Test -> OperationType -> Bool
isOperation test operationType =
    interpretOperation test.operation operationType test.before == test.after


getOperations : Test -> List OperationType
getOperations test =
    allOperations |> List.filter (isOperation test)


test1 =
    Parser.run parseTest """Before: [3, 0, 0, 1]
0 3 0 2
After:  [3, 0, 1, 1]
""" |> Debug.log "test"


part1 =
    input
        |> Parser.run (Parser.repeat Parser.oneOrMore parseTest)
        |> H.uR
        |> List.filter (getOperations >> (\x -> List.length x >= 3))
        |> List.length
        |> Debug.log "answer1"


type alias State =
    { opcodes : Dict Int OperationType
    , registers : Dict Int Int
    }


type alias Candidates =
    Dict Int (Set OperationType)


workoutTypes : Dict Int (Set String) -> Dict String Int -> Dict Int OperationType
workoutTypes stringSetIntDict intStringDict =
    if Dict.isEmpty stringSetIntDict then
        intStringDict |> Dict.toList |> List.map (Tuple.mapFirst stringToOperationType) |> List.map (\( a, b ) -> ( b, a )) |> Dict.fromList
    else
        let
            singles =
                stringSetIntDict
                    |> Dict.filter (\_ s -> Set.size s == 1)
                    |> Dict.toList
                    |> List.map (Tuple.mapSecond (Set.toList >> H.at 0))

            removed =
                singles |> List.foldl (\( _, s ) dict -> dict |> Dict.map (\_ set -> set |> Set.remove s)) stringSetIntDict |> Dict.filter (\_ s -> Set.isEmpty s |> not)

            newIntStringDict =
                singles |> List.map (\( a, b ) -> ( b, a )) |> Dict.fromList |> Dict.union intStringDict
        in
        workoutTypes removed newIntStringDict


getInitialState : List Test -> Dict Int (Set String) -> State
getInitialState tests candidates =
    case tests of
        [] ->
            State
                (workoutTypes candidates Dict.empty)
                ([ 0, 0, 0, 0 ] |> List.indexedMap (,) |> Dict.fromList)

        test :: rest ->
            getInitialState rest
                (candidates
                    |> Dict.update test.operation.opcode
                        (\value ->
                            case value of
                                Nothing ->
                                    getOperations test |> List.map operationTypeToString |> Set.fromList |> Just

                                Just operations ->
                                    Just <| Set.intersect operations <| Set.fromList <| List.map operationTypeToString <| getOperations test
                        )
                )


runOperation : Operation -> State -> State
runOperation operation state =
    State state.opcodes (interpretOperation operation (state.opcodes |> Dict.get operation.opcode |> H.uM) state.registers)


test2 =
    inputmarek
        |> Parser.run (Parser.repeat Parser.oneOrMore parseTest)
        |> H.uR
        |> (\v -> getInitialState v Dict.empty)
        |> Debug.log "initialState"
        |> (\initialState ->
                inputlatermarek
                    |> String.lines
                    |> List.map (Parser.run parseOperation >> H.uR)
                    |> List.foldl runOperation initialState
           )
        |> Debug.log "finalState"


input : String
input =
    """Before: [3, 0, 0, 1]
0 3 0 2
After:  [3, 0, 1, 1]

Before: [2, 0, 0, 2]
4 0 3 1
After:  [2, 1, 0, 2]

Before: [0, 1, 1, 1]
14 0 0 2
After:  [0, 1, 0, 1]

Before: [3, 0, 1, 1]
11 0 0 3
After:  [3, 0, 1, 1]

Before: [1, 2, 2, 0]
9 0 2 1
After:  [1, 0, 2, 0]

Before: [0, 2, 3, 3]
11 2 2 3
After:  [0, 2, 3, 1]

Before: [2, 0, 1, 2]
4 0 3 2
After:  [2, 0, 1, 2]

Before: [2, 0, 2, 2]
6 3 3 3
After:  [2, 0, 2, 0]

Before: [0, 1, 2, 2]
1 1 2 3
After:  [0, 1, 2, 0]

Before: [0, 3, 0, 0]
14 0 0 0
After:  [0, 3, 0, 0]

Before: [2, 2, 0, 2]
4 0 3 3
After:  [2, 2, 0, 1]

Before: [2, 3, 2, 1]
13 2 2 0
After:  [1, 3, 2, 1]

Before: [2, 1, 1, 2]
4 0 3 1
After:  [2, 1, 1, 2]

Before: [1, 2, 2, 1]
9 0 2 0
After:  [0, 2, 2, 1]

Before: [2, 2, 0, 2]
4 0 3 1
After:  [2, 1, 0, 2]

Before: [1, 0, 2, 3]
9 0 2 1
After:  [1, 0, 2, 3]

Before: [1, 1, 3, 2]
10 1 3 1
After:  [1, 0, 3, 2]

Before: [0, 2, 1, 3]
14 0 0 1
After:  [0, 0, 1, 3]

Before: [2, 1, 2, 1]
11 0 0 1
After:  [2, 1, 2, 1]

Before: [1, 1, 2, 2]
3 2 3 1
After:  [1, 2, 2, 2]

Before: [3, 0, 2, 3]
8 1 0 1
After:  [3, 0, 2, 3]

Before: [1, 3, 2, 2]
9 0 2 2
After:  [1, 3, 0, 2]

Before: [2, 0, 3, 2]
4 0 3 2
After:  [2, 0, 1, 2]

Before: [2, 1, 1, 2]
10 1 3 1
After:  [2, 0, 1, 2]

Before: [2, 1, 2, 3]
1 1 2 2
After:  [2, 1, 0, 3]

Before: [3, 1, 2, 1]
1 1 2 3
After:  [3, 1, 2, 0]

Before: [2, 2, 2, 3]
5 2 2 1
After:  [2, 2, 2, 3]

Before: [2, 0, 2, 2]
4 0 3 2
After:  [2, 0, 1, 2]

Before: [2, 3, 1, 1]
11 0 0 0
After:  [1, 3, 1, 1]

Before: [2, 3, 2, 2]
4 0 3 1
After:  [2, 1, 2, 2]

Before: [3, 1, 3, 0]
0 1 0 1
After:  [3, 1, 3, 0]

Before: [3, 1, 2, 3]
12 3 0 3
After:  [3, 1, 2, 1]

Before: [1, 0, 3, 1]
6 3 3 1
After:  [1, 0, 3, 1]

Before: [0, 1, 2, 3]
1 1 2 3
After:  [0, 1, 2, 0]

Before: [1, 2, 1, 3]
2 1 3 1
After:  [1, 0, 1, 3]

Before: [1, 2, 2, 3]
9 0 2 2
After:  [1, 2, 0, 3]

Before: [3, 3, 3, 2]
11 0 2 1
After:  [3, 1, 3, 2]

Before: [2, 1, 0, 2]
10 1 3 3
After:  [2, 1, 0, 0]

Before: [3, 3, 3, 3]
5 3 3 0
After:  [3, 3, 3, 3]

Before: [0, 3, 2, 0]
13 0 0 0
After:  [1, 3, 2, 0]

Before: [3, 0, 2, 2]
3 2 3 3
After:  [3, 0, 2, 2]

Before: [1, 3, 2, 1]
9 0 2 1
After:  [1, 0, 2, 1]

Before: [1, 1, 2, 3]
1 1 2 0
After:  [0, 1, 2, 3]

Before: [1, 1, 0, 2]
15 1 0 1
After:  [1, 1, 0, 2]

Before: [0, 1, 1, 3]
2 2 3 3
After:  [0, 1, 1, 0]

Before: [3, 1, 3, 3]
0 3 0 3
After:  [3, 1, 3, 3]

Before: [0, 0, 3, 3]
14 0 0 3
After:  [0, 0, 3, 0]

Before: [2, 1, 2, 2]
1 1 2 2
After:  [2, 1, 0, 2]

Before: [2, 1, 0, 2]
4 0 3 0
After:  [1, 1, 0, 2]

Before: [2, 1, 2, 1]
7 3 2 0
After:  [1, 1, 2, 1]

Before: [3, 1, 2, 2]
10 1 3 2
After:  [3, 1, 0, 2]

Before: [3, 1, 1, 3]
0 1 0 2
After:  [3, 1, 1, 3]

Before: [3, 0, 2, 1]
7 3 2 0
After:  [1, 0, 2, 1]

Before: [2, 2, 0, 2]
4 0 3 0
After:  [1, 2, 0, 2]

Before: [0, 3, 3, 3]
13 3 3 0
After:  [1, 3, 3, 3]

Before: [2, 1, 2, 3]
5 3 3 3
After:  [2, 1, 2, 3]

Before: [1, 1, 2, 1]
7 3 2 0
After:  [1, 1, 2, 1]

Before: [3, 0, 1, 3]
2 2 3 2
After:  [3, 0, 0, 3]

Before: [0, 2, 2, 1]
14 0 0 2
After:  [0, 2, 0, 1]

Before: [0, 3, 3, 1]
14 0 0 2
After:  [0, 3, 0, 1]

Before: [0, 1, 2, 2]
3 2 3 0
After:  [2, 1, 2, 2]

Before: [0, 3, 2, 0]
14 0 0 1
After:  [0, 0, 2, 0]

Before: [0, 1, 2, 2]
10 1 3 0
After:  [0, 1, 2, 2]

Before: [1, 3, 2, 1]
7 3 2 1
After:  [1, 1, 2, 1]

Before: [1, 0, 2, 3]
9 0 2 3
After:  [1, 0, 2, 0]

Before: [0, 3, 2, 2]
6 3 3 0
After:  [0, 3, 2, 2]

Before: [0, 0, 0, 3]
13 3 3 0
After:  [1, 0, 0, 3]

Before: [2, 0, 2, 2]
3 2 3 0
After:  [2, 0, 2, 2]

Before: [0, 0, 2, 2]
3 2 3 3
After:  [0, 0, 2, 2]

Before: [2, 2, 2, 1]
7 3 2 0
After:  [1, 2, 2, 1]

Before: [3, 3, 1, 3]
0 3 0 2
After:  [3, 3, 3, 3]

Before: [1, 1, 1, 1]
15 1 0 3
After:  [1, 1, 1, 1]

Before: [1, 2, 2, 0]
9 0 2 3
After:  [1, 2, 2, 0]

Before: [2, 2, 2, 1]
12 2 0 2
After:  [2, 2, 1, 1]

Before: [2, 1, 2, 2]
1 1 2 0
After:  [0, 1, 2, 2]

Before: [1, 0, 2, 2]
5 2 2 2
After:  [1, 0, 2, 2]

Before: [0, 0, 2, 2]
3 2 3 1
After:  [0, 2, 2, 2]

Before: [0, 1, 0, 2]
10 1 3 1
After:  [0, 0, 0, 2]

Before: [3, 1, 1, 3]
2 2 3 1
After:  [3, 0, 1, 3]

Before: [0, 2, 1, 0]
8 0 1 3
After:  [0, 2, 1, 0]

Before: [1, 1, 3, 3]
2 1 3 0
After:  [0, 1, 3, 3]

Before: [0, 0, 2, 2]
14 0 0 0
After:  [0, 0, 2, 2]

Before: [1, 2, 2, 3]
9 0 2 3
After:  [1, 2, 2, 0]

Before: [2, 2, 1, 3]
5 3 3 1
After:  [2, 3, 1, 3]

Before: [2, 2, 2, 2]
4 0 3 0
After:  [1, 2, 2, 2]

Before: [0, 0, 3, 0]
14 0 0 3
After:  [0, 0, 3, 0]

Before: [3, 2, 2, 0]
12 2 1 1
After:  [3, 1, 2, 0]

Before: [2, 1, 1, 2]
4 0 3 2
After:  [2, 1, 1, 2]

Before: [3, 2, 2, 3]
5 3 3 3
After:  [3, 2, 2, 3]

Before: [3, 2, 2, 2]
3 2 3 0
After:  [2, 2, 2, 2]

Before: [0, 0, 0, 1]
6 3 3 0
After:  [0, 0, 0, 1]

Before: [1, 1, 0, 0]
15 1 0 1
After:  [1, 1, 0, 0]

Before: [0, 0, 1, 1]
14 0 0 2
After:  [0, 0, 0, 1]

Before: [1, 3, 0, 3]
13 3 3 1
After:  [1, 1, 0, 3]

Before: [1, 1, 3, 1]
15 1 0 3
After:  [1, 1, 3, 1]

Before: [1, 1, 2, 1]
5 2 2 2
After:  [1, 1, 2, 1]

Before: [3, 2, 2, 1]
7 3 2 1
After:  [3, 1, 2, 1]

Before: [1, 1, 2, 0]
15 1 0 2
After:  [1, 1, 1, 0]

Before: [0, 0, 3, 0]
11 2 2 0
After:  [1, 0, 3, 0]

Before: [0, 2, 2, 3]
12 2 1 2
After:  [0, 2, 1, 3]

Before: [0, 0, 3, 2]
14 0 0 3
After:  [0, 0, 3, 0]

Before: [1, 3, 2, 3]
13 2 2 2
After:  [1, 3, 1, 3]

Before: [1, 1, 2, 1]
7 3 2 3
After:  [1, 1, 2, 1]

Before: [0, 1, 3, 0]
8 0 1 1
After:  [0, 0, 3, 0]

Before: [1, 0, 2, 2]
9 0 2 0
After:  [0, 0, 2, 2]

Before: [0, 1, 0, 3]
8 0 1 3
After:  [0, 1, 0, 0]

Before: [0, 2, 1, 3]
2 1 3 3
After:  [0, 2, 1, 0]

Before: [2, 3, 2, 3]
11 0 0 0
After:  [1, 3, 2, 3]

Before: [0, 0, 2, 1]
6 3 3 3
After:  [0, 0, 2, 0]

Before: [2, 2, 3, 2]
11 0 0 0
After:  [1, 2, 3, 2]

Before: [3, 1, 2, 1]
7 3 2 1
After:  [3, 1, 2, 1]

Before: [0, 2, 2, 1]
7 3 2 1
After:  [0, 1, 2, 1]

Before: [0, 1, 0, 3]
14 0 0 1
After:  [0, 0, 0, 3]

Before: [1, 1, 2, 1]
9 0 2 3
After:  [1, 1, 2, 0]

Before: [0, 3, 3, 3]
8 0 1 3
After:  [0, 3, 3, 0]

Before: [0, 3, 0, 3]
13 3 3 3
After:  [0, 3, 0, 1]

Before: [0, 2, 2, 3]
8 0 3 0
After:  [0, 2, 2, 3]

Before: [2, 3, 2, 1]
12 2 0 0
After:  [1, 3, 2, 1]

Before: [0, 3, 3, 0]
8 0 1 0
After:  [0, 3, 3, 0]

Before: [1, 1, 1, 2]
10 1 3 3
After:  [1, 1, 1, 0]

Before: [0, 3, 1, 1]
8 0 3 0
After:  [0, 3, 1, 1]

Before: [2, 1, 3, 3]
2 1 3 0
After:  [0, 1, 3, 3]

Before: [1, 1, 2, 3]
1 1 2 2
After:  [1, 1, 0, 3]

Before: [3, 3, 2, 3]
2 2 3 3
After:  [3, 3, 2, 0]

Before: [0, 1, 1, 3]
13 3 2 0
After:  [0, 1, 1, 3]

Before: [0, 2, 0, 2]
13 0 0 1
After:  [0, 1, 0, 2]

Before: [3, 1, 2, 3]
1 1 2 0
After:  [0, 1, 2, 3]

Before: [0, 3, 3, 3]
11 2 2 1
After:  [0, 1, 3, 3]

Before: [0, 2, 3, 2]
8 0 2 3
After:  [0, 2, 3, 0]

Before: [3, 1, 1, 0]
0 1 0 0
After:  [1, 1, 1, 0]

Before: [2, 1, 2, 1]
0 2 0 3
After:  [2, 1, 2, 2]

Before: [0, 1, 2, 3]
13 3 3 1
After:  [0, 1, 2, 3]

Before: [1, 0, 2, 0]
9 0 2 3
After:  [1, 0, 2, 0]

Before: [3, 1, 2, 0]
11 0 0 0
After:  [1, 1, 2, 0]

Before: [0, 2, 1, 0]
14 0 0 1
After:  [0, 0, 1, 0]

Before: [2, 1, 3, 2]
4 0 3 3
After:  [2, 1, 3, 1]

Before: [0, 2, 2, 3]
2 1 3 0
After:  [0, 2, 2, 3]

Before: [2, 3, 3, 1]
11 2 2 1
After:  [2, 1, 3, 1]

Before: [0, 2, 2, 1]
12 2 1 2
After:  [0, 2, 1, 1]

Before: [2, 3, 2, 2]
3 2 3 2
After:  [2, 3, 2, 2]

Before: [2, 3, 3, 2]
4 0 3 0
After:  [1, 3, 3, 2]

Before: [3, 1, 3, 3]
0 3 0 0
After:  [3, 1, 3, 3]

Before: [0, 2, 1, 3]
8 0 3 2
After:  [0, 2, 0, 3]

Before: [2, 1, 2, 2]
3 2 3 3
After:  [2, 1, 2, 2]

Before: [2, 2, 2, 2]
12 2 1 1
After:  [2, 1, 2, 2]

Before: [2, 1, 2, 2]
3 2 3 1
After:  [2, 2, 2, 2]

Before: [3, 0, 3, 3]
0 3 0 2
After:  [3, 0, 3, 3]

Before: [1, 0, 3, 0]
11 2 2 3
After:  [1, 0, 3, 1]

Before: [0, 2, 2, 0]
12 2 1 1
After:  [0, 1, 2, 0]

Before: [1, 1, 0, 0]
15 1 0 2
After:  [1, 1, 1, 0]

Before: [2, 1, 0, 2]
4 0 3 1
After:  [2, 1, 0, 2]

Before: [1, 2, 2, 3]
2 2 3 2
After:  [1, 2, 0, 3]

Before: [3, 1, 2, 3]
2 1 3 1
After:  [3, 0, 2, 3]

Before: [1, 1, 1, 3]
15 1 0 2
After:  [1, 1, 1, 3]

Before: [1, 2, 3, 0]
11 2 2 3
After:  [1, 2, 3, 1]

Before: [1, 1, 0, 2]
15 1 0 2
After:  [1, 1, 1, 2]

Before: [3, 1, 3, 2]
10 1 3 3
After:  [3, 1, 3, 0]

Before: [1, 1, 2, 2]
3 2 3 0
After:  [2, 1, 2, 2]

Before: [2, 1, 2, 1]
1 1 2 1
After:  [2, 0, 2, 1]

Before: [3, 1, 3, 1]
0 1 0 2
After:  [3, 1, 1, 1]

Before: [3, 1, 1, 2]
0 1 0 2
After:  [3, 1, 1, 2]

Before: [1, 3, 3, 1]
12 2 3 2
After:  [1, 3, 0, 1]

Before: [3, 1, 1, 2]
10 1 3 3
After:  [3, 1, 1, 0]

Before: [1, 1, 1, 2]
10 1 3 0
After:  [0, 1, 1, 2]

Before: [0, 1, 0, 2]
8 0 3 3
After:  [0, 1, 0, 0]

Before: [3, 0, 0, 3]
5 3 3 1
After:  [3, 3, 0, 3]

Before: [1, 1, 2, 2]
1 1 2 1
After:  [1, 0, 2, 2]

Before: [0, 0, 1, 1]
6 3 3 3
After:  [0, 0, 1, 0]

Before: [1, 2, 2, 2]
6 3 3 3
After:  [1, 2, 2, 0]

Before: [2, 3, 2, 1]
7 3 2 3
After:  [2, 3, 2, 1]

Before: [3, 3, 1, 1]
11 0 0 0
After:  [1, 3, 1, 1]

Before: [3, 0, 2, 1]
7 3 2 2
After:  [3, 0, 1, 1]

Before: [2, 0, 3, 2]
4 0 3 1
After:  [2, 1, 3, 2]

Before: [2, 3, 2, 1]
5 2 2 1
After:  [2, 2, 2, 1]

Before: [1, 1, 0, 2]
10 1 3 3
After:  [1, 1, 0, 0]

Before: [0, 3, 1, 3]
8 0 2 1
After:  [0, 0, 1, 3]

Before: [2, 0, 1, 2]
4 0 3 1
After:  [2, 1, 1, 2]

Before: [1, 3, 3, 3]
5 3 3 0
After:  [3, 3, 3, 3]

Before: [2, 2, 2, 2]
4 0 3 3
After:  [2, 2, 2, 1]

Before: [3, 1, 2, 0]
1 1 2 1
After:  [3, 0, 2, 0]

Before: [1, 3, 3, 1]
6 3 3 3
After:  [1, 3, 3, 0]

Before: [1, 1, 3, 1]
15 1 0 2
After:  [1, 1, 1, 1]

Before: [1, 1, 1, 1]
15 1 0 1
After:  [1, 1, 1, 1]

Before: [1, 3, 2, 3]
5 3 3 0
After:  [3, 3, 2, 3]

Before: [2, 0, 2, 1]
7 3 2 2
After:  [2, 0, 1, 1]

Before: [0, 1, 1, 2]
10 1 3 2
After:  [0, 1, 0, 2]

Before: [0, 2, 1, 3]
2 2 3 2
After:  [0, 2, 0, 3]

Before: [1, 1, 2, 0]
1 1 2 2
After:  [1, 1, 0, 0]

Before: [1, 0, 1, 3]
2 2 3 2
After:  [1, 0, 0, 3]

Before: [3, 1, 0, 3]
0 3 0 3
After:  [3, 1, 0, 3]

Before: [0, 2, 0, 1]
8 0 3 0
After:  [0, 2, 0, 1]

Before: [3, 1, 1, 0]
11 0 0 1
After:  [3, 1, 1, 0]

Before: [1, 2, 2, 2]
6 3 3 1
After:  [1, 0, 2, 2]

Before: [1, 2, 1, 3]
2 2 3 3
After:  [1, 2, 1, 0]

Before: [0, 3, 3, 2]
14 0 0 1
After:  [0, 0, 3, 2]

Before: [2, 2, 2, 1]
7 3 2 2
After:  [2, 2, 1, 1]

Before: [2, 2, 0, 2]
4 0 3 2
After:  [2, 2, 1, 2]

Before: [0, 1, 1, 3]
8 0 2 1
After:  [0, 0, 1, 3]

Before: [2, 1, 1, 2]
10 1 3 3
After:  [2, 1, 1, 0]

Before: [3, 0, 2, 3]
13 3 2 1
After:  [3, 0, 2, 3]

Before: [0, 3, 2, 1]
14 0 0 3
After:  [0, 3, 2, 0]

Before: [2, 3, 2, 1]
5 2 2 0
After:  [2, 3, 2, 1]

Before: [2, 1, 2, 3]
2 2 3 0
After:  [0, 1, 2, 3]

Before: [0, 1, 3, 2]
10 1 3 1
After:  [0, 0, 3, 2]

Before: [1, 1, 2, 0]
9 0 2 2
After:  [1, 1, 0, 0]

Before: [0, 3, 0, 3]
14 0 0 0
After:  [0, 3, 0, 3]

Before: [0, 2, 0, 3]
2 1 3 0
After:  [0, 2, 0, 3]

Before: [0, 0, 2, 1]
13 2 2 3
After:  [0, 0, 2, 1]

Before: [0, 1, 2, 2]
1 1 2 2
After:  [0, 1, 0, 2]

Before: [3, 1, 3, 3]
2 1 3 3
After:  [3, 1, 3, 0]

Before: [0, 3, 2, 0]
5 2 2 0
After:  [2, 3, 2, 0]

Before: [1, 2, 2, 1]
9 0 2 1
After:  [1, 0, 2, 1]

Before: [1, 3, 2, 3]
2 2 3 3
After:  [1, 3, 2, 0]

Before: [2, 3, 0, 2]
4 0 3 3
After:  [2, 3, 0, 1]

Before: [2, 2, 3, 1]
12 2 3 0
After:  [0, 2, 3, 1]

Before: [0, 1, 2, 2]
13 0 0 1
After:  [0, 1, 2, 2]

Before: [1, 0, 2, 1]
7 3 2 2
After:  [1, 0, 1, 1]

Before: [0, 3, 3, 0]
14 0 0 2
After:  [0, 3, 0, 0]

Before: [3, 0, 3, 1]
11 0 2 0
After:  [1, 0, 3, 1]

Before: [1, 2, 0, 1]
6 3 3 0
After:  [0, 2, 0, 1]

Before: [2, 1, 2, 1]
1 1 2 0
After:  [0, 1, 2, 1]

Before: [3, 3, 2, 3]
0 3 0 0
After:  [3, 3, 2, 3]

Before: [0, 3, 2, 3]
5 3 3 0
After:  [3, 3, 2, 3]

Before: [0, 1, 3, 3]
14 0 0 3
After:  [0, 1, 3, 0]

Before: [0, 0, 3, 1]
13 0 0 1
After:  [0, 1, 3, 1]

Before: [0, 3, 2, 2]
3 2 3 2
After:  [0, 3, 2, 2]

Before: [2, 2, 2, 3]
2 1 3 0
After:  [0, 2, 2, 3]

Before: [2, 2, 2, 3]
12 2 1 0
After:  [1, 2, 2, 3]

Before: [0, 0, 1, 0]
14 0 0 3
After:  [0, 0, 1, 0]

Before: [1, 1, 1, 2]
15 1 0 0
After:  [1, 1, 1, 2]

Before: [2, 0, 2, 1]
7 3 2 0
After:  [1, 0, 2, 1]

Before: [3, 0, 2, 1]
11 0 0 3
After:  [3, 0, 2, 1]

Before: [3, 1, 2, 2]
10 1 3 3
After:  [3, 1, 2, 0]

Before: [0, 1, 2, 3]
13 3 1 0
After:  [0, 1, 2, 3]

Before: [0, 1, 0, 3]
14 0 0 3
After:  [0, 1, 0, 0]

Before: [0, 3, 2, 3]
8 0 2 3
After:  [0, 3, 2, 0]

Before: [1, 3, 2, 2]
9 0 2 0
After:  [0, 3, 2, 2]

Before: [1, 0, 2, 1]
9 0 2 1
After:  [1, 0, 2, 1]

Before: [0, 1, 0, 3]
2 1 3 1
After:  [0, 0, 0, 3]

Before: [0, 0, 2, 3]
14 0 0 0
After:  [0, 0, 2, 3]

Before: [2, 2, 2, 1]
12 2 1 0
After:  [1, 2, 2, 1]

Before: [3, 3, 3, 1]
0 3 0 3
After:  [3, 3, 3, 1]

Before: [3, 3, 0, 1]
0 3 0 1
After:  [3, 1, 0, 1]

Before: [0, 1, 2, 3]
1 1 2 0
After:  [0, 1, 2, 3]

Before: [3, 3, 3, 3]
0 3 0 3
After:  [3, 3, 3, 3]

Before: [1, 0, 2, 2]
3 2 3 1
After:  [1, 2, 2, 2]

Before: [1, 2, 2, 2]
12 2 1 3
After:  [1, 2, 2, 1]

Before: [0, 1, 1, 2]
10 1 3 1
After:  [0, 0, 1, 2]

Before: [1, 0, 2, 2]
9 0 2 2
After:  [1, 0, 0, 2]

Before: [1, 1, 2, 2]
10 1 3 1
After:  [1, 0, 2, 2]

Before: [3, 3, 1, 1]
0 3 0 1
After:  [3, 1, 1, 1]

Before: [2, 1, 2, 2]
12 2 0 0
After:  [1, 1, 2, 2]

Before: [2, 0, 3, 2]
4 0 3 0
After:  [1, 0, 3, 2]

Before: [2, 2, 3, 2]
4 0 3 3
After:  [2, 2, 3, 1]

Before: [2, 3, 3, 3]
13 3 3 2
After:  [2, 3, 1, 3]

Before: [3, 3, 2, 1]
7 3 2 1
After:  [3, 1, 2, 1]

Before: [0, 3, 2, 0]
8 0 2 2
After:  [0, 3, 0, 0]

Before: [0, 1, 3, 2]
13 2 1 1
After:  [0, 0, 3, 2]

Before: [0, 1, 2, 2]
13 0 0 0
After:  [1, 1, 2, 2]

Before: [2, 2, 1, 3]
2 1 3 0
After:  [0, 2, 1, 3]

Before: [1, 1, 1, 0]
15 1 0 1
After:  [1, 1, 1, 0]

Before: [2, 2, 2, 2]
3 2 3 2
After:  [2, 2, 2, 2]

Before: [1, 1, 2, 3]
15 1 0 3
After:  [1, 1, 2, 1]

Before: [2, 0, 3, 2]
4 0 3 3
After:  [2, 0, 3, 1]

Before: [3, 0, 3, 3]
0 3 0 3
After:  [3, 0, 3, 3]

Before: [0, 2, 1, 1]
8 0 2 1
After:  [0, 0, 1, 1]

Before: [1, 3, 2, 0]
5 2 2 3
After:  [1, 3, 2, 2]

Before: [0, 2, 2, 1]
6 3 3 1
After:  [0, 0, 2, 1]

Before: [1, 1, 3, 0]
15 1 0 1
After:  [1, 1, 3, 0]

Before: [2, 0, 1, 3]
5 3 3 0
After:  [3, 0, 1, 3]

Before: [1, 1, 2, 1]
15 1 0 1
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 2]
3 2 3 0
After:  [2, 1, 2, 2]

Before: [1, 1, 3, 2]
15 1 0 3
After:  [1, 1, 3, 1]

Before: [0, 2, 1, 1]
14 0 0 0
After:  [0, 2, 1, 1]

Before: [0, 0, 2, 0]
5 2 2 1
After:  [0, 2, 2, 0]

Before: [0, 1, 2, 3]
2 1 3 2
After:  [0, 1, 0, 3]

Before: [3, 1, 2, 2]
3 2 3 0
After:  [2, 1, 2, 2]

Before: [1, 2, 2, 2]
9 0 2 2
After:  [1, 2, 0, 2]

Before: [2, 2, 2, 1]
5 2 2 1
After:  [2, 2, 2, 1]

Before: [1, 1, 2, 1]
7 3 2 1
After:  [1, 1, 2, 1]

Before: [3, 0, 0, 1]
0 3 0 3
After:  [3, 0, 0, 1]

Before: [3, 2, 3, 3]
12 3 0 3
After:  [3, 2, 3, 1]

Before: [3, 1, 2, 2]
10 1 3 1
After:  [3, 0, 2, 2]

Before: [1, 0, 1, 3]
13 3 2 1
After:  [1, 0, 1, 3]

Before: [0, 0, 0, 2]
14 0 0 1
After:  [0, 0, 0, 2]

Before: [3, 0, 3, 1]
0 3 0 1
After:  [3, 1, 3, 1]

Before: [1, 1, 2, 3]
1 1 2 3
After:  [1, 1, 2, 0]

Before: [3, 1, 2, 1]
1 1 2 1
After:  [3, 0, 2, 1]

Before: [3, 3, 0, 3]
13 3 3 2
After:  [3, 3, 1, 3]

Before: [3, 3, 2, 3]
2 2 3 2
After:  [3, 3, 0, 3]

Before: [1, 2, 2, 1]
9 0 2 2
After:  [1, 2, 0, 1]

Before: [3, 1, 0, 1]
0 3 0 1
After:  [3, 1, 0, 1]

Before: [2, 2, 3, 2]
4 0 3 0
After:  [1, 2, 3, 2]

Before: [2, 0, 2, 1]
7 3 2 1
After:  [2, 1, 2, 1]

Before: [1, 1, 2, 2]
9 0 2 2
After:  [1, 1, 0, 2]

Before: [0, 1, 3, 2]
6 3 3 2
After:  [0, 1, 0, 2]

Before: [3, 3, 0, 2]
6 3 3 3
After:  [3, 3, 0, 0]

Before: [3, 1, 3, 1]
11 0 2 2
After:  [3, 1, 1, 1]

Before: [0, 1, 2, 1]
7 3 2 3
After:  [0, 1, 2, 1]

Before: [1, 2, 2, 1]
7 3 2 1
After:  [1, 1, 2, 1]

Before: [3, 1, 0, 2]
10 1 3 2
After:  [3, 1, 0, 2]

Before: [0, 1, 3, 2]
14 0 0 2
After:  [0, 1, 0, 2]

Before: [0, 3, 0, 2]
14 0 0 1
After:  [0, 0, 0, 2]

Before: [0, 1, 2, 1]
1 1 2 3
After:  [0, 1, 2, 0]

Before: [1, 1, 2, 1]
9 0 2 1
After:  [1, 0, 2, 1]

Before: [0, 1, 2, 2]
10 1 3 2
After:  [0, 1, 0, 2]

Before: [1, 0, 2, 2]
3 2 3 2
After:  [1, 0, 2, 2]

Before: [2, 3, 3, 1]
12 2 3 3
After:  [2, 3, 3, 0]

Before: [2, 0, 3, 1]
12 2 3 0
After:  [0, 0, 3, 1]

Before: [2, 1, 2, 1]
7 3 2 2
After:  [2, 1, 1, 1]

Before: [3, 3, 2, 2]
3 2 3 0
After:  [2, 3, 2, 2]

Before: [0, 1, 0, 1]
8 0 1 1
After:  [0, 0, 0, 1]

Before: [1, 1, 2, 1]
6 3 3 2
After:  [1, 1, 0, 1]

Before: [0, 1, 2, 2]
1 1 2 0
After:  [0, 1, 2, 2]

Before: [3, 3, 1, 3]
2 2 3 2
After:  [3, 3, 0, 3]

Before: [3, 2, 1, 3]
5 3 3 1
After:  [3, 3, 1, 3]

Before: [2, 1, 2, 2]
4 0 3 0
After:  [1, 1, 2, 2]

Before: [3, 3, 2, 2]
13 2 2 2
After:  [3, 3, 1, 2]

Before: [2, 1, 3, 2]
4 0 3 2
After:  [2, 1, 1, 2]

Before: [3, 0, 2, 2]
3 2 3 1
After:  [3, 2, 2, 2]

Before: [2, 0, 1, 2]
4 0 3 3
After:  [2, 0, 1, 1]

Before: [3, 2, 2, 0]
5 2 2 3
After:  [3, 2, 2, 2]

Before: [0, 0, 3, 3]
5 3 3 3
After:  [0, 0, 3, 3]

Before: [0, 1, 0, 3]
13 3 1 0
After:  [0, 1, 0, 3]

Before: [2, 2, 3, 2]
4 0 3 2
After:  [2, 2, 1, 2]

Before: [0, 3, 0, 1]
14 0 0 2
After:  [0, 3, 0, 1]

Before: [3, 3, 3, 0]
11 0 0 2
After:  [3, 3, 1, 0]

Before: [0, 3, 3, 3]
14 0 0 1
After:  [0, 0, 3, 3]

Before: [2, 2, 1, 1]
6 3 3 2
After:  [2, 2, 0, 1]

Before: [3, 2, 0, 3]
5 3 3 0
After:  [3, 2, 0, 3]

Before: [2, 2, 2, 3]
2 1 3 1
After:  [2, 0, 2, 3]

Before: [2, 3, 2, 2]
3 2 3 1
After:  [2, 2, 2, 2]

Before: [0, 1, 2, 1]
7 3 2 1
After:  [0, 1, 2, 1]

Before: [0, 1, 2, 2]
3 2 3 1
After:  [0, 2, 2, 2]

Before: [3, 1, 2, 0]
0 1 0 2
After:  [3, 1, 1, 0]

Before: [2, 2, 2, 0]
0 2 0 1
After:  [2, 2, 2, 0]

Before: [1, 1, 3, 0]
15 1 0 0
After:  [1, 1, 3, 0]

Before: [0, 3, 0, 3]
5 3 3 0
After:  [3, 3, 0, 3]

Before: [3, 3, 3, 3]
0 3 0 0
After:  [3, 3, 3, 3]

Before: [3, 0, 2, 0]
5 2 2 2
After:  [3, 0, 2, 0]

Before: [2, 3, 2, 2]
6 3 3 0
After:  [0, 3, 2, 2]

Before: [1, 0, 2, 3]
9 0 2 0
After:  [0, 0, 2, 3]

Before: [2, 3, 2, 0]
5 2 2 1
After:  [2, 2, 2, 0]

Before: [3, 3, 3, 1]
12 2 3 3
After:  [3, 3, 3, 0]

Before: [1, 1, 1, 3]
2 2 3 2
After:  [1, 1, 0, 3]

Before: [3, 1, 1, 2]
11 0 0 2
After:  [3, 1, 1, 2]

Before: [1, 1, 0, 2]
15 1 0 3
After:  [1, 1, 0, 1]

Before: [1, 1, 1, 1]
15 1 0 2
After:  [1, 1, 1, 1]

Before: [0, 1, 0, 3]
2 1 3 2
After:  [0, 1, 0, 3]

Before: [2, 0, 2, 2]
3 2 3 2
After:  [2, 0, 2, 2]

Before: [0, 3, 3, 3]
11 2 2 3
After:  [0, 3, 3, 1]

Before: [0, 1, 2, 2]
3 2 3 3
After:  [0, 1, 2, 2]

Before: [0, 3, 3, 0]
8 0 1 1
After:  [0, 0, 3, 0]

Before: [3, 1, 3, 2]
10 1 3 1
After:  [3, 0, 3, 2]

Before: [3, 2, 2, 3]
12 2 1 2
After:  [3, 2, 1, 3]

Before: [3, 2, 2, 0]
5 2 2 0
After:  [2, 2, 2, 0]

Before: [3, 1, 1, 2]
10 1 3 1
After:  [3, 0, 1, 2]

Before: [3, 0, 3, 1]
11 0 2 2
After:  [3, 0, 1, 1]

Before: [3, 1, 0, 3]
0 1 0 1
After:  [3, 1, 0, 3]

Before: [0, 1, 2, 1]
7 3 2 2
After:  [0, 1, 1, 1]

Before: [2, 1, 2, 0]
12 2 0 1
After:  [2, 1, 2, 0]

Before: [0, 1, 0, 2]
14 0 0 2
After:  [0, 1, 0, 2]

Before: [2, 3, 2, 1]
0 2 0 3
After:  [2, 3, 2, 2]

Before: [1, 1, 0, 2]
10 1 3 1
After:  [1, 0, 0, 2]

Before: [3, 1, 2, 3]
13 3 3 2
After:  [3, 1, 1, 3]

Before: [2, 3, 2, 3]
0 2 0 1
After:  [2, 2, 2, 3]

Before: [3, 1, 2, 3]
1 1 2 3
After:  [3, 1, 2, 0]

Before: [2, 2, 1, 2]
4 0 3 0
After:  [1, 2, 1, 2]

Before: [0, 0, 3, 1]
14 0 0 0
After:  [0, 0, 3, 1]

Before: [3, 2, 3, 0]
11 0 2 3
After:  [3, 2, 3, 1]

Before: [2, 1, 1, 1]
11 0 0 1
After:  [2, 1, 1, 1]

Before: [3, 2, 2, 2]
12 2 1 2
After:  [3, 2, 1, 2]

Before: [0, 1, 3, 2]
10 1 3 2
After:  [0, 1, 0, 2]

Before: [1, 1, 2, 2]
15 1 0 1
After:  [1, 1, 2, 2]

Before: [0, 0, 3, 3]
8 0 2 3
After:  [0, 0, 3, 0]

Before: [2, 0, 0, 0]
11 0 0 1
After:  [2, 1, 0, 0]

Before: [3, 2, 3, 3]
0 3 0 2
After:  [3, 2, 3, 3]

Before: [2, 2, 2, 2]
4 0 3 2
After:  [2, 2, 1, 2]

Before: [1, 1, 1, 3]
15 1 0 0
After:  [1, 1, 1, 3]

Before: [0, 1, 2, 0]
8 0 1 0
After:  [0, 1, 2, 0]

Before: [3, 1, 3, 1]
12 2 3 0
After:  [0, 1, 3, 1]

Before: [1, 1, 2, 2]
3 2 3 2
After:  [1, 1, 2, 2]

Before: [3, 1, 2, 2]
3 2 3 1
After:  [3, 2, 2, 2]

Before: [1, 1, 0, 1]
15 1 0 0
After:  [1, 1, 0, 1]

Before: [2, 3, 1, 3]
2 2 3 3
After:  [2, 3, 1, 0]

Before: [2, 0, 3, 1]
12 2 3 2
After:  [2, 0, 0, 1]

Before: [2, 0, 0, 3]
8 1 0 0
After:  [0, 0, 0, 3]

Before: [1, 1, 2, 0]
15 1 0 0
After:  [1, 1, 2, 0]

Before: [3, 0, 0, 0]
8 2 0 3
After:  [3, 0, 0, 0]

Before: [2, 0, 2, 2]
0 2 0 3
After:  [2, 0, 2, 2]

Before: [2, 0, 2, 2]
4 0 3 3
After:  [2, 0, 2, 1]

Before: [0, 2, 2, 1]
7 3 2 2
After:  [0, 2, 1, 1]

Before: [2, 2, 0, 1]
6 3 3 1
After:  [2, 0, 0, 1]

Before: [2, 1, 1, 2]
10 1 3 2
After:  [2, 1, 0, 2]

Before: [2, 0, 1, 3]
2 2 3 0
After:  [0, 0, 1, 3]

Before: [2, 0, 0, 2]
4 0 3 3
After:  [2, 0, 0, 1]

Before: [1, 2, 2, 2]
3 2 3 0
After:  [2, 2, 2, 2]

Before: [0, 3, 2, 1]
8 0 1 0
After:  [0, 3, 2, 1]

Before: [0, 2, 2, 3]
8 0 3 2
After:  [0, 2, 0, 3]

Before: [0, 2, 2, 0]
5 2 2 3
After:  [0, 2, 2, 2]

Before: [3, 0, 3, 1]
12 2 3 0
After:  [0, 0, 3, 1]

Before: [0, 2, 1, 3]
2 1 3 0
After:  [0, 2, 1, 3]

Before: [1, 1, 2, 2]
1 1 2 0
After:  [0, 1, 2, 2]

Before: [2, 0, 2, 0]
0 2 0 2
After:  [2, 0, 2, 0]

Before: [0, 1, 3, 2]
10 1 3 3
After:  [0, 1, 3, 0]

Before: [1, 1, 2, 0]
15 1 0 3
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 3]
1 1 2 0
After:  [0, 1, 2, 3]

Before: [1, 3, 1, 3]
2 2 3 3
After:  [1, 3, 1, 0]

Before: [2, 3, 2, 0]
12 2 0 1
After:  [2, 1, 2, 0]

Before: [3, 0, 3, 3]
12 3 0 2
After:  [3, 0, 1, 3]

Before: [3, 1, 1, 3]
2 2 3 0
After:  [0, 1, 1, 3]

Before: [3, 2, 2, 3]
2 1 3 3
After:  [3, 2, 2, 0]

Before: [2, 2, 2, 1]
7 3 2 3
After:  [2, 2, 2, 1]

Before: [1, 1, 1, 1]
6 2 3 3
After:  [1, 1, 1, 0]

Before: [2, 3, 2, 3]
5 2 2 2
After:  [2, 3, 2, 3]

Before: [2, 3, 2, 2]
4 0 3 0
After:  [1, 3, 2, 2]

Before: [0, 1, 2, 1]
13 2 2 2
After:  [0, 1, 1, 1]

Before: [1, 3, 2, 2]
9 0 2 3
After:  [1, 3, 2, 0]

Before: [0, 0, 0, 3]
14 0 0 1
After:  [0, 0, 0, 3]

Before: [0, 2, 0, 0]
14 0 0 2
After:  [0, 2, 0, 0]

Before: [1, 3, 2, 1]
9 0 2 3
After:  [1, 3, 2, 0]

Before: [3, 2, 2, 3]
13 3 2 1
After:  [3, 0, 2, 3]

Before: [1, 0, 2, 0]
9 0 2 0
After:  [0, 0, 2, 0]

Before: [0, 3, 2, 1]
5 2 2 1
After:  [0, 2, 2, 1]

Before: [3, 1, 2, 3]
1 1 2 1
After:  [3, 0, 2, 3]

Before: [0, 3, 1, 2]
14 0 0 2
After:  [0, 3, 0, 2]

Before: [2, 1, 1, 3]
13 3 2 1
After:  [2, 0, 1, 3]

Before: [0, 0, 2, 2]
6 3 3 3
After:  [0, 0, 2, 0]

Before: [0, 1, 3, 0]
8 0 2 0
After:  [0, 1, 3, 0]

Before: [1, 1, 1, 2]
15 1 0 2
After:  [1, 1, 1, 2]

Before: [0, 1, 3, 2]
8 0 2 3
After:  [0, 1, 3, 0]

Before: [1, 1, 3, 3]
15 1 0 0
After:  [1, 1, 3, 3]

Before: [2, 2, 1, 2]
11 0 1 0
After:  [1, 2, 1, 2]

Before: [1, 0, 2, 2]
3 2 3 0
After:  [2, 0, 2, 2]

Before: [0, 0, 2, 2]
3 2 3 0
After:  [2, 0, 2, 2]

Before: [2, 2, 2, 2]
3 2 3 0
After:  [2, 2, 2, 2]

Before: [2, 2, 2, 0]
12 2 0 0
After:  [1, 2, 2, 0]

Before: [0, 1, 1, 2]
10 1 3 0
After:  [0, 1, 1, 2]

Before: [0, 0, 0, 2]
14 0 0 3
After:  [0, 0, 0, 0]

Before: [1, 3, 3, 3]
13 3 3 2
After:  [1, 3, 1, 3]

Before: [3, 2, 2, 3]
5 3 3 0
After:  [3, 2, 2, 3]

Before: [1, 1, 3, 1]
15 1 0 1
After:  [1, 1, 3, 1]

Before: [2, 3, 2, 2]
4 0 3 2
After:  [2, 3, 1, 2]

Before: [3, 3, 2, 3]
0 3 0 1
After:  [3, 3, 2, 3]

Before: [1, 1, 2, 0]
9 0 2 3
After:  [1, 1, 2, 0]

Before: [1, 2, 2, 0]
9 0 2 2
After:  [1, 2, 0, 0]

Before: [0, 3, 2, 1]
7 3 2 1
After:  [0, 1, 2, 1]

Before: [2, 0, 2, 3]
5 2 2 0
After:  [2, 0, 2, 3]

Before: [2, 0, 0, 2]
4 0 3 2
After:  [2, 0, 1, 2]

Before: [3, 3, 1, 1]
0 3 0 2
After:  [3, 3, 1, 1]

Before: [0, 3, 3, 0]
14 0 0 1
After:  [0, 0, 3, 0]

Before: [3, 2, 1, 1]
0 3 0 1
After:  [3, 1, 1, 1]

Before: [1, 1, 2, 2]
15 1 0 0
After:  [1, 1, 2, 2]

Before: [2, 2, 1, 2]
6 3 3 0
After:  [0, 2, 1, 2]

Before: [2, 1, 2, 2]
4 0 3 2
After:  [2, 1, 1, 2]

Before: [1, 1, 1, 2]
10 1 3 1
After:  [1, 0, 1, 2]

Before: [1, 1, 0, 0]
15 1 0 0
After:  [1, 1, 0, 0]

Before: [1, 2, 2, 3]
9 0 2 1
After:  [1, 0, 2, 3]

Before: [2, 0, 1, 2]
4 0 3 0
After:  [1, 0, 1, 2]

Before: [1, 3, 3, 3]
12 3 2 3
After:  [1, 3, 3, 1]

Before: [3, 1, 1, 2]
0 1 0 0
After:  [1, 1, 1, 2]

Before: [3, 0, 2, 1]
0 3 0 3
After:  [3, 0, 2, 1]

Before: [3, 3, 0, 1]
0 3 0 0
After:  [1, 3, 0, 1]

Before: [1, 3, 3, 2]
6 3 3 2
After:  [1, 3, 0, 2]

Before: [0, 2, 1, 0]
8 0 2 2
After:  [0, 2, 0, 0]

Before: [2, 2, 2, 0]
12 2 0 2
After:  [2, 2, 1, 0]

Before: [2, 1, 2, 3]
5 2 2 0
After:  [2, 1, 2, 3]

Before: [0, 0, 1, 3]
5 3 3 3
After:  [0, 0, 1, 3]

Before: [1, 0, 2, 3]
13 2 2 2
After:  [1, 0, 1, 3]

Before: [1, 1, 2, 3]
15 1 0 2
After:  [1, 1, 1, 3]

Before: [2, 2, 2, 2]
3 2 3 3
After:  [2, 2, 2, 2]

Before: [1, 1, 0, 1]
15 1 0 2
After:  [1, 1, 1, 1]

Before: [1, 1, 2, 3]
15 1 0 0
After:  [1, 1, 2, 3]

Before: [1, 1, 0, 3]
2 1 3 1
After:  [1, 0, 0, 3]

Before: [2, 0, 2, 1]
12 2 0 1
After:  [2, 1, 2, 1]

Before: [0, 2, 1, 2]
8 0 1 1
After:  [0, 0, 1, 2]

Before: [1, 1, 2, 3]
1 1 2 1
After:  [1, 0, 2, 3]

Before: [0, 3, 2, 3]
2 2 3 0
After:  [0, 3, 2, 3]

Before: [0, 0, 2, 3]
13 3 3 2
After:  [0, 0, 1, 3]

Before: [2, 1, 3, 2]
10 1 3 1
After:  [2, 0, 3, 2]

Before: [1, 1, 2, 3]
2 1 3 3
After:  [1, 1, 2, 0]

Before: [0, 0, 3, 2]
14 0 0 2
After:  [0, 0, 0, 2]

Before: [0, 2, 2, 1]
7 3 2 3
After:  [0, 2, 2, 1]

Before: [1, 0, 3, 2]
11 2 2 3
After:  [1, 0, 3, 1]

Before: [3, 2, 3, 3]
11 0 0 1
After:  [3, 1, 3, 3]

Before: [0, 3, 2, 2]
14 0 0 2
After:  [0, 3, 0, 2]

Before: [3, 2, 1, 1]
6 3 3 2
After:  [3, 2, 0, 1]

Before: [0, 2, 2, 3]
2 2 3 0
After:  [0, 2, 2, 3]

Before: [1, 2, 1, 2]
6 3 3 3
After:  [1, 2, 1, 0]

Before: [2, 1, 2, 1]
7 3 2 1
After:  [2, 1, 2, 1]

Before: [1, 1, 1, 2]
10 1 3 2
After:  [1, 1, 0, 2]

Before: [0, 0, 1, 1]
14 0 0 1
After:  [0, 0, 1, 1]

Before: [3, 3, 3, 1]
11 0 2 1
After:  [3, 1, 3, 1]

Before: [2, 1, 2, 2]
12 2 0 2
After:  [2, 1, 1, 2]

Before: [1, 2, 2, 1]
7 3 2 0
After:  [1, 2, 2, 1]

Before: [1, 2, 2, 2]
9 0 2 1
After:  [1, 0, 2, 2]

Before: [3, 3, 2, 1]
7 3 2 3
After:  [3, 3, 2, 1]

Before: [2, 1, 1, 2]
6 3 3 3
After:  [2, 1, 1, 0]

Before: [2, 2, 1, 3]
2 2 3 2
After:  [2, 2, 0, 3]

Before: [3, 2, 2, 2]
3 2 3 1
After:  [3, 2, 2, 2]

Before: [2, 2, 2, 1]
0 2 0 0
After:  [2, 2, 2, 1]

Before: [0, 1, 3, 1]
14 0 0 1
After:  [0, 0, 3, 1]

Before: [2, 0, 3, 1]
8 1 0 0
After:  [0, 0, 3, 1]

Before: [2, 3, 0, 2]
4 0 3 2
After:  [2, 3, 1, 2]

Before: [2, 0, 2, 2]
3 2 3 1
After:  [2, 2, 2, 2]

Before: [0, 0, 3, 2]
8 0 2 2
After:  [0, 0, 0, 2]

Before: [3, 1, 1, 2]
10 1 3 2
After:  [3, 1, 0, 2]

Before: [3, 2, 2, 1]
7 3 2 2
After:  [3, 2, 1, 1]

Before: [2, 2, 2, 2]
4 0 3 1
After:  [2, 1, 2, 2]

Before: [1, 1, 2, 2]
9 0 2 1
After:  [1, 0, 2, 2]

Before: [3, 0, 2, 2]
5 2 2 1
After:  [3, 2, 2, 2]

Before: [0, 2, 0, 1]
14 0 0 0
After:  [0, 2, 0, 1]

Before: [2, 2, 2, 1]
5 2 2 0
After:  [2, 2, 2, 1]

Before: [2, 1, 2, 3]
1 1 2 1
After:  [2, 0, 2, 3]

Before: [2, 3, 0, 2]
4 0 3 1
After:  [2, 1, 0, 2]

Before: [2, 3, 2, 1]
7 3 2 0
After:  [1, 3, 2, 1]

Before: [2, 0, 2, 2]
12 2 0 0
After:  [1, 0, 2, 2]

Before: [1, 2, 2, 2]
3 2 3 2
After:  [1, 2, 2, 2]

Before: [0, 0, 2, 1]
7 3 2 0
After:  [1, 0, 2, 1]

Before: [2, 2, 3, 3]
11 2 2 3
After:  [2, 2, 3, 1]

Before: [3, 3, 2, 0]
5 2 2 2
After:  [3, 3, 2, 0]

Before: [3, 2, 2, 2]
3 2 3 2
After:  [3, 2, 2, 2]

Before: [2, 3, 2, 2]
4 0 3 3
After:  [2, 3, 2, 1]

Before: [2, 1, 1, 2]
11 0 0 1
After:  [2, 1, 1, 2]

Before: [0, 0, 2, 1]
7 3 2 2
After:  [0, 0, 1, 1]

Before: [0, 2, 1, 0]
14 0 0 3
After:  [0, 2, 1, 0]

Before: [1, 1, 2, 0]
15 1 0 1
After:  [1, 1, 2, 0]

Before: [0, 1, 2, 3]
1 1 2 1
After:  [0, 0, 2, 3]

Before: [0, 3, 1, 0]
14 0 0 3
After:  [0, 3, 1, 0]

Before: [0, 2, 3, 2]
8 0 3 0
After:  [0, 2, 3, 2]

Before: [0, 1, 3, 3]
13 0 0 0
After:  [1, 1, 3, 3]

Before: [2, 1, 2, 0]
1 1 2 0
After:  [0, 1, 2, 0]

Before: [3, 0, 1, 1]
11 0 0 0
After:  [1, 0, 1, 1]

Before: [0, 2, 1, 3]
14 0 0 3
After:  [0, 2, 1, 0]

Before: [0, 2, 2, 1]
14 0 0 0
After:  [0, 2, 2, 1]

Before: [0, 2, 3, 0]
14 0 0 1
After:  [0, 0, 3, 0]

Before: [2, 2, 2, 2]
3 2 3 1
After:  [2, 2, 2, 2]

Before: [3, 0, 2, 1]
7 3 2 3
After:  [3, 0, 2, 1]

Before: [2, 2, 2, 1]
6 3 3 0
After:  [0, 2, 2, 1]

Before: [2, 1, 3, 2]
10 1 3 2
After:  [2, 1, 0, 2]

Before: [2, 1, 2, 2]
5 2 2 3
After:  [2, 1, 2, 2]

Before: [3, 2, 1, 1]
0 3 0 3
After:  [3, 2, 1, 1]

Before: [2, 1, 2, 3]
1 1 2 3
After:  [2, 1, 2, 0]

Before: [2, 3, 2, 0]
0 2 0 1
After:  [2, 2, 2, 0]

Before: [3, 0, 3, 1]
11 0 0 3
After:  [3, 0, 3, 1]

Before: [2, 3, 1, 1]
6 2 3 2
After:  [2, 3, 0, 1]

Before: [2, 0, 3, 2]
11 2 2 0
After:  [1, 0, 3, 2]

Before: [0, 1, 1, 2]
10 1 3 3
After:  [0, 1, 1, 0]

Before: [0, 1, 3, 3]
12 3 2 2
After:  [0, 1, 1, 3]

Before: [0, 1, 1, 2]
14 0 0 1
After:  [0, 0, 1, 2]

Before: [0, 1, 2, 3]
1 1 2 2
After:  [0, 1, 0, 3]

Before: [1, 1, 2, 2]
1 1 2 3
After:  [1, 1, 2, 0]

Before: [0, 1, 0, 1]
14 0 0 3
After:  [0, 1, 0, 0]

Before: [2, 0, 0, 1]
6 3 3 1
After:  [2, 0, 0, 1]

Before: [1, 2, 1, 3]
13 3 2 3
After:  [1, 2, 1, 0]

Before: [2, 1, 2, 1]
6 3 3 3
After:  [2, 1, 2, 0]

Before: [0, 1, 0, 2]
6 3 3 3
After:  [0, 1, 0, 0]

Before: [3, 1, 0, 2]
8 2 0 0
After:  [0, 1, 0, 2]

Before: [1, 1, 3, 3]
12 3 2 0
After:  [1, 1, 3, 3]

Before: [0, 1, 2, 1]
5 2 2 3
After:  [0, 1, 2, 2]

Before: [1, 0, 3, 0]
11 2 2 0
After:  [1, 0, 3, 0]

Before: [3, 3, 2, 1]
5 2 2 0
After:  [2, 3, 2, 1]

Before: [0, 2, 2, 3]
14 0 0 3
After:  [0, 2, 2, 0]

Before: [3, 0, 1, 3]
2 2 3 3
After:  [3, 0, 1, 0]

Before: [2, 3, 3, 2]
4 0 3 3
After:  [2, 3, 3, 1]

Before: [1, 1, 3, 2]
15 1 0 2
After:  [1, 1, 1, 2]

Before: [1, 1, 2, 2]
9 0 2 0
After:  [0, 1, 2, 2]

Before: [2, 1, 2, 2]
10 1 3 0
After:  [0, 1, 2, 2]

Before: [3, 1, 2, 0]
1 1 2 3
After:  [3, 1, 2, 0]

Before: [0, 1, 1, 3]
14 0 0 1
After:  [0, 0, 1, 3]

Before: [1, 1, 2, 3]
13 3 1 0
After:  [0, 1, 2, 3]

Before: [1, 1, 1, 2]
6 3 3 1
After:  [1, 0, 1, 2]

Before: [2, 1, 2, 2]
5 2 2 1
After:  [2, 2, 2, 2]

Before: [0, 2, 2, 0]
14 0 0 0
After:  [0, 2, 2, 0]

Before: [0, 0, 2, 2]
8 0 2 2
After:  [0, 0, 0, 2]

Before: [0, 1, 2, 0]
14 0 0 3
After:  [0, 1, 2, 0]

Before: [1, 3, 2, 2]
9 0 2 1
After:  [1, 0, 2, 2]

Before: [1, 2, 2, 0]
12 2 1 0
After:  [1, 2, 2, 0]

Before: [2, 1, 3, 2]
4 0 3 0
After:  [1, 1, 3, 2]

Before: [0, 1, 2, 1]
1 1 2 1
After:  [0, 0, 2, 1]

Before: [3, 0, 3, 3]
11 0 0 0
After:  [1, 0, 3, 3]

Before: [0, 3, 2, 1]
7 3 2 0
After:  [1, 3, 2, 1]

Before: [3, 1, 1, 3]
12 3 0 0
After:  [1, 1, 1, 3]

Before: [0, 2, 2, 1]
7 3 2 0
After:  [1, 2, 2, 1]

Before: [1, 1, 3, 2]
10 1 3 3
After:  [1, 1, 3, 0]

Before: [1, 1, 2, 1]
1 1 2 1
After:  [1, 0, 2, 1]

Before: [2, 0, 1, 2]
6 3 3 0
After:  [0, 0, 1, 2]

Before: [2, 0, 2, 0]
0 2 0 3
After:  [2, 0, 2, 2]

Before: [0, 2, 3, 3]
13 3 3 0
After:  [1, 2, 3, 3]

Before: [1, 1, 0, 3]
15 1 0 3
After:  [1, 1, 0, 1]

Before: [2, 0, 1, 1]
11 0 0 2
After:  [2, 0, 1, 1]

Before: [2, 3, 0, 2]
6 3 3 2
After:  [2, 3, 0, 2]

Before: [3, 1, 1, 2]
10 1 3 0
After:  [0, 1, 1, 2]

Before: [2, 3, 0, 1]
6 3 3 1
After:  [2, 0, 0, 1]

Before: [3, 1, 2, 1]
1 1 2 0
After:  [0, 1, 2, 1]

Before: [1, 1, 0, 2]
10 1 3 0
After:  [0, 1, 0, 2]

Before: [2, 1, 2, 1]
1 1 2 2
After:  [2, 1, 0, 1]

Before: [2, 1, 0, 2]
10 1 3 2
After:  [2, 1, 0, 2]

Before: [1, 1, 2, 2]
15 1 0 2
After:  [1, 1, 1, 2]

Before: [2, 3, 0, 2]
4 0 3 0
After:  [1, 3, 0, 2]

Before: [1, 1, 2, 1]
1 1 2 0
After:  [0, 1, 2, 1]

Before: [3, 1, 2, 3]
13 3 3 0
After:  [1, 1, 2, 3]

Before: [3, 0, 2, 3]
0 3 0 1
After:  [3, 3, 2, 3]

Before: [0, 1, 2, 1]
1 1 2 0
After:  [0, 1, 2, 1]

Before: [2, 2, 1, 2]
4 0 3 3
After:  [2, 2, 1, 1]

Before: [3, 3, 0, 3]
11 0 0 0
After:  [1, 3, 0, 3]

Before: [3, 3, 2, 2]
3 2 3 1
After:  [3, 2, 2, 2]

Before: [1, 3, 2, 3]
2 2 3 0
After:  [0, 3, 2, 3]

Before: [1, 2, 2, 3]
9 0 2 0
After:  [0, 2, 2, 3]

Before: [2, 1, 2, 0]
1 1 2 2
After:  [2, 1, 0, 0]

Before: [3, 1, 2, 1]
1 1 2 2
After:  [3, 1, 0, 1]

Before: [3, 1, 0, 2]
10 1 3 1
After:  [3, 0, 0, 2]

Before: [3, 0, 3, 1]
12 2 3 2
After:  [3, 0, 0, 1]

Before: [1, 0, 2, 2]
9 0 2 3
After:  [1, 0, 2, 0]

Before: [0, 2, 2, 3]
5 3 3 1
After:  [0, 3, 2, 3]

Before: [2, 0, 2, 1]
12 2 0 2
After:  [2, 0, 1, 1]

Before: [2, 3, 1, 2]
11 0 0 3
After:  [2, 3, 1, 1]

Before: [0, 2, 2, 2]
14 0 0 0
After:  [0, 2, 2, 2]

Before: [1, 0, 3, 0]
11 2 2 2
After:  [1, 0, 1, 0]

Before: [0, 1, 2, 3]
14 0 0 0
After:  [0, 1, 2, 3]

Before: [2, 2, 3, 1]
6 3 3 3
After:  [2, 2, 3, 0]

Before: [1, 1, 2, 2]
10 1 3 0
After:  [0, 1, 2, 2]

Before: [3, 1, 2, 2]
6 3 3 1
After:  [3, 0, 2, 2]

Before: [1, 2, 3, 3]
13 3 1 2
After:  [1, 2, 0, 3]

Before: [1, 2, 1, 3]
2 2 3 2
After:  [1, 2, 0, 3]

Before: [3, 1, 0, 2]
10 1 3 3
After:  [3, 1, 0, 0]

Before: [3, 1, 3, 3]
11 2 2 3
After:  [3, 1, 3, 1]

Before: [2, 2, 1, 2]
11 0 0 2
After:  [2, 2, 1, 2]

Before: [3, 1, 3, 2]
10 1 3 0
After:  [0, 1, 3, 2]

Before: [3, 0, 2, 2]
3 2 3 2
After:  [3, 0, 2, 2]

Before: [1, 1, 0, 3]
2 1 3 0
After:  [0, 1, 0, 3]

Before: [2, 1, 2, 2]
1 1 2 3
After:  [2, 1, 2, 0]

Before: [2, 2, 2, 3]
2 2 3 2
After:  [2, 2, 0, 3]

Before: [3, 0, 2, 1]
7 3 2 1
After:  [3, 1, 2, 1]

Before: [1, 1, 2, 3]
9 0 2 0
After:  [0, 1, 2, 3]

Before: [3, 2, 2, 3]
2 1 3 2
After:  [3, 2, 0, 3]

Before: [2, 1, 2, 2]
4 0 3 1
After:  [2, 1, 2, 2]

Before: [2, 0, 2, 2]
4 0 3 0
After:  [1, 0, 2, 2]

Before: [2, 3, 1, 2]
4 0 3 2
After:  [2, 3, 1, 2]

Before: [0, 2, 1, 0]
8 0 2 3
After:  [0, 2, 1, 0]

Before: [3, 0, 1, 1]
6 2 3 3
After:  [3, 0, 1, 0]

Before: [1, 2, 2, 0]
12 2 1 2
After:  [1, 2, 1, 0]

Before: [1, 3, 2, 1]
9 0 2 2
After:  [1, 3, 0, 1]

Before: [1, 1, 1, 3]
15 1 0 1
After:  [1, 1, 1, 3]

Before: [0, 0, 0, 3]
8 0 3 2
After:  [0, 0, 0, 3]

Before: [3, 1, 2, 0]
1 1 2 2
After:  [3, 1, 0, 0]

Before: [2, 1, 2, 2]
10 1 3 2
After:  [2, 1, 0, 2]

Before: [1, 1, 0, 1]
15 1 0 1
After:  [1, 1, 0, 1]

Before: [1, 1, 3, 0]
13 2 1 0
After:  [0, 1, 3, 0]

Before: [0, 0, 2, 1]
7 3 2 1
After:  [0, 1, 2, 1]

Before: [2, 1, 3, 0]
11 0 0 3
After:  [2, 1, 3, 1]

Before: [3, 1, 3, 0]
0 1 0 3
After:  [3, 1, 3, 1]

Before: [1, 1, 3, 3]
13 2 1 1
After:  [1, 0, 3, 3]

Before: [1, 3, 2, 3]
9 0 2 3
After:  [1, 3, 2, 0]

Before: [0, 3, 1, 3]
8 0 3 1
After:  [0, 0, 1, 3]

Before: [2, 1, 0, 2]
4 0 3 3
After:  [2, 1, 0, 1]

Before: [2, 0, 2, 2]
3 2 3 3
After:  [2, 0, 2, 2]

Before: [2, 3, 1, 0]
11 0 0 0
After:  [1, 3, 1, 0]

Before: [3, 2, 3, 2]
11 0 0 2
After:  [3, 2, 1, 2]

Before: [2, 1, 2, 2]
4 0 3 3
After:  [2, 1, 2, 1]

Before: [3, 0, 3, 3]
5 3 3 1
After:  [3, 3, 3, 3]

Before: [2, 2, 2, 1]
7 3 2 1
After:  [2, 1, 2, 1]

Before: [2, 1, 2, 2]
3 2 3 2
After:  [2, 1, 2, 2]

Before: [1, 1, 2, 1]
7 3 2 2
After:  [1, 1, 1, 1]

Before: [2, 0, 2, 2]
5 2 2 0
After:  [2, 0, 2, 2]

Before: [2, 1, 3, 2]
4 0 3 1
After:  [2, 1, 3, 2]

Before: [0, 2, 2, 2]
3 2 3 3
After:  [0, 2, 2, 2]

Before: [0, 3, 2, 2]
14 0 0 0
After:  [0, 3, 2, 2]

Before: [2, 1, 3, 2]
10 1 3 0
After:  [0, 1, 3, 2]

Before: [3, 1, 2, 1]
7 3 2 0
After:  [1, 1, 2, 1]

Before: [2, 2, 1, 2]
4 0 3 2
After:  [2, 2, 1, 2]

Before: [3, 2, 1, 1]
11 0 0 2
After:  [3, 2, 1, 1]

Before: [0, 3, 3, 1]
13 0 0 1
After:  [0, 1, 3, 1]

Before: [3, 0, 2, 3]
13 3 2 2
After:  [3, 0, 0, 3]

Before: [0, 0, 0, 1]
6 3 3 2
After:  [0, 0, 0, 1]

Before: [2, 3, 1, 2]
4 0 3 1
After:  [2, 1, 1, 2]

Before: [2, 3, 2, 3]
0 2 0 3
After:  [2, 3, 2, 2]

Before: [1, 3, 2, 0]
9 0 2 0
After:  [0, 3, 2, 0]

Before: [0, 1, 0, 2]
6 3 3 0
After:  [0, 1, 0, 2]

Before: [0, 1, 2, 1]
1 1 2 2
After:  [0, 1, 0, 1]

Before: [1, 3, 2, 1]
7 3 2 0
After:  [1, 3, 2, 1]

Before: [0, 3, 2, 1]
8 0 2 1
After:  [0, 0, 2, 1]

Before: [2, 2, 0, 3]
5 3 3 1
After:  [2, 3, 0, 3]

Before: [1, 1, 2, 2]
3 2 3 3
After:  [1, 1, 2, 2]

Before: [0, 0, 3, 2]
11 2 2 1
After:  [0, 1, 3, 2]

Before: [0, 3, 1, 3]
2 2 3 3
After:  [0, 3, 1, 0]

Before: [3, 3, 3, 3]
11 2 0 0
After:  [1, 3, 3, 3]

Before: [2, 0, 0, 2]
4 0 3 0
After:  [1, 0, 0, 2]

Before: [0, 1, 2, 3]
2 2 3 1
After:  [0, 0, 2, 3]

Before: [2, 3, 2, 1]
7 3 2 2
After:  [2, 3, 1, 1]

Before: [3, 1, 2, 2]
10 1 3 0
After:  [0, 1, 2, 2]

Before: [0, 2, 3, 3]
14 0 0 3
After:  [0, 2, 3, 0]

Before: [3, 2, 2, 3]
13 3 1 1
After:  [3, 0, 2, 3]

Before: [1, 1, 2, 2]
9 0 2 3
After:  [1, 1, 2, 0]

Before: [0, 2, 1, 3]
14 0 0 2
After:  [0, 2, 0, 3]

Before: [1, 0, 2, 1]
7 3 2 3
After:  [1, 0, 2, 1]

Before: [0, 3, 2, 1]
7 3 2 3
After:  [0, 3, 2, 1]

Before: [0, 1, 3, 2]
11 2 2 0
After:  [1, 1, 3, 2]

Before: [0, 0, 2, 0]
13 0 0 2
After:  [0, 0, 1, 0]

Before: [3, 1, 1, 3]
11 0 0 2
After:  [3, 1, 1, 3]

Before: [3, 2, 1, 3]
0 3 0 3
After:  [3, 2, 1, 3]

Before: [1, 2, 2, 0]
9 0 2 0
After:  [0, 2, 2, 0]

Before: [3, 0, 0, 3]
0 3 0 0
After:  [3, 0, 0, 3]

Before: [1, 0, 1, 3]
2 2 3 0
After:  [0, 0, 1, 3]

Before: [0, 0, 2, 1]
7 3 2 3
After:  [0, 0, 2, 1]

Before: [1, 1, 2, 2]
15 1 0 3
After:  [1, 1, 2, 1]

Before: [2, 1, 1, 3]
2 1 3 0
After:  [0, 1, 1, 3]

Before: [0, 2, 0, 2]
8 0 3 0
After:  [0, 2, 0, 2]

Before: [3, 1, 0, 3]
0 3 0 0
After:  [3, 1, 0, 3]

Before: [2, 3, 2, 3]
2 2 3 1
After:  [2, 0, 2, 3]

Before: [2, 1, 2, 3]
0 2 0 0
After:  [2, 1, 2, 3]

Before: [2, 0, 2, 3]
8 1 0 3
After:  [2, 0, 2, 0]

Before: [1, 1, 2, 3]
2 2 3 1
After:  [1, 0, 2, 3]

Before: [0, 0, 2, 2]
8 0 3 1
After:  [0, 0, 2, 2]

Before: [1, 0, 2, 1]
7 3 2 0
After:  [1, 0, 2, 1]

Before: [1, 3, 2, 3]
9 0 2 2
After:  [1, 3, 0, 3]

Before: [1, 1, 3, 3]
11 2 2 1
After:  [1, 1, 3, 3]

Before: [1, 1, 0, 3]
15 1 0 1
After:  [1, 1, 0, 3]

Before: [2, 3, 3, 2]
4 0 3 1
After:  [2, 1, 3, 2]

Before: [2, 1, 0, 1]
6 3 3 0
After:  [0, 1, 0, 1]

Before: [1, 1, 2, 2]
10 1 3 2
After:  [1, 1, 0, 2]

Before: [2, 0, 2, 1]
7 3 2 3
After:  [2, 0, 2, 1]

Before: [2, 3, 2, 2]
13 2 2 3
After:  [2, 3, 2, 1]

Before: [0, 1, 1, 0]
14 0 0 0
After:  [0, 1, 1, 0]

Before: [0, 1, 3, 2]
14 0 0 1
After:  [0, 0, 3, 2]

Before: [0, 3, 2, 3]
8 0 1 2
After:  [0, 3, 0, 3]

Before: [2, 3, 1, 3]
2 2 3 0
After:  [0, 3, 1, 3]

Before: [0, 0, 1, 1]
8 0 2 0
After:  [0, 0, 1, 1]

Before: [0, 1, 0, 2]
10 1 3 0
After:  [0, 1, 0, 2]

Before: [2, 3, 3, 3]
5 3 3 3
After:  [2, 3, 3, 3]

Before: [0, 3, 2, 1]
7 3 2 2
After:  [0, 3, 1, 1]

Before: [1, 0, 2, 3]
2 2 3 1
After:  [1, 0, 2, 3]

Before: [2, 1, 2, 1]
7 3 2 3
After:  [2, 1, 2, 1]

Before: [3, 1, 1, 1]
0 1 0 0
After:  [1, 1, 1, 1]

Before: [2, 3, 0, 3]
5 3 3 3
After:  [2, 3, 0, 3]

Before: [2, 0, 2, 0]
13 2 2 0
After:  [1, 0, 2, 0]

Before: [3, 2, 2, 3]
13 3 2 2
After:  [3, 2, 0, 3]

Before: [2, 0, 2, 1]
0 2 0 2
After:  [2, 0, 2, 1]

Before: [3, 1, 2, 0]
1 1 2 0
After:  [0, 1, 2, 0]

Before: [1, 2, 2, 1]
9 0 2 3
After:  [1, 2, 2, 0]

Before: [3, 0, 1, 1]
0 3 0 0
After:  [1, 0, 1, 1]

Before: [3, 1, 3, 3]
0 1 0 2
After:  [3, 1, 1, 3]

Before: [0, 1, 1, 3]
2 1 3 1
After:  [0, 0, 1, 3]

Before: [0, 3, 3, 3]
5 3 3 1
After:  [0, 3, 3, 3]

Before: [3, 3, 2, 2]
13 2 2 3
After:  [3, 3, 2, 1]

Before: [1, 1, 2, 1]
1 1 2 3
After:  [1, 1, 2, 0]

Before: [1, 1, 3, 2]
15 1 0 0
After:  [1, 1, 3, 2]

Before: [1, 0, 2, 2]
5 2 2 1
After:  [1, 2, 2, 2]

Before: [2, 2, 1, 1]
11 0 1 1
After:  [2, 1, 1, 1]

Before: [0, 0, 3, 1]
14 0 0 1
After:  [0, 0, 3, 1]

Before: [2, 1, 2, 2]
10 1 3 1
After:  [2, 0, 2, 2]

Before: [0, 1, 0, 2]
10 1 3 3
After:  [0, 1, 0, 0]

Before: [3, 1, 2, 2]
1 1 2 3
After:  [3, 1, 2, 0]

Before: [1, 0, 2, 1]
9 0 2 3
After:  [1, 0, 2, 0]

Before: [2, 1, 2, 2]
1 1 2 1
After:  [2, 0, 2, 2]

Before: [2, 3, 1, 2]
4 0 3 0
After:  [1, 3, 1, 2]

Before: [2, 1, 3, 3]
5 3 3 3
After:  [2, 1, 3, 3]

Before: [2, 2, 3, 3]
2 1 3 0
After:  [0, 2, 3, 3]

Before: [1, 1, 3, 2]
15 1 0 1
After:  [1, 1, 3, 2]

Before: [2, 3, 2, 1]
7 3 2 1
After:  [2, 1, 2, 1]

Before: [1, 0, 2, 0]
9 0 2 2
After:  [1, 0, 0, 0]

Before: [3, 3, 2, 3]
5 2 2 1
After:  [3, 2, 2, 3]

Before: [3, 1, 2, 2]
1 1 2 1
After:  [3, 0, 2, 2]

Before: [0, 0, 2, 3]
2 2 3 3
After:  [0, 0, 2, 0]

Before: [3, 3, 1, 3]
2 2 3 0
After:  [0, 3, 1, 3]

Before: [2, 2, 2, 3]
0 2 0 1
After:  [2, 2, 2, 3]

Before: [3, 1, 2, 1]
0 1 0 2
After:  [3, 1, 1, 1]

Before: [3, 3, 2, 1]
7 3 2 0
After:  [1, 3, 2, 1]

Before: [0, 1, 2, 0]
1 1 2 2
After:  [0, 1, 0, 0]

Before: [0, 0, 1, 0]
14 0 0 0
After:  [0, 0, 1, 0]

Before: [0, 2, 2, 1]
12 2 1 3
After:  [0, 2, 2, 1]

Before: [3, 2, 3, 1]
6 3 3 3
After:  [3, 2, 3, 0]

Before: [1, 1, 2, 1]
1 1 2 2
After:  [1, 1, 0, 1]

Before: [2, 3, 1, 2]
4 0 3 3
After:  [2, 3, 1, 1]

Before: [2, 2, 1, 2]
4 0 3 1
After:  [2, 1, 1, 2]

Before: [2, 1, 1, 2]
4 0 3 0
After:  [1, 1, 1, 2]

"""


inputLater =
    """9 2 0 1
9 0 1 0
9 3 0 3
10 3 1 1
8 1 1 1
14 1 2 2
7 2 2 1
9 1 3 3
9 1 2 0
9 3 0 2
8 3 2 3
8 3 1 3
14 3 1 1
7 1 3 2
9 1 2 3
8 0 0 0
0 0 2 0
8 1 0 1
0 1 3 1
15 3 0 0
8 0 3 0
8 0 2 0
14 2 0 2
7 2 1 1
9 1 2 2
8 0 0 0
0 0 3 0
9 0 3 3
1 0 2 2
8 2 1 2
14 2 1 1
7 1 0 2
8 2 0 3
0 3 2 3
9 2 1 0
9 3 3 1
4 0 3 0
8 0 3 0
14 0 2 2
9 1 1 3
9 1 2 1
9 1 0 0
14 1 3 0
8 0 1 0
14 0 2 2
9 2 1 1
9 2 2 0
6 0 3 1
8 1 2 1
8 1 2 1
14 1 2 2
7 2 2 1
9 1 0 0
8 1 0 3
0 3 0 3
9 3 0 2
12 3 2 0
8 0 3 0
8 0 1 0
14 0 1 1
9 1 2 0
8 3 0 3
0 3 1 3
9 0 0 2
8 0 2 2
8 2 2 2
14 1 2 1
9 2 0 0
8 0 0 2
0 2 2 2
6 0 3 0
8 0 1 0
8 0 3 0
14 0 1 1
7 1 0 3
9 2 3 0
9 3 3 2
9 3 0 1
1 1 2 2
8 2 2 2
14 3 2 3
7 3 3 0
9 0 1 2
9 0 1 1
9 3 1 3
1 3 2 2
8 2 3 2
14 0 2 0
7 0 0 3
9 2 2 0
9 1 1 1
9 3 2 2
13 0 2 1
8 1 2 1
8 1 2 1
14 3 1 3
7 3 0 1
9 2 2 3
9 0 1 2
12 2 3 2
8 2 1 2
14 1 2 1
8 1 0 2
0 2 2 2
9 1 2 3
8 2 0 0
0 0 1 0
7 0 2 0
8 0 3 0
8 0 2 0
14 1 0 1
7 1 0 0
8 3 0 2
0 2 1 2
9 1 3 1
14 1 3 3
8 3 1 3
8 3 3 3
14 0 3 0
9 2 1 1
9 0 1 3
9 2 3 3
8 3 2 3
8 3 3 3
14 0 3 0
7 0 2 1
9 1 2 3
9 2 1 0
6 0 3 2
8 2 3 2
14 1 2 1
7 1 2 3
9 1 1 0
9 2 1 2
9 3 1 1
0 0 1 2
8 2 2 2
14 2 3 3
7 3 1 0
9 2 1 3
8 0 0 2
0 2 3 2
9 0 0 1
9 2 1 2
8 2 3 2
8 2 3 2
14 0 2 0
7 0 1 2
9 1 2 3
9 2 3 1
9 3 2 0
10 0 1 0
8 0 2 0
14 2 0 2
7 2 0 3
8 2 0 2
0 2 3 2
9 2 1 0
8 1 0 1
0 1 3 1
10 1 0 1
8 1 2 1
8 1 3 1
14 1 3 3
7 3 2 0
9 1 0 1
9 1 3 3
9 2 3 1
8 1 2 1
14 1 0 0
7 0 3 2
9 2 3 0
9 3 0 3
9 2 1 1
10 3 0 1
8 1 3 1
14 2 1 2
7 2 0 0
9 3 0 2
9 3 0 1
9 2 1 3
10 1 3 3
8 3 2 3
14 3 0 0
7 0 0 2
9 1 3 3
9 2 0 0
9 1 1 1
6 0 3 0
8 0 1 0
14 0 2 2
7 2 0 3
8 2 0 0
0 0 1 0
9 2 0 2
9 2 3 1
7 0 2 0
8 0 2 0
14 0 3 3
9 3 2 1
9 1 0 2
9 1 0 0
0 0 1 0
8 0 2 0
14 3 0 3
7 3 3 1
9 2 2 3
9 2 0 2
9 1 3 0
7 0 2 3
8 3 3 3
8 3 2 3
14 1 3 1
9 0 3 2
9 3 3 3
8 0 2 3
8 3 2 3
8 3 2 3
14 3 1 1
8 1 0 0
0 0 0 0
9 1 3 3
9 3 0 2
8 3 2 2
8 2 2 2
14 1 2 1
7 1 0 2
9 2 0 0
9 0 2 1
9 2 1 3
5 0 3 3
8 3 1 3
8 3 3 3
14 2 3 2
9 1 2 3
9 1 0 1
15 3 0 0
8 0 2 0
14 0 2 2
7 2 0 0
9 1 2 2
9 2 0 3
15 1 3 2
8 2 1 2
14 2 0 0
9 0 0 3
8 3 0 2
0 2 2 2
9 0 1 1
5 2 3 1
8 1 1 1
8 1 3 1
14 0 1 0
9 3 2 2
9 3 0 1
9 2 2 3
1 1 2 2
8 2 3 2
14 2 0 0
7 0 1 3
9 2 3 2
9 2 2 0
11 0 1 2
8 2 1 2
14 3 2 3
7 3 0 1
9 2 3 2
9 1 1 0
9 3 3 3
7 0 2 3
8 3 1 3
14 1 3 1
9 3 3 2
8 1 0 3
0 3 3 3
8 3 0 0
0 0 2 0
13 0 2 2
8 2 2 2
8 2 2 2
14 1 2 1
9 2 2 2
9 1 3 3
6 0 3 2
8 2 3 2
14 1 2 1
7 1 0 2
9 3 3 1
9 1 0 0
0 3 1 1
8 1 3 1
14 2 1 2
7 2 1 1
9 2 1 0
8 3 0 3
0 3 3 3
9 3 3 2
3 0 2 2
8 2 1 2
8 2 1 2
14 2 1 1
7 1 2 2
8 2 0 3
0 3 1 3
9 0 1 1
6 0 3 1
8 1 1 1
14 2 1 2
9 0 3 1
9 3 2 0
9 2 3 3
10 0 3 1
8 1 2 1
8 1 2 1
14 2 1 2
7 2 3 3
9 0 1 2
9 1 0 0
9 3 3 1
8 0 2 1
8 1 1 1
14 3 1 3
7 3 1 2
9 2 0 1
9 0 2 3
9 2 3 0
5 1 3 1
8 1 3 1
8 1 1 1
14 1 2 2
7 2 0 1
9 3 2 0
8 0 0 2
0 2 0 2
9 3 2 3
13 2 0 0
8 0 3 0
14 1 0 1
8 3 0 2
0 2 2 2
8 1 0 0
0 0 1 0
7 0 2 2
8 2 3 2
8 2 1 2
14 1 2 1
7 1 2 2
9 0 0 3
8 2 0 1
0 1 1 1
14 0 0 0
8 0 2 0
14 2 0 2
7 2 3 3
8 2 0 0
0 0 3 0
8 0 0 1
0 1 3 1
9 0 0 2
1 0 2 0
8 0 1 0
14 0 3 3
7 3 2 1
9 1 2 3
9 2 1 0
6 0 3 0
8 0 2 0
8 0 3 0
14 0 1 1
7 1 0 3
9 2 0 2
9 1 1 0
8 2 0 1
0 1 2 1
7 0 2 1
8 1 2 1
8 1 3 1
14 3 1 3
9 1 1 1
7 0 2 1
8 1 1 1
14 1 3 3
7 3 3 2
9 2 0 3
9 3 1 1
0 0 1 0
8 0 1 0
14 2 0 2
7 2 0 3
8 0 0 2
0 2 1 2
9 2 0 0
1 1 2 1
8 1 1 1
14 1 3 3
7 3 1 2
9 1 2 1
9 2 0 3
15 1 3 0
8 0 2 0
14 0 2 2
9 3 2 3
9 2 0 0
9 3 2 1
11 0 1 0
8 0 3 0
14 0 2 2
7 2 3 3
9 3 1 0
9 3 3 2
9 1 2 1
8 1 2 1
8 1 1 1
8 1 3 1
14 3 1 3
7 3 0 0
9 2 2 2
8 1 0 3
0 3 0 3
9 0 2 1
2 3 2 1
8 1 1 1
14 1 0 0
7 0 0 1
9 2 1 0
9 3 2 2
12 3 2 2
8 2 3 2
14 2 1 1
7 1 0 2
8 0 0 1
0 1 3 1
9 2 3 3
10 1 3 1
8 1 2 1
14 1 2 2
7 2 2 1
9 1 0 2
5 0 3 0
8 0 1 0
14 0 1 1
7 1 3 3
9 3 3 2
9 2 0 0
9 3 1 1
13 0 2 2
8 2 3 2
8 2 1 2
14 3 2 3
9 3 3 2
8 2 0 1
0 1 0 1
3 0 2 1
8 1 3 1
8 1 3 1
14 1 3 3
7 3 2 1
9 1 3 3
13 0 2 2
8 2 2 2
14 2 1 1
7 1 2 0
9 2 1 3
9 1 0 1
9 2 0 2
5 2 3 3
8 3 1 3
14 0 3 0
8 1 0 3
0 3 2 3
9 3 1 1
10 1 3 1
8 1 3 1
8 1 1 1
14 0 1 0
9 3 3 1
9 3 2 3
11 2 1 1
8 1 2 1
8 1 1 1
14 1 0 0
7 0 0 1
9 0 2 2
8 2 0 0
0 0 3 0
8 1 0 3
0 3 2 3
13 2 0 3
8 3 3 3
14 3 1 1
9 1 1 2
8 1 0 3
0 3 1 3
14 3 3 3
8 3 3 3
14 3 1 1
7 1 3 3
9 3 0 2
9 1 0 1
9 2 1 0
8 0 1 0
8 0 2 0
14 0 3 3
7 3 3 2
8 3 0 0
0 0 2 0
8 3 0 1
0 1 3 1
9 1 1 3
6 0 3 0
8 0 2 0
14 2 0 2
9 1 0 1
9 2 1 3
9 0 2 0
15 1 3 1
8 1 3 1
14 2 1 2
7 2 3 1
9 1 3 0
9 3 1 2
9 0 0 3
12 3 2 3
8 3 1 3
14 3 1 1
7 1 1 2
9 1 1 3
9 2 3 0
9 0 1 1
15 3 0 0
8 0 2 0
14 0 2 2
7 2 3 0
8 1 0 1
0 1 2 1
9 0 2 3
9 2 2 2
2 3 2 3
8 3 3 3
14 0 3 0
7 0 0 2
9 3 3 0
9 2 2 3
3 1 0 1
8 1 1 1
14 1 2 2
9 0 1 1
10 0 3 0
8 0 1 0
8 0 2 0
14 0 2 2
8 3 0 0
0 0 2 0
4 0 3 3
8 3 3 3
14 3 2 2
7 2 3 0
8 0 0 3
0 3 1 3
9 3 2 1
9 2 0 2
14 3 3 1
8 1 3 1
14 1 0 0
7 0 1 2
9 1 0 0
9 2 0 3
9 3 0 1
10 1 3 3
8 3 2 3
14 2 3 2
7 2 2 1
8 3 0 3
0 3 0 3
9 2 1 2
8 3 0 0
0 0 3 0
11 2 0 3
8 3 1 3
8 3 1 3
14 1 3 1
9 1 0 3
9 3 2 2
9 2 0 0
13 0 2 3
8 3 2 3
14 3 1 1
8 3 0 3
0 3 3 3
8 0 0 0
0 0 1 0
8 0 2 0
8 0 3 0
14 1 0 1
7 1 1 3
9 1 3 1
9 2 1 0
13 0 2 0
8 0 2 0
8 0 2 0
14 3 0 3
7 3 3 0
8 0 0 3
0 3 3 3
9 0 2 1
1 3 2 2
8 2 2 2
14 2 0 0
9 3 2 2
9 3 1 1
1 3 2 1
8 1 3 1
8 1 1 1
14 1 0 0
7 0 2 2
9 3 3 1
9 1 2 0
9 1 0 3
0 0 1 1
8 1 3 1
14 2 1 2
7 2 3 1
8 3 0 2
0 2 3 2
8 3 0 0
0 0 2 0
9 2 1 3
4 0 3 0
8 0 1 0
14 0 1 1
9 3 1 3
9 2 3 2
9 2 0 0
10 3 0 0
8 0 3 0
14 1 0 1
8 0 0 3
0 3 0 3
9 1 0 0
7 0 2 2
8 2 1 2
14 1 2 1
9 3 0 0
8 2 0 2
0 2 0 2
13 2 0 2
8 2 3 2
14 1 2 1
9 3 0 2
9 2 2 3
9 2 2 0
3 0 2 3
8 3 2 3
14 3 1 1
7 1 2 2
9 2 0 3
8 1 0 1
0 1 1 1
4 0 3 3
8 3 3 3
8 3 1 3
14 3 2 2
7 2 2 1
9 3 0 0
8 0 0 2
0 2 0 2
9 2 1 3
13 2 0 2
8 2 3 2
14 1 2 1
7 1 1 2
9 0 1 1
9 1 0 0
8 3 0 3
0 3 3 3
0 0 1 1
8 1 1 1
14 1 2 2
7 2 3 1
9 2 3 0
9 2 1 3
9 2 0 2
4 0 3 3
8 3 3 3
14 1 3 1
9 1 1 3
8 1 0 2
0 2 0 2
6 0 3 3
8 3 3 3
14 3 1 1
7 1 1 3
9 1 2 0
9 3 0 1
9 3 1 2
0 0 1 0
8 0 1 0
14 0 3 3
7 3 3 1
8 1 0 0
0 0 1 0
9 2 2 3
9 2 0 2
7 0 2 0
8 0 2 0
14 0 1 1
7 1 0 2
9 1 0 3
9 2 1 1
9 1 1 0
14 0 0 1
8 1 1 1
8 1 2 1
14 1 2 2
7 2 1 3
9 2 3 0
9 3 1 2
9 2 3 1
13 0 2 0
8 0 3 0
14 3 0 3
7 3 0 2
9 3 1 1
9 1 0 3
9 2 2 0
11 0 1 3
8 3 1 3
14 2 3 2
7 2 2 1
8 2 0 0
0 0 1 0
9 2 1 2
9 1 3 3
7 0 2 3
8 3 3 3
14 3 1 1
7 1 2 0
9 0 1 3
9 1 1 1
2 3 2 2
8 2 1 2
14 2 0 0
7 0 3 3
9 3 0 1
9 1 3 0
8 3 0 2
0 2 2 2
0 0 1 1
8 1 2 1
14 1 3 3
7 3 1 2
9 2 1 1
9 1 2 3
9 2 1 0
6 0 3 0
8 0 1 0
8 0 3 0
14 0 2 2
9 2 1 3
9 1 3 1
9 2 1 0
4 0 3 1
8 1 3 1
8 1 1 1
14 1 2 2
7 2 3 1
9 1 3 2
9 3 3 0
9 0 2 3
9 2 0 2
8 2 1 2
14 2 1 1
9 3 2 3
8 1 0 2
0 2 0 2
9 2 2 0
10 3 0 3
8 3 2 3
14 1 3 1
9 0 1 3
9 2 1 2
2 3 2 0
8 0 2 0
14 1 0 1
7 1 0 3
9 3 0 0
9 0 3 1
11 2 0 0
8 0 2 0
14 3 0 3
9 3 3 2
9 3 2 0
9 2 1 1
1 0 2 0
8 0 3 0
14 3 0 3
7 3 0 2
8 1 0 0
0 0 2 0
9 1 0 3
8 0 0 1
0 1 0 1
6 0 3 3
8 3 2 3
8 3 1 3
14 3 2 2
7 2 0 3
9 3 2 2
13 0 2 2
8 2 1 2
14 3 2 3
7 3 3 1
8 0 0 3
0 3 1 3
9 3 1 0
9 2 0 2
11 2 0 3
8 3 1 3
14 3 1 1
9 0 1 3
9 0 1 0
2 3 2 2
8 2 2 2
8 2 2 2
14 2 1 1
7 1 3 3
9 0 2 2
9 3 1 1
1 1 2 2
8 2 3 2
14 2 3 3
7 3 3 0"""


inputmarek =
    """Before: [1, 1, 0, 3]
3 0 2 0
After:  [0, 1, 0, 3]

Before: [0, 1, 2, 3]
12 1 2 3
After:  [0, 1, 2, 0]

Before: [1, 1, 2, 0]
12 1 2 2
After:  [1, 1, 0, 0]

Before: [2, 1, 1, 1]
1 1 3 0
After:  [1, 1, 1, 1]

Before: [0, 3, 1, 2]
15 0 0 2
After:  [0, 3, 1, 2]

Before: [1, 1, 1, 3]
5 2 1 2
After:  [1, 1, 2, 3]

Before: [0, 1, 0, 1]
1 1 3 3
After:  [0, 1, 0, 1]

Before: [2, 1, 2, 0]
8 0 1 0
After:  [1, 1, 2, 0]

Before: [3, 1, 2, 1]
4 3 2 1
After:  [3, 1, 2, 1]

Before: [2, 2, 1, 3]
15 3 3 3
After:  [2, 2, 1, 1]

Before: [2, 1, 2, 0]
15 2 0 2
After:  [2, 1, 1, 0]

Before: [1, 1, 1, 1]
0 1 0 1
After:  [1, 1, 1, 1]

Before: [1, 1, 1, 2]
0 1 0 3
After:  [1, 1, 1, 1]

Before: [2, 1, 0, 2]
8 0 1 3
After:  [2, 1, 0, 1]

Before: [2, 3, 2, 1]
4 3 2 1
After:  [2, 1, 2, 1]

Before: [0, 1, 1, 0]
10 0 0 2
After:  [0, 1, 0, 0]

Before: [2, 0, 2, 1]
7 0 1 0
After:  [1, 0, 2, 1]

Before: [0, 2, 2, 1]
4 3 2 2
After:  [0, 2, 1, 1]

Before: [2, 1, 1, 0]
5 2 1 2
After:  [2, 1, 2, 0]

Before: [3, 1, 2, 1]
4 3 2 0
After:  [1, 1, 2, 1]

Before: [1, 1, 0, 2]
13 3 3 0
After:  [0, 1, 0, 2]

Before: [0, 1, 1, 0]
10 0 0 1
After:  [0, 0, 1, 0]

Before: [0, 1, 1, 3]
5 2 1 0
After:  [2, 1, 1, 3]

Before: [1, 1, 2, 3]
0 1 0 0
After:  [1, 1, 2, 3]

Before: [2, 3, 3, 1]
13 3 3 2
After:  [2, 3, 0, 1]

Before: [0, 1, 2, 2]
12 1 2 0
After:  [0, 1, 2, 2]

Before: [0, 1, 3, 3]
15 3 3 3
After:  [0, 1, 3, 1]

Before: [1, 2, 2, 2]
2 0 2 2
After:  [1, 2, 0, 2]

Before: [2, 1, 1, 2]
5 2 1 2
After:  [2, 1, 2, 2]

Before: [0, 1, 2, 0]
12 1 2 3
After:  [0, 1, 2, 0]

Before: [1, 1, 1, 1]
5 2 1 1
After:  [1, 2, 1, 1]

Before: [1, 1, 2, 1]
13 3 3 2
After:  [1, 1, 0, 1]

Before: [2, 1, 3, 1]
1 1 3 3
After:  [2, 1, 3, 1]

Before: [2, 1, 2, 2]
12 1 2 2
After:  [2, 1, 0, 2]

Before: [1, 0, 2, 0]
2 0 2 1
After:  [1, 0, 2, 0]

Before: [3, 2, 1, 3]
14 2 1 1
After:  [3, 2, 1, 3]

Before: [2, 2, 0, 1]
11 0 3 3
After:  [2, 2, 0, 1]

Before: [2, 2, 0, 1]
11 0 3 1
After:  [2, 1, 0, 1]

Before: [0, 2, 2, 3]
10 0 0 0
After:  [0, 2, 2, 3]

Before: [1, 2, 3, 1]
13 3 3 3
After:  [1, 2, 3, 0]

Before: [2, 0, 2, 1]
11 0 3 3
After:  [2, 0, 2, 1]

Before: [1, 2, 0, 0]
3 0 2 0
After:  [0, 2, 0, 0]

Before: [2, 3, 1, 2]
13 3 3 2
After:  [2, 3, 0, 2]

Before: [3, 1, 3, 2]
9 1 2 2
After:  [3, 1, 0, 2]

Before: [3, 1, 0, 1]
13 3 3 1
After:  [3, 0, 0, 1]

Before: [1, 1, 0, 1]
3 0 2 0
After:  [0, 1, 0, 1]

Before: [1, 1, 3, 2]
9 1 2 3
After:  [1, 1, 3, 0]

Before: [1, 2, 1, 3]
6 1 3 1
After:  [1, 0, 1, 3]

Before: [3, 3, 2, 3]
6 2 3 2
After:  [3, 3, 0, 3]

Before: [1, 3, 2, 3]
2 0 2 3
After:  [1, 3, 2, 0]

Before: [0, 1, 1, 0]
5 2 1 0
After:  [2, 1, 1, 0]

Before: [1, 0, 1, 3]
6 2 3 3
After:  [1, 0, 1, 0]

Before: [1, 1, 2, 1]
7 3 1 0
After:  [0, 1, 2, 1]

Before: [1, 0, 0, 1]
3 0 2 1
After:  [1, 0, 0, 1]

Before: [0, 1, 2, 1]
12 1 2 2
After:  [0, 1, 0, 1]

Before: [1, 3, 0, 0]
3 0 2 1
After:  [1, 0, 0, 0]

Before: [1, 1, 2, 0]
12 1 2 1
After:  [1, 0, 2, 0]

Before: [2, 1, 2, 1]
12 1 2 1
After:  [2, 0, 2, 1]

Before: [3, 3, 2, 1]
13 3 3 1
After:  [3, 0, 2, 1]

Before: [2, 3, 2, 1]
13 3 3 0
After:  [0, 3, 2, 1]

Before: [2, 0, 1, 1]
11 0 3 2
After:  [2, 0, 1, 1]

Before: [1, 1, 2, 3]
0 1 0 2
After:  [1, 1, 1, 3]

Before: [2, 1, 3, 2]
9 1 2 0
After:  [0, 1, 3, 2]

Before: [2, 3, 2, 1]
13 3 3 2
After:  [2, 3, 0, 1]

Before: [0, 1, 1, 1]
1 1 3 1
After:  [0, 1, 1, 1]

Before: [3, 1, 2, 1]
4 3 2 2
After:  [3, 1, 1, 1]

Before: [3, 2, 1, 2]
14 2 1 0
After:  [2, 2, 1, 2]

Before: [2, 2, 1, 1]
14 2 1 2
After:  [2, 2, 2, 1]

Before: [3, 1, 1, 3]
5 2 1 1
After:  [3, 2, 1, 3]

Before: [2, 1, 2, 0]
12 1 2 2
After:  [2, 1, 0, 0]

Before: [0, 3, 1, 0]
10 0 0 1
After:  [0, 0, 1, 0]

Before: [0, 3, 1, 0]
10 0 0 0
After:  [0, 3, 1, 0]

Before: [0, 3, 3, 0]
10 0 0 3
After:  [0, 3, 3, 0]

Before: [1, 3, 2, 0]
2 0 2 1
After:  [1, 0, 2, 0]

Before: [0, 2, 1, 0]
10 0 0 2
After:  [0, 2, 0, 0]

Before: [2, 1, 2, 1]
15 2 0 3
After:  [2, 1, 2, 1]

Before: [0, 1, 2, 1]
1 1 3 3
After:  [0, 1, 2, 1]

Before: [0, 0, 0, 2]
15 0 0 1
After:  [0, 1, 0, 2]

Before: [0, 1, 1, 1]
5 2 1 0
After:  [2, 1, 1, 1]

Before: [2, 1, 0, 1]
7 3 1 0
After:  [0, 1, 0, 1]

Before: [2, 1, 1, 2]
8 0 1 3
After:  [2, 1, 1, 1]

Before: [0, 2, 3, 2]
10 0 0 2
After:  [0, 2, 0, 2]

Before: [0, 1, 1, 1]
5 2 1 1
After:  [0, 2, 1, 1]

Before: [3, 1, 1, 0]
5 2 1 0
After:  [2, 1, 1, 0]

Before: [3, 2, 2, 0]
8 0 2 3
After:  [3, 2, 2, 1]

Before: [3, 2, 2, 2]
7 3 2 1
After:  [3, 0, 2, 2]

Before: [1, 0, 0, 1]
3 0 2 0
After:  [0, 0, 0, 1]

Before: [2, 1, 3, 2]
13 3 3 0
After:  [0, 1, 3, 2]

Before: [1, 1, 0, 0]
0 1 0 0
After:  [1, 1, 0, 0]

Before: [1, 0, 0, 3]
3 0 2 1
After:  [1, 0, 0, 3]

Before: [1, 2, 0, 1]
3 0 2 2
After:  [1, 2, 0, 1]

Before: [0, 1, 0, 2]
10 0 0 1
After:  [0, 0, 0, 2]

Before: [1, 1, 2, 0]
2 0 2 3
After:  [1, 1, 2, 0]

Before: [0, 1, 2, 1]
12 1 2 1
After:  [0, 0, 2, 1]

Before: [1, 1, 2, 0]
15 2 2 3
After:  [1, 1, 2, 1]

Before: [2, 2, 2, 0]
15 2 0 1
After:  [2, 1, 2, 0]

Before: [0, 1, 3, 1]
13 3 3 0
After:  [0, 1, 3, 1]

Before: [0, 2, 0, 3]
6 1 3 3
After:  [0, 2, 0, 0]

Before: [3, 1, 1, 2]
5 2 1 2
After:  [3, 1, 2, 2]

Before: [1, 1, 0, 3]
15 3 3 0
After:  [1, 1, 0, 3]

Before: [1, 1, 3, 1]
7 3 1 2
After:  [1, 1, 0, 1]

Before: [3, 1, 1, 1]
13 2 3 3
After:  [3, 1, 1, 0]

Before: [2, 0, 2, 1]
4 3 2 0
After:  [1, 0, 2, 1]

Before: [0, 2, 2, 1]
4 3 2 1
After:  [0, 1, 2, 1]

Before: [3, 1, 2, 2]
12 1 2 2
After:  [3, 1, 0, 2]

Before: [1, 0, 2, 1]
4 3 2 3
After:  [1, 0, 2, 1]

Before: [0, 1, 3, 1]
9 1 2 3
After:  [0, 1, 3, 0]

Before: [2, 2, 3, 1]
7 2 0 2
After:  [2, 2, 1, 1]

Before: [2, 2, 1, 1]
11 0 3 3
After:  [2, 2, 1, 1]

Before: [3, 1, 3, 0]
15 2 1 1
After:  [3, 0, 3, 0]

Before: [3, 1, 1, 1]
5 2 1 0
After:  [2, 1, 1, 1]

Before: [0, 2, 1, 2]
10 0 0 3
After:  [0, 2, 1, 0]

Before: [3, 2, 2, 3]
6 2 3 1
After:  [3, 0, 2, 3]

Before: [2, 1, 1, 1]
5 2 1 3
After:  [2, 1, 1, 2]

Before: [1, 1, 2, 1]
2 0 2 1
After:  [1, 0, 2, 1]

Before: [1, 0, 2, 2]
7 3 2 1
After:  [1, 0, 2, 2]

Before: [2, 0, 3, 1]
11 0 3 0
After:  [1, 0, 3, 1]

Before: [3, 1, 3, 0]
9 1 2 0
After:  [0, 1, 3, 0]

Before: [2, 1, 1, 1]
11 0 3 0
After:  [1, 1, 1, 1]

Before: [1, 1, 0, 3]
3 0 2 2
After:  [1, 1, 0, 3]

Before: [0, 2, 1, 0]
14 2 1 3
After:  [0, 2, 1, 2]

Before: [1, 1, 2, 2]
12 1 2 2
After:  [1, 1, 0, 2]

Before: [1, 1, 1, 2]
5 2 1 2
After:  [1, 1, 2, 2]

Before: [3, 2, 0, 0]
7 0 2 3
After:  [3, 2, 0, 1]

Before: [2, 1, 1, 3]
7 2 1 1
After:  [2, 0, 1, 3]

Before: [2, 1, 0, 3]
8 0 1 0
After:  [1, 1, 0, 3]

Before: [3, 2, 2, 1]
4 3 2 0
After:  [1, 2, 2, 1]

Before: [1, 1, 1, 0]
5 2 1 3
After:  [1, 1, 1, 2]

Before: [2, 0, 3, 1]
7 0 1 3
After:  [2, 0, 3, 1]

Before: [0, 2, 2, 1]
4 3 2 0
After:  [1, 2, 2, 1]

Before: [1, 2, 1, 0]
14 2 1 2
After:  [1, 2, 2, 0]

Before: [1, 1, 2, 1]
1 1 3 3
After:  [1, 1, 2, 1]

Before: [1, 1, 1, 0]
0 1 0 0
After:  [1, 1, 1, 0]

Before: [1, 3, 2, 3]
6 2 3 2
After:  [1, 3, 0, 3]

Before: [2, 1, 1, 1]
11 0 3 1
After:  [2, 1, 1, 1]

Before: [2, 3, 3, 1]
11 0 3 1
After:  [2, 1, 3, 1]

Before: [3, 0, 1, 3]
15 3 2 0
After:  [0, 0, 1, 3]

Before: [2, 1, 2, 1]
4 3 2 1
After:  [2, 1, 2, 1]

Before: [1, 1, 0, 3]
3 0 2 3
After:  [1, 1, 0, 0]

Before: [1, 3, 2, 2]
2 0 2 3
After:  [1, 3, 2, 0]

Before: [1, 2, 3, 3]
6 1 3 2
After:  [1, 2, 0, 3]

Before: [0, 0, 1, 1]
10 0 0 0
After:  [0, 0, 1, 1]

Before: [2, 1, 2, 1]
11 0 3 1
After:  [2, 1, 2, 1]

Before: [1, 0, 2, 0]
2 0 2 2
After:  [1, 0, 0, 0]

Before: [0, 1, 1, 2]
5 2 1 3
After:  [0, 1, 1, 2]

Before: [1, 1, 2, 2]
0 1 0 0
After:  [1, 1, 2, 2]

Before: [0, 1, 0, 1]
1 1 3 2
After:  [0, 1, 1, 1]

Before: [1, 1, 3, 1]
0 1 0 2
After:  [1, 1, 1, 1]

Before: [3, 1, 1, 1]
1 1 3 1
After:  [3, 1, 1, 1]

Before: [1, 3, 2, 3]
2 0 2 0
After:  [0, 3, 2, 3]

Before: [2, 2, 1, 3]
6 2 3 0
After:  [0, 2, 1, 3]

Before: [0, 1, 1, 2]
5 2 1 0
After:  [2, 1, 1, 2]

Before: [2, 1, 3, 1]
13 3 3 0
After:  [0, 1, 3, 1]

Before: [2, 1, 2, 3]
12 1 2 3
After:  [2, 1, 2, 0]

Before: [3, 2, 2, 1]
4 3 2 1
After:  [3, 1, 2, 1]

Before: [1, 2, 1, 3]
6 2 3 1
After:  [1, 0, 1, 3]

Before: [1, 3, 1, 3]
6 2 3 2
After:  [1, 3, 0, 3]

Before: [1, 1, 2, 1]
0 1 0 1
After:  [1, 1, 2, 1]

Before: [2, 3, 2, 3]
6 2 3 2
After:  [2, 3, 0, 3]

Before: [1, 1, 3, 3]
15 3 3 3
After:  [1, 1, 3, 1]

Before: [0, 0, 2, 3]
6 2 3 3
After:  [0, 0, 2, 0]

Before: [1, 1, 3, 1]
0 1 0 0
After:  [1, 1, 3, 1]

Before: [3, 2, 1, 3]
15 3 3 0
After:  [1, 2, 1, 3]

Before: [1, 0, 2, 1]
2 0 2 0
After:  [0, 0, 2, 1]

Before: [3, 1, 0, 3]
7 0 2 3
After:  [3, 1, 0, 1]

Before: [1, 1, 3, 1]
1 1 3 1
After:  [1, 1, 3, 1]

Before: [2, 3, 0, 1]
11 0 3 2
After:  [2, 3, 1, 1]

Before: [2, 3, 3, 1]
7 2 0 2
After:  [2, 3, 1, 1]

Before: [1, 3, 2, 1]
13 3 3 3
After:  [1, 3, 2, 0]

Before: [0, 3, 2, 2]
7 3 2 3
After:  [0, 3, 2, 0]

Before: [2, 1, 3, 2]
13 3 3 3
After:  [2, 1, 3, 0]

Before: [2, 0, 1, 1]
7 0 1 1
After:  [2, 1, 1, 1]

Before: [3, 1, 2, 3]
8 0 2 1
After:  [3, 1, 2, 3]

Before: [2, 1, 1, 3]
6 2 3 2
After:  [2, 1, 0, 3]

Before: [2, 1, 1, 0]
5 2 1 3
After:  [2, 1, 1, 2]

Before: [0, 0, 0, 0]
10 0 0 3
After:  [0, 0, 0, 0]

Before: [2, 1, 2, 1]
1 1 3 3
After:  [2, 1, 2, 1]

Before: [3, 1, 0, 2]
7 0 2 0
After:  [1, 1, 0, 2]

Before: [1, 2, 2, 1]
13 3 3 2
After:  [1, 2, 0, 1]

Before: [3, 1, 1, 1]
5 2 1 1
After:  [3, 2, 1, 1]

Before: [1, 3, 0, 2]
3 0 2 1
After:  [1, 0, 0, 2]

Before: [0, 1, 0, 1]
1 1 3 0
After:  [1, 1, 0, 1]

Before: [3, 1, 2, 1]
12 1 2 0
After:  [0, 1, 2, 1]

Before: [1, 3, 2, 1]
2 0 2 2
After:  [1, 3, 0, 1]

Before: [2, 3, 1, 1]
11 0 3 0
After:  [1, 3, 1, 1]

Before: [0, 1, 1, 0]
5 2 1 2
After:  [0, 1, 2, 0]

Before: [0, 1, 3, 0]
9 1 2 2
After:  [0, 1, 0, 0]

Before: [2, 1, 1, 1]
5 2 1 0
After:  [2, 1, 1, 1]

Before: [1, 1, 1, 1]
0 1 0 0
After:  [1, 1, 1, 1]

Before: [1, 0, 0, 1]
3 0 2 2
After:  [1, 0, 0, 1]

Before: [0, 1, 3, 2]
9 1 2 0
After:  [0, 1, 3, 2]

Before: [1, 3, 0, 1]
3 0 2 2
After:  [1, 3, 0, 1]

Before: [2, 0, 2, 1]
4 3 2 1
After:  [2, 1, 2, 1]

Before: [0, 2, 1, 3]
6 2 3 1
After:  [0, 0, 1, 3]

Before: [1, 2, 0, 2]
3 0 2 0
After:  [0, 2, 0, 2]

Before: [0, 1, 2, 2]
12 1 2 3
After:  [0, 1, 2, 0]

Before: [1, 1, 1, 2]
0 1 0 2
After:  [1, 1, 1, 2]

Before: [1, 1, 1, 0]
0 1 0 3
After:  [1, 1, 1, 1]

Before: [3, 1, 2, 3]
6 1 3 3
After:  [3, 1, 2, 0]

Before: [2, 2, 1, 1]
11 0 3 2
After:  [2, 2, 1, 1]

Before: [2, 3, 3, 1]
11 0 3 2
After:  [2, 3, 1, 1]

Before: [0, 2, 3, 2]
15 0 0 1
After:  [0, 1, 3, 2]

Before: [0, 3, 1, 3]
6 2 3 3
After:  [0, 3, 1, 0]

Before: [3, 2, 3, 1]
15 2 3 2
After:  [3, 2, 0, 1]

Before: [0, 1, 1, 1]
7 2 1 2
After:  [0, 1, 0, 1]

Before: [3, 1, 2, 1]
1 1 3 0
After:  [1, 1, 2, 1]

Before: [0, 0, 0, 3]
10 0 0 0
After:  [0, 0, 0, 3]

Before: [1, 1, 3, 1]
9 1 2 0
After:  [0, 1, 3, 1]

Before: [0, 3, 1, 3]
10 0 0 1
After:  [0, 0, 1, 3]

Before: [1, 2, 1, 1]
14 2 1 2
After:  [1, 2, 2, 1]

Before: [3, 1, 0, 1]
1 1 3 3
After:  [3, 1, 0, 1]

Before: [0, 1, 1, 1]
1 1 3 2
After:  [0, 1, 1, 1]

Before: [1, 1, 2, 0]
0 1 0 2
After:  [1, 1, 1, 0]

Before: [0, 3, 2, 2]
7 3 2 0
After:  [0, 3, 2, 2]

Before: [0, 3, 0, 3]
10 0 0 3
After:  [0, 3, 0, 0]

Before: [1, 1, 2, 1]
12 1 2 3
After:  [1, 1, 2, 0]

Before: [0, 0, 2, 1]
4 3 2 2
After:  [0, 0, 1, 1]

Before: [1, 1, 2, 0]
12 1 2 0
After:  [0, 1, 2, 0]

Before: [0, 1, 2, 1]
12 1 2 3
After:  [0, 1, 2, 0]

Before: [0, 1, 1, 3]
6 1 3 0
After:  [0, 1, 1, 3]

Before: [2, 3, 2, 1]
11 0 3 0
After:  [1, 3, 2, 1]

Before: [1, 1, 1, 1]
5 2 1 3
After:  [1, 1, 1, 2]

Before: [1, 0, 2, 0]
2 0 2 3
After:  [1, 0, 2, 0]

Before: [1, 1, 2, 3]
2 0 2 2
After:  [1, 1, 0, 3]

Before: [2, 0, 0, 1]
11 0 3 0
After:  [1, 0, 0, 1]

Before: [3, 0, 3, 3]
15 3 2 2
After:  [3, 0, 1, 3]

Before: [1, 2, 2, 2]
2 0 2 3
After:  [1, 2, 2, 0]

Before: [1, 1, 2, 1]
12 1 2 2
After:  [1, 1, 0, 1]

Before: [1, 1, 2, 0]
0 1 0 1
After:  [1, 1, 2, 0]

Before: [1, 0, 2, 2]
13 3 3 1
After:  [1, 0, 2, 2]

Before: [2, 1, 2, 1]
12 1 2 3
After:  [2, 1, 2, 0]

Before: [0, 3, 2, 2]
10 0 0 3
After:  [0, 3, 2, 0]

Before: [1, 1, 1, 2]
5 2 1 1
After:  [1, 2, 1, 2]

Before: [3, 3, 0, 1]
13 3 3 0
After:  [0, 3, 0, 1]

Before: [1, 1, 0, 2]
3 0 2 3
After:  [1, 1, 0, 0]

Before: [2, 1, 2, 3]
15 2 2 0
After:  [1, 1, 2, 3]

Before: [2, 1, 1, 1]
8 0 1 2
After:  [2, 1, 1, 1]

Before: [0, 1, 1, 2]
10 0 0 0
After:  [0, 1, 1, 2]

Before: [1, 1, 2, 1]
0 1 0 2
After:  [1, 1, 1, 1]

Before: [1, 2, 2, 1]
15 2 2 2
After:  [1, 2, 1, 1]

Before: [0, 3, 2, 1]
4 3 2 0
After:  [1, 3, 2, 1]

Before: [0, 1, 3, 3]
9 1 2 0
After:  [0, 1, 3, 3]

Before: [0, 1, 1, 0]
7 2 1 3
After:  [0, 1, 1, 0]

Before: [1, 2, 2, 1]
2 0 2 3
After:  [1, 2, 2, 0]

Before: [2, 2, 3, 1]
11 0 3 1
After:  [2, 1, 3, 1]

Before: [3, 2, 1, 1]
14 2 1 1
After:  [3, 2, 1, 1]

Before: [3, 1, 3, 1]
9 1 2 1
After:  [3, 0, 3, 1]

Before: [2, 1, 0, 1]
1 1 3 3
After:  [2, 1, 0, 1]

Before: [1, 1, 3, 1]
0 1 0 3
After:  [1, 1, 3, 1]

Before: [2, 2, 2, 1]
4 3 2 0
After:  [1, 2, 2, 1]

Before: [1, 3, 2, 2]
2 0 2 0
After:  [0, 3, 2, 2]

Before: [2, 1, 3, 3]
9 1 2 0
After:  [0, 1, 3, 3]

Before: [3, 0, 2, 0]
8 0 2 0
After:  [1, 0, 2, 0]

Before: [1, 1, 1, 3]
0 1 0 1
After:  [1, 1, 1, 3]

Before: [2, 1, 2, 1]
11 0 3 0
After:  [1, 1, 2, 1]

Before: [1, 1, 2, 1]
2 0 2 0
After:  [0, 1, 2, 1]

Before: [1, 1, 0, 0]
3 0 2 0
After:  [0, 1, 0, 0]

Before: [0, 3, 1, 1]
15 0 0 0
After:  [1, 3, 1, 1]

Before: [1, 3, 2, 3]
6 2 3 0
After:  [0, 3, 2, 3]

Before: [0, 0, 1, 2]
13 3 3 1
After:  [0, 0, 1, 2]

Before: [1, 1, 2, 1]
4 3 2 3
After:  [1, 1, 2, 1]

Before: [1, 2, 1, 3]
14 2 1 0
After:  [2, 2, 1, 3]

Before: [0, 3, 1, 1]
10 0 0 3
After:  [0, 3, 1, 0]

Before: [2, 3, 1, 1]
13 2 3 1
After:  [2, 0, 1, 1]

Before: [3, 1, 2, 1]
4 3 2 3
After:  [3, 1, 2, 1]

Before: [2, 2, 1, 1]
11 0 3 1
After:  [2, 1, 1, 1]

Before: [0, 2, 2, 2]
10 0 0 2
After:  [0, 2, 0, 2]

Before: [0, 0, 2, 1]
4 3 2 0
After:  [1, 0, 2, 1]

Before: [3, 1, 1, 3]
5 2 1 2
After:  [3, 1, 2, 3]

Before: [2, 2, 0, 3]
6 1 3 1
After:  [2, 0, 0, 3]

Before: [3, 0, 2, 1]
4 3 2 2
After:  [3, 0, 1, 1]

Before: [3, 0, 2, 1]
8 0 2 3
After:  [3, 0, 2, 1]

Before: [3, 1, 0, 0]
7 0 2 3
After:  [3, 1, 0, 1]

Before: [2, 1, 3, 2]
9 1 2 2
After:  [2, 1, 0, 2]

Before: [0, 2, 2, 0]
10 0 0 0
After:  [0, 2, 2, 0]

Before: [1, 2, 2, 1]
4 3 2 2
After:  [1, 2, 1, 1]

Before: [2, 1, 1, 0]
8 0 1 2
After:  [2, 1, 1, 0]

Before: [1, 0, 2, 3]
6 2 3 2
After:  [1, 0, 0, 3]

Before: [1, 1, 2, 3]
6 1 3 2
After:  [1, 1, 0, 3]

Before: [2, 3, 2, 1]
4 3 2 0
After:  [1, 3, 2, 1]

Before: [1, 2, 1, 0]
14 2 1 3
After:  [1, 2, 1, 2]

Before: [1, 1, 0, 3]
0 1 0 1
After:  [1, 1, 0, 3]

Before: [2, 2, 1, 3]
15 3 3 0
After:  [1, 2, 1, 3]

Before: [0, 2, 1, 3]
10 0 0 1
After:  [0, 0, 1, 3]

Before: [1, 1, 3, 2]
0 1 0 2
After:  [1, 1, 1, 2]

Before: [2, 0, 3, 1]
11 0 3 3
After:  [2, 0, 3, 1]

Before: [2, 1, 2, 3]
12 1 2 1
After:  [2, 0, 2, 3]

Before: [1, 1, 0, 0]
3 0 2 2
After:  [1, 1, 0, 0]

Before: [3, 1, 1, 1]
13 3 3 0
After:  [0, 1, 1, 1]

Before: [0, 0, 2, 3]
10 0 0 3
After:  [0, 0, 2, 0]

Before: [3, 1, 3, 1]
9 1 2 0
After:  [0, 1, 3, 1]

Before: [1, 1, 2, 0]
0 1 0 0
After:  [1, 1, 2, 0]

Before: [0, 1, 2, 3]
6 2 3 1
After:  [0, 0, 2, 3]

Before: [2, 1, 3, 3]
9 1 2 1
After:  [2, 0, 3, 3]

Before: [1, 2, 1, 3]
14 2 1 1
After:  [1, 2, 1, 3]

Before: [0, 1, 2, 2]
10 0 0 3
After:  [0, 1, 2, 0]

Before: [2, 1, 2, 0]
12 1 2 1
After:  [2, 0, 2, 0]

Before: [1, 1, 0, 1]
1 1 3 1
After:  [1, 1, 0, 1]

Before: [1, 3, 2, 3]
15 3 2 3
After:  [1, 3, 2, 0]

Before: [1, 2, 2, 2]
7 3 2 2
After:  [1, 2, 0, 2]

Before: [3, 3, 2, 0]
8 0 2 3
After:  [3, 3, 2, 1]

Before: [0, 3, 1, 1]
10 0 0 0
After:  [0, 3, 1, 1]

Before: [0, 1, 1, 2]
13 3 3 0
After:  [0, 1, 1, 2]

Before: [1, 1, 1, 0]
5 2 1 1
After:  [1, 2, 1, 0]

Before: [1, 2, 0, 1]
3 0 2 1
After:  [1, 0, 0, 1]

Before: [3, 1, 3, 1]
9 1 2 3
After:  [3, 1, 3, 0]

Before: [1, 2, 2, 3]
2 0 2 0
After:  [0, 2, 2, 3]

Before: [0, 3, 2, 1]
4 3 2 2
After:  [0, 3, 1, 1]

Before: [1, 2, 2, 1]
15 2 1 0
After:  [1, 2, 2, 1]

Before: [2, 0, 3, 0]
7 2 0 1
After:  [2, 1, 3, 0]

Before: [1, 3, 2, 1]
4 3 2 1
After:  [1, 1, 2, 1]

Before: [1, 3, 0, 1]
3 0 2 0
After:  [0, 3, 0, 1]

Before: [3, 1, 1, 1]
13 2 3 1
After:  [3, 0, 1, 1]

Before: [2, 2, 3, 1]
11 0 3 3
After:  [2, 2, 3, 1]

Before: [3, 3, 2, 1]
15 2 2 3
After:  [3, 3, 2, 1]

Before: [3, 0, 3, 3]
15 3 2 3
After:  [3, 0, 3, 1]

Before: [1, 1, 0, 1]
3 0 2 1
After:  [1, 0, 0, 1]

Before: [1, 1, 0, 2]
0 1 0 3
After:  [1, 1, 0, 1]

Before: [0, 0, 2, 1]
10 0 0 1
After:  [0, 0, 2, 1]

Before: [1, 1, 3, 0]
0 1 0 1
After:  [1, 1, 3, 0]

Before: [1, 0, 0, 3]
3 0 2 0
After:  [0, 0, 0, 3]

Before: [0, 2, 1, 3]
10 0 0 0
After:  [0, 2, 1, 3]

Before: [3, 1, 2, 0]
12 1 2 3
After:  [3, 1, 2, 0]

Before: [2, 1, 3, 0]
8 0 1 0
After:  [1, 1, 3, 0]

Before: [1, 0, 2, 1]
4 3 2 1
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 3]
6 1 3 0
After:  [0, 1, 2, 3]

Before: [1, 1, 0, 0]
0 1 0 3
After:  [1, 1, 0, 1]

Before: [3, 1, 1, 3]
7 2 1 3
After:  [3, 1, 1, 0]

Before: [0, 2, 1, 1]
14 2 1 2
After:  [0, 2, 2, 1]

Before: [2, 1, 0, 1]
11 0 3 3
After:  [2, 1, 0, 1]

Before: [1, 1, 2, 3]
0 1 0 1
After:  [1, 1, 2, 3]

Before: [2, 1, 3, 0]
9 1 2 0
After:  [0, 1, 3, 0]

Before: [0, 2, 1, 3]
6 1 3 0
After:  [0, 2, 1, 3]

Before: [1, 1, 3, 2]
0 1 0 0
After:  [1, 1, 3, 2]

Before: [0, 2, 1, 3]
14 2 1 0
After:  [2, 2, 1, 3]

Before: [0, 0, 1, 1]
13 3 3 1
After:  [0, 0, 1, 1]

Before: [2, 1, 1, 0]
5 2 1 0
After:  [2, 1, 1, 0]

Before: [3, 1, 1, 1]
13 3 3 3
After:  [3, 1, 1, 0]

Before: [1, 1, 2, 1]
1 1 3 1
After:  [1, 1, 2, 1]

Before: [0, 1, 2, 1]
1 1 3 2
After:  [0, 1, 1, 1]

Before: [0, 1, 1, 2]
5 2 1 1
After:  [0, 2, 1, 2]

Before: [2, 1, 1, 2]
8 0 1 1
After:  [2, 1, 1, 2]

Before: [2, 1, 1, 2]
8 0 1 0
After:  [1, 1, 1, 2]

Before: [2, 1, 1, 1]
5 2 1 1
After:  [2, 2, 1, 1]

Before: [3, 2, 1, 0]
14 2 1 2
After:  [3, 2, 2, 0]

Before: [2, 3, 0, 1]
11 0 3 0
After:  [1, 3, 0, 1]

Before: [0, 1, 1, 0]
5 2 1 1
After:  [0, 2, 1, 0]

Before: [3, 3, 0, 3]
7 0 2 1
After:  [3, 1, 0, 3]

Before: [1, 1, 2, 3]
6 2 3 1
After:  [1, 0, 2, 3]

Before: [1, 1, 2, 0]
2 0 2 0
After:  [0, 1, 2, 0]

Before: [3, 0, 2, 3]
8 0 2 0
After:  [1, 0, 2, 3]

Before: [0, 1, 1, 1]
1 1 3 3
After:  [0, 1, 1, 1]

Before: [2, 1, 2, 2]
12 1 2 1
After:  [2, 0, 2, 2]

Before: [3, 3, 2, 1]
4 3 2 3
After:  [3, 3, 2, 1]

Before: [1, 2, 2, 3]
2 0 2 3
After:  [1, 2, 2, 0]

Before: [1, 1, 0, 1]
0 1 0 2
After:  [1, 1, 1, 1]

Before: [0, 2, 2, 1]
4 3 2 3
After:  [0, 2, 2, 1]

Before: [0, 1, 1, 1]
7 3 1 0
After:  [0, 1, 1, 1]

Before: [2, 0, 0, 1]
11 0 3 3
After:  [2, 0, 0, 1]

Before: [1, 1, 2, 2]
0 1 0 1
After:  [1, 1, 2, 2]

Before: [1, 2, 0, 3]
3 0 2 1
After:  [1, 0, 0, 3]

Before: [1, 1, 3, 3]
9 1 2 2
After:  [1, 1, 0, 3]

Before: [3, 1, 3, 0]
9 1 2 3
After:  [3, 1, 3, 0]

Before: [1, 1, 1, 2]
0 1 0 1
After:  [1, 1, 1, 2]

Before: [0, 1, 2, 1]
4 3 2 2
After:  [0, 1, 1, 1]

Before: [1, 1, 1, 0]
5 2 1 2
After:  [1, 1, 2, 0]

Before: [1, 1, 3, 3]
6 1 3 3
After:  [1, 1, 3, 0]

Before: [0, 1, 0, 1]
7 3 1 0
After:  [0, 1, 0, 1]

Before: [3, 1, 1, 1]
1 1 3 0
After:  [1, 1, 1, 1]

Before: [2, 1, 2, 1]
4 3 2 0
After:  [1, 1, 2, 1]

Before: [2, 3, 1, 1]
13 3 3 1
After:  [2, 0, 1, 1]

Before: [2, 0, 3, 1]
11 0 3 2
After:  [2, 0, 1, 1]

Before: [0, 1, 3, 0]
9 1 2 0
After:  [0, 1, 3, 0]

Before: [1, 2, 2, 3]
2 0 2 1
After:  [1, 0, 2, 3]

Before: [1, 3, 0, 0]
3 0 2 0
After:  [0, 3, 0, 0]

Before: [0, 2, 1, 1]
14 2 1 1
After:  [0, 2, 1, 1]

Before: [1, 2, 2, 2]
2 0 2 1
After:  [1, 0, 2, 2]

Before: [0, 3, 2, 0]
10 0 0 0
After:  [0, 3, 2, 0]

Before: [1, 1, 0, 1]
0 1 0 0
After:  [1, 1, 0, 1]

Before: [3, 1, 2, 2]
7 3 2 1
After:  [3, 0, 2, 2]

Before: [1, 1, 1, 1]
5 2 1 2
After:  [1, 1, 2, 1]

Before: [1, 0, 0, 2]
3 0 2 3
After:  [1, 0, 0, 0]

Before: [1, 1, 3, 0]
0 1 0 3
After:  [1, 1, 3, 1]

Before: [0, 3, 2, 0]
15 0 0 1
After:  [0, 1, 2, 0]

Before: [2, 2, 2, 3]
15 2 2 0
After:  [1, 2, 2, 3]

Before: [1, 1, 1, 1]
0 1 0 3
After:  [1, 1, 1, 1]

Before: [0, 1, 3, 1]
15 2 3 3
After:  [0, 1, 3, 0]

Before: [0, 0, 0, 2]
10 0 0 1
After:  [0, 0, 0, 2]

Before: [1, 3, 0, 3]
3 0 2 3
After:  [1, 3, 0, 0]

Before: [3, 2, 2, 2]
8 0 2 1
After:  [3, 1, 2, 2]

Before: [2, 1, 2, 3]
6 1 3 2
After:  [2, 1, 0, 3]

Before: [3, 1, 1, 1]
5 2 1 3
After:  [3, 1, 1, 2]

Before: [0, 0, 3, 1]
10 0 0 3
After:  [0, 0, 3, 0]

Before: [3, 1, 3, 1]
9 1 2 2
After:  [3, 1, 0, 1]

Before: [1, 2, 2, 1]
13 3 3 0
After:  [0, 2, 2, 1]

Before: [1, 0, 0, 2]
13 3 3 0
After:  [0, 0, 0, 2]

Before: [0, 2, 1, 0]
14 2 1 1
After:  [0, 2, 1, 0]

Before: [3, 1, 1, 2]
5 2 1 0
After:  [2, 1, 1, 2]

Before: [2, 1, 0, 3]
8 0 1 2
After:  [2, 1, 1, 3]

Before: [1, 1, 0, 3]
0 1 0 0
After:  [1, 1, 0, 3]

Before: [2, 2, 2, 1]
4 3 2 1
After:  [2, 1, 2, 1]

Before: [1, 3, 0, 3]
3 0 2 2
After:  [1, 3, 0, 3]

Before: [2, 0, 2, 0]
7 0 1 0
After:  [1, 0, 2, 0]

Before: [3, 1, 0, 1]
1 1 3 0
After:  [1, 1, 0, 1]

Before: [1, 1, 0, 0]
3 0 2 3
After:  [1, 1, 0, 0]

Before: [2, 1, 0, 1]
11 0 3 2
After:  [2, 1, 1, 1]

Before: [3, 2, 2, 3]
6 2 3 3
After:  [3, 2, 2, 0]

Before: [2, 0, 0, 3]
7 0 1 2
After:  [2, 0, 1, 3]

Before: [0, 0, 2, 1]
4 3 2 3
After:  [0, 0, 2, 1]

Before: [0, 3, 0, 2]
10 0 0 3
After:  [0, 3, 0, 0]

Before: [2, 0, 2, 2]
7 3 2 3
After:  [2, 0, 2, 0]

Before: [1, 1, 0, 3]
0 1 0 2
After:  [1, 1, 1, 3]

Before: [2, 0, 2, 1]
11 0 3 1
After:  [2, 1, 2, 1]

Before: [1, 2, 3, 3]
15 3 2 0
After:  [1, 2, 3, 3]

Before: [2, 1, 3, 1]
7 3 1 1
After:  [2, 0, 3, 1]

Before: [1, 1, 0, 3]
6 1 3 0
After:  [0, 1, 0, 3]

Before: [1, 0, 0, 0]
3 0 2 2
After:  [1, 0, 0, 0]

Before: [2, 1, 3, 1]
11 0 3 2
After:  [2, 1, 1, 1]

Before: [2, 0, 1, 1]
11 0 3 1
After:  [2, 1, 1, 1]

Before: [1, 1, 1, 3]
0 1 0 3
After:  [1, 1, 1, 1]

Before: [1, 2, 2, 0]
2 0 2 0
After:  [0, 2, 2, 0]

Before: [1, 2, 0, 3]
3 0 2 3
After:  [1, 2, 0, 0]

Before: [1, 3, 2, 1]
4 3 2 3
After:  [1, 3, 2, 1]

Before: [0, 2, 1, 2]
14 2 1 3
After:  [0, 2, 1, 2]

Before: [3, 0, 2, 3]
8 0 2 1
After:  [3, 1, 2, 3]

Before: [0, 1, 1, 3]
10 0 0 3
After:  [0, 1, 1, 0]

Before: [2, 1, 2, 1]
4 3 2 3
After:  [2, 1, 2, 1]

Before: [1, 1, 2, 3]
6 1 3 0
After:  [0, 1, 2, 3]

Before: [2, 1, 1, 2]
5 2 1 0
After:  [2, 1, 1, 2]

Before: [2, 1, 1, 0]
5 2 1 1
After:  [2, 2, 1, 0]

Before: [0, 1, 1, 1]
5 2 1 2
After:  [0, 1, 2, 1]

Before: [2, 3, 1, 1]
11 0 3 1
After:  [2, 1, 1, 1]

Before: [1, 1, 3, 0]
0 1 0 0
After:  [1, 1, 3, 0]

Before: [1, 3, 2, 3]
2 0 2 1
After:  [1, 0, 2, 3]

Before: [0, 1, 1, 1]
5 2 1 3
After:  [0, 1, 1, 2]

Before: [0, 1, 3, 3]
6 1 3 2
After:  [0, 1, 0, 3]

Before: [2, 0, 2, 3]
6 2 3 0
After:  [0, 0, 2, 3]

Before: [2, 2, 3, 1]
7 2 0 3
After:  [2, 2, 3, 1]

Before: [1, 3, 0, 3]
3 0 2 1
After:  [1, 0, 0, 3]

Before: [1, 2, 0, 2]
3 0 2 1
After:  [1, 0, 0, 2]

Before: [2, 2, 1, 1]
14 2 1 0
After:  [2, 2, 1, 1]

Before: [2, 1, 3, 3]
9 1 2 3
After:  [2, 1, 3, 0]

Before: [1, 1, 2, 2]
0 1 0 3
After:  [1, 1, 2, 1]

Before: [0, 1, 1, 3]
15 3 3 3
After:  [0, 1, 1, 1]

Before: [1, 3, 2, 1]
4 3 2 0
After:  [1, 3, 2, 1]

Before: [2, 1, 2, 3]
8 0 1 0
After:  [1, 1, 2, 3]

Before: [1, 0, 2, 3]
2 0 2 3
After:  [1, 0, 2, 0]

Before: [0, 0, 2, 3]
15 3 3 2
After:  [0, 0, 1, 3]

Before: [0, 0, 2, 2]
15 2 2 0
After:  [1, 0, 2, 2]

Before: [3, 3, 2, 2]
8 0 2 1
After:  [3, 1, 2, 2]

Before: [1, 1, 3, 1]
13 3 3 1
After:  [1, 0, 3, 1]

Before: [3, 2, 2, 1]
4 3 2 3
After:  [3, 2, 2, 1]

Before: [1, 1, 3, 1]
1 1 3 0
After:  [1, 1, 3, 1]

Before: [0, 3, 2, 1]
4 3 2 3
After:  [0, 3, 2, 1]

Before: [3, 1, 2, 3]
12 1 2 1
After:  [3, 0, 2, 3]

Before: [1, 2, 1, 2]
14 2 1 1
After:  [1, 2, 1, 2]

Before: [1, 3, 0, 2]
3 0 2 2
After:  [1, 3, 0, 2]

Before: [1, 1, 3, 3]
0 1 0 3
After:  [1, 1, 3, 1]

Before: [3, 3, 2, 1]
4 3 2 1
After:  [3, 1, 2, 1]

Before: [0, 1, 1, 2]
10 0 0 1
After:  [0, 0, 1, 2]

Before: [1, 2, 1, 0]
14 2 1 1
After:  [1, 2, 1, 0]

Before: [2, 1, 0, 1]
1 1 3 2
After:  [2, 1, 1, 1]

Before: [2, 1, 0, 2]
13 3 3 2
After:  [2, 1, 0, 2]

Before: [1, 2, 0, 0]
3 0 2 1
After:  [1, 0, 0, 0]

Before: [3, 2, 1, 1]
14 2 1 3
After:  [3, 2, 1, 2]

Before: [3, 0, 1, 1]
13 2 3 0
After:  [0, 0, 1, 1]

Before: [2, 2, 2, 1]
11 0 3 2
After:  [2, 2, 1, 1]

Before: [2, 1, 1, 1]
1 1 3 2
After:  [2, 1, 1, 1]

Before: [0, 2, 0, 0]
10 0 0 1
After:  [0, 0, 0, 0]

Before: [1, 1, 1, 3]
0 1 0 2
After:  [1, 1, 1, 3]

Before: [3, 2, 2, 3]
8 0 2 2
After:  [3, 2, 1, 3]

Before: [1, 3, 0, 0]
3 0 2 2
After:  [1, 3, 0, 0]

Before: [2, 1, 1, 3]
15 3 3 3
After:  [2, 1, 1, 1]

Before: [2, 1, 0, 1]
11 0 3 1
After:  [2, 1, 0, 1]

Before: [3, 3, 2, 1]
13 3 3 3
After:  [3, 3, 2, 0]

Before: [3, 1, 1, 2]
5 2 1 3
After:  [3, 1, 1, 2]

Before: [1, 1, 3, 3]
6 1 3 0
After:  [0, 1, 3, 3]

Before: [0, 1, 1, 1]
1 1 3 0
After:  [1, 1, 1, 1]

Before: [1, 1, 0, 0]
0 1 0 1
After:  [1, 1, 0, 0]

Before: [1, 1, 2, 3]
2 0 2 0
After:  [0, 1, 2, 3]

Before: [1, 3, 0, 0]
3 0 2 3
After:  [1, 3, 0, 0]

Before: [0, 1, 2, 3]
15 0 0 2
After:  [0, 1, 1, 3]

Before: [0, 0, 2, 2]
10 0 0 3
After:  [0, 0, 2, 0]

Before: [1, 1, 3, 3]
0 1 0 0
After:  [1, 1, 3, 3]

Before: [0, 2, 2, 0]
10 0 0 1
After:  [0, 0, 2, 0]

Before: [0, 3, 3, 0]
10 0 0 1
After:  [0, 0, 3, 0]

Before: [0, 1, 1, 3]
5 2 1 2
After:  [0, 1, 2, 3]

Before: [3, 3, 2, 2]
8 0 2 2
After:  [3, 3, 1, 2]

Before: [2, 3, 3, 1]
11 0 3 3
After:  [2, 3, 3, 1]

Before: [2, 1, 3, 1]
7 3 1 0
After:  [0, 1, 3, 1]

Before: [3, 1, 1, 1]
5 2 1 2
After:  [3, 1, 2, 1]

Before: [3, 1, 3, 1]
1 1 3 3
After:  [3, 1, 3, 1]

Before: [0, 1, 1, 3]
5 2 1 3
After:  [0, 1, 1, 2]

Before: [2, 2, 3, 3]
6 1 3 1
After:  [2, 0, 3, 3]

Before: [3, 2, 1, 3]
15 3 0 1
After:  [3, 1, 1, 3]

Before: [1, 1, 1, 3]
0 1 0 0
After:  [1, 1, 1, 3]

Before: [2, 1, 0, 3]
6 1 3 0
After:  [0, 1, 0, 3]

Before: [1, 2, 2, 2]
15 2 1 2
After:  [1, 2, 1, 2]

Before: [2, 3, 2, 1]
11 0 3 3
After:  [2, 3, 2, 1]

Before: [2, 3, 2, 1]
11 0 3 1
After:  [2, 1, 2, 1]

Before: [1, 1, 2, 2]
2 0 2 0
After:  [0, 1, 2, 2]

Before: [1, 1, 1, 2]
5 2 1 3
After:  [1, 1, 1, 2]

Before: [2, 1, 3, 1]
11 0 3 3
After:  [2, 1, 3, 1]

Before: [2, 2, 1, 2]
14 2 1 1
After:  [2, 2, 1, 2]

Before: [0, 0, 2, 3]
15 3 3 3
After:  [0, 0, 2, 1]

Before: [2, 0, 3, 1]
7 0 1 2
After:  [2, 0, 1, 1]

Before: [3, 1, 3, 2]
9 1 2 0
After:  [0, 1, 3, 2]

Before: [0, 3, 3, 1]
13 3 3 1
After:  [0, 0, 3, 1]

Before: [1, 1, 1, 3]
6 1 3 2
After:  [1, 1, 0, 3]

Before: [3, 2, 2, 0]
15 2 1 1
After:  [3, 1, 2, 0]

Before: [0, 2, 1, 2]
14 2 1 1
After:  [0, 2, 1, 2]

Before: [3, 3, 2, 3]
15 3 3 3
After:  [3, 3, 2, 1]

Before: [2, 1, 1, 3]
5 2 1 2
After:  [2, 1, 2, 3]

Before: [2, 3, 2, 1]
11 0 3 2
After:  [2, 3, 1, 1]

Before: [3, 3, 2, 2]
7 3 2 3
After:  [3, 3, 2, 0]

Before: [1, 1, 3, 3]
0 1 0 2
After:  [1, 1, 1, 3]

Before: [0, 1, 2, 1]
4 3 2 0
After:  [1, 1, 2, 1]

Before: [2, 1, 3, 0]
8 0 1 3
After:  [2, 1, 3, 1]

Before: [2, 1, 1, 3]
6 2 3 1
After:  [2, 0, 1, 3]

Before: [1, 2, 2, 1]
4 3 2 3
After:  [1, 2, 2, 1]

Before: [0, 2, 0, 3]
15 3 1 3
After:  [0, 2, 0, 0]

Before: [0, 3, 2, 1]
4 3 2 1
After:  [0, 1, 2, 1]

Before: [3, 1, 2, 2]
7 3 2 0
After:  [0, 1, 2, 2]

Before: [3, 1, 3, 2]
9 1 2 1
After:  [3, 0, 3, 2]

Before: [1, 1, 1, 1]
0 1 0 2
After:  [1, 1, 1, 1]

Before: [0, 2, 1, 1]
14 2 1 3
After:  [0, 2, 1, 2]

Before: [1, 1, 3, 2]
9 1 2 1
After:  [1, 0, 3, 2]

Before: [2, 0, 2, 1]
11 0 3 2
After:  [2, 0, 1, 1]

Before: [2, 1, 1, 3]
8 0 1 1
After:  [2, 1, 1, 3]

Before: [0, 3, 2, 2]
10 0 0 2
After:  [0, 3, 0, 2]

Before: [1, 2, 0, 0]
3 0 2 2
After:  [1, 2, 0, 0]

Before: [3, 0, 2, 1]
4 3 2 1
After:  [3, 1, 2, 1]

Before: [2, 1, 1, 1]
11 0 3 2
After:  [2, 1, 1, 1]

Before: [2, 1, 1, 2]
5 2 1 1
After:  [2, 2, 1, 2]

Before: [1, 1, 0, 1]
1 1 3 0
After:  [1, 1, 0, 1]

Before: [0, 3, 3, 1]
13 3 3 0
After:  [0, 3, 3, 1]

Before: [0, 3, 2, 2]
10 0 0 0
After:  [0, 3, 2, 2]

Before: [3, 1, 2, 1]
1 1 3 3
After:  [3, 1, 2, 1]

Before: [2, 0, 3, 2]
7 0 1 1
After:  [2, 1, 3, 2]

Before: [0, 1, 3, 0]
9 1 2 3
After:  [0, 1, 3, 0]

Before: [1, 1, 2, 3]
12 1 2 3
After:  [1, 1, 2, 0]

Before: [1, 1, 2, 3]
0 1 0 3
After:  [1, 1, 2, 1]

Before: [1, 3, 0, 1]
3 0 2 3
After:  [1, 3, 0, 0]

Before: [1, 1, 2, 2]
12 1 2 1
After:  [1, 0, 2, 2]

Before: [3, 2, 1, 3]
14 2 1 2
After:  [3, 2, 2, 3]

Before: [2, 2, 1, 0]
14 2 1 2
After:  [2, 2, 2, 0]

Before: [2, 1, 3, 1]
1 1 3 0
After:  [1, 1, 3, 1]

Before: [1, 1, 1, 1]
5 2 1 0
After:  [2, 1, 1, 1]

Before: [3, 1, 1, 3]
5 2 1 0
After:  [2, 1, 1, 3]

Before: [1, 1, 0, 1]
0 1 0 3
After:  [1, 1, 0, 1]

Before: [0, 3, 1, 3]
10 0 0 2
After:  [0, 3, 0, 3]

Before: [1, 0, 0, 1]
3 0 2 3
After:  [1, 0, 0, 0]

Before: [0, 2, 1, 3]
14 2 1 3
After:  [0, 2, 1, 2]

Before: [1, 1, 3, 2]
15 2 1 2
After:  [1, 1, 0, 2]

Before: [3, 1, 3, 3]
9 1 2 0
After:  [0, 1, 3, 3]

Before: [2, 0, 2, 1]
4 3 2 2
After:  [2, 0, 1, 1]

Before: [2, 0, 2, 2]
7 3 2 1
After:  [2, 0, 2, 2]

Before: [2, 3, 2, 3]
15 3 2 0
After:  [0, 3, 2, 3]

Before: [2, 1, 1, 0]
7 2 1 0
After:  [0, 1, 1, 0]

Before: [1, 0, 0, 2]
3 0 2 2
After:  [1, 0, 0, 2]

Before: [1, 2, 2, 1]
4 3 2 0
After:  [1, 2, 2, 1]

Before: [0, 2, 1, 1]
10 0 0 3
After:  [0, 2, 1, 0]

Before: [3, 3, 2, 1]
8 0 2 3
After:  [3, 3, 2, 1]

Before: [3, 3, 2, 1]
8 0 2 0
After:  [1, 3, 2, 1]

Before: [2, 1, 1, 1]
8 0 1 1
After:  [2, 1, 1, 1]

Before: [1, 1, 2, 2]
2 0 2 2
After:  [1, 1, 0, 2]

Before: [1, 3, 2, 2]
2 0 2 2
After:  [1, 3, 0, 2]

Before: [2, 1, 1, 3]
5 2 1 1
After:  [2, 2, 1, 3]

Before: [2, 1, 3, 2]
8 0 1 1
After:  [2, 1, 3, 2]

Before: [0, 1, 3, 3]
15 2 1 1
After:  [0, 0, 3, 3]

Before: [1, 1, 2, 1]
0 1 0 3
After:  [1, 1, 2, 1]

Before: [3, 2, 0, 3]
6 1 3 1
After:  [3, 0, 0, 3]

Before: [2, 1, 2, 2]
8 0 1 3
After:  [2, 1, 2, 1]

Before: [0, 3, 0, 0]
10 0 0 0
After:  [0, 3, 0, 0]

Before: [3, 1, 1, 0]
5 2 1 3
After:  [3, 1, 1, 2]

Before: [1, 1, 0, 2]
3 0 2 2
After:  [1, 1, 0, 2]

Before: [0, 1, 2, 3]
6 1 3 1
After:  [0, 0, 2, 3]

Before: [0, 3, 1, 1]
13 3 3 1
After:  [0, 0, 1, 1]

Before: [0, 1, 2, 1]
7 3 1 1
After:  [0, 0, 2, 1]

Before: [1, 0, 0, 0]
3 0 2 0
After:  [0, 0, 0, 0]

Before: [3, 1, 2, 1]
1 1 3 2
After:  [3, 1, 1, 1]

Before: [1, 3, 2, 1]
2 0 2 0
After:  [0, 3, 2, 1]

Before: [0, 1, 2, 3]
12 1 2 1
After:  [0, 0, 2, 3]

Before: [1, 1, 0, 2]
13 3 3 1
After:  [1, 0, 0, 2]

Before: [0, 1, 2, 3]
10 0 0 1
After:  [0, 0, 2, 3]

Before: [1, 3, 2, 0]
2 0 2 3
After:  [1, 3, 2, 0]

Before: [1, 1, 2, 1]
1 1 3 0
After:  [1, 1, 2, 1]

Before: [1, 1, 2, 0]
12 1 2 3
After:  [1, 1, 2, 0]

Before: [2, 3, 1, 1]
11 0 3 3
After:  [2, 3, 1, 1]

Before: [3, 3, 0, 2]
7 0 2 3
After:  [3, 3, 0, 1]

Before: [0, 3, 0, 1]
10 0 0 1
After:  [0, 0, 0, 1]

Before: [3, 3, 1, 2]
13 3 3 1
After:  [3, 0, 1, 2]

Before: [1, 1, 3, 2]
0 1 0 3
After:  [1, 1, 3, 1]

Before: [3, 3, 2, 2]
8 0 2 0
After:  [1, 3, 2, 2]

Before: [3, 2, 1, 0]
14 2 1 0
After:  [2, 2, 1, 0]

Before: [1, 1, 3, 2]
13 3 3 2
After:  [1, 1, 0, 2]

Before: [2, 1, 2, 2]
7 3 2 1
After:  [2, 0, 2, 2]

Before: [1, 3, 2, 1]
2 0 2 1
After:  [1, 0, 2, 1]

Before: [1, 1, 3, 1]
0 1 0 1
After:  [1, 1, 3, 1]

Before: [2, 0, 3, 1]
11 0 3 1
After:  [2, 1, 3, 1]

Before: [0, 2, 1, 0]
14 2 1 0
After:  [2, 2, 1, 0]

Before: [1, 1, 3, 1]
9 1 2 1
After:  [1, 0, 3, 1]

Before: [3, 1, 3, 3]
9 1 2 3
After:  [3, 1, 3, 0]

Before: [2, 0, 2, 1]
4 3 2 3
After:  [2, 0, 2, 1]

Before: [1, 1, 2, 2]
12 1 2 0
After:  [0, 1, 2, 2]

Before: [2, 0, 3, 1]
7 2 0 0
After:  [1, 0, 3, 1]

Before: [1, 3, 2, 2]
7 3 2 2
After:  [1, 3, 0, 2]

Before: [1, 1, 1, 0]
0 1 0 1
After:  [1, 1, 1, 0]

Before: [2, 2, 1, 3]
14 2 1 1
After:  [2, 2, 1, 3]

Before: [1, 3, 3, 1]
13 3 3 3
After:  [1, 3, 3, 0]

Before: [3, 2, 2, 3]
6 1 3 1
After:  [3, 0, 2, 3]

Before: [1, 1, 0, 0]
3 0 2 1
After:  [1, 0, 0, 0]

Before: [1, 2, 1, 3]
14 2 1 3
After:  [1, 2, 1, 2]

Before: [3, 2, 2, 2]
7 3 2 2
After:  [3, 2, 0, 2]

Before: [1, 2, 0, 2]
3 0 2 3
After:  [1, 2, 0, 0]

Before: [0, 1, 2, 1]
1 1 3 0
After:  [1, 1, 2, 1]

Before: [1, 1, 0, 1]
3 0 2 3
After:  [1, 1, 0, 0]

Before: [0, 2, 3, 0]
10 0 0 3
After:  [0, 2, 3, 0]

Before: [2, 1, 2, 3]
12 1 2 0
After:  [0, 1, 2, 3]

Before: [2, 1, 2, 2]
12 1 2 0
After:  [0, 1, 2, 2]

Before: [0, 1, 3, 2]
10 0 0 3
After:  [0, 1, 3, 0]

Before: [3, 0, 2, 1]
4 3 2 3
After:  [3, 0, 2, 1]

Before: [1, 2, 2, 3]
15 2 1 3
After:  [1, 2, 2, 1]

Before: [0, 0, 1, 2]
10 0 0 1
After:  [0, 0, 1, 2]

Before: [1, 2, 1, 2]
14 2 1 0
After:  [2, 2, 1, 2]

Before: [2, 1, 3, 3]
9 1 2 2
After:  [2, 1, 0, 3]

Before: [2, 2, 2, 2]
15 2 0 0
After:  [1, 2, 2, 2]

Before: [1, 1, 3, 2]
9 1 2 2
After:  [1, 1, 0, 2]

Before: [1, 2, 0, 2]
13 3 3 3
After:  [1, 2, 0, 0]

Before: [0, 2, 1, 0]
14 2 1 2
After:  [0, 2, 2, 0]

Before: [2, 2, 1, 1]
13 3 3 2
After:  [2, 2, 0, 1]

Before: [2, 1, 1, 2]
7 2 1 3
After:  [2, 1, 1, 0]

Before: [2, 0, 3, 2]
13 3 3 1
After:  [2, 0, 3, 2]

Before: [0, 2, 1, 1]
14 2 1 0
After:  [2, 2, 1, 1]

Before: [1, 2, 2, 1]
2 0 2 2
After:  [1, 2, 0, 1]

Before: [0, 1, 1, 3]
10 0 0 0
After:  [0, 1, 1, 3]

Before: [0, 3, 2, 2]
7 3 2 1
After:  [0, 0, 2, 2]

Before: [0, 1, 1, 2]
5 2 1 2
After:  [0, 1, 2, 2]

Before: [1, 1, 2, 0]
2 0 2 1
After:  [1, 0, 2, 0]

Before: [0, 1, 3, 1]
13 3 3 2
After:  [0, 1, 0, 1]

Before: [0, 2, 1, 3]
14 2 1 2
After:  [0, 2, 2, 3]

Before: [0, 1, 2, 3]
12 1 2 2
After:  [0, 1, 0, 3]

Before: [2, 1, 2, 0]
8 0 1 2
After:  [2, 1, 1, 0]

Before: [0, 1, 0, 1]
1 1 3 1
After:  [0, 1, 0, 1]

Before: [2, 2, 2, 1]
4 3 2 3
After:  [2, 2, 2, 1]

Before: [0, 0, 1, 0]
10 0 0 3
After:  [0, 0, 1, 0]

Before: [2, 1, 3, 0]
8 0 1 2
After:  [2, 1, 1, 0]

Before: [0, 1, 3, 1]
9 1 2 0
After:  [0, 1, 3, 1]

Before: [1, 0, 2, 1]
4 3 2 2
After:  [1, 0, 1, 1]

Before: [1, 1, 3, 1]
1 1 3 3
After:  [1, 1, 3, 1]

Before: [3, 1, 2, 2]
15 2 2 2
After:  [3, 1, 1, 2]

Before: [2, 3, 3, 2]
7 2 0 2
After:  [2, 3, 1, 2]

Before: [1, 1, 3, 1]
15 2 1 2
After:  [1, 1, 0, 1]

Before: [2, 2, 1, 2]
14 2 1 0
After:  [2, 2, 1, 2]

Before: [2, 2, 1, 0]
14 2 1 1
After:  [2, 2, 1, 0]

Before: [0, 2, 3, 1]
13 3 3 3
After:  [0, 2, 3, 0]

Before: [2, 1, 0, 2]
8 0 1 1
After:  [2, 1, 0, 2]

Before: [1, 3, 2, 3]
2 0 2 2
After:  [1, 3, 0, 3]

Before: [0, 0, 2, 0]
10 0 0 0
After:  [0, 0, 2, 0]

Before: [1, 1, 1, 1]
7 3 1 3
After:  [1, 1, 1, 0]

Before: [2, 1, 1, 1]
1 1 3 3
After:  [2, 1, 1, 1]

Before: [3, 2, 1, 2]
14 2 1 3
After:  [3, 2, 1, 2]

Before: [2, 2, 0, 1]
11 0 3 2
After:  [2, 2, 1, 1]

Before: [0, 1, 3, 1]
1 1 3 1
After:  [0, 1, 3, 1]

Before: [0, 2, 0, 2]
10 0 0 2
After:  [0, 2, 0, 2]

Before: [2, 2, 1, 3]
6 1 3 2
After:  [2, 2, 0, 3]

Before: [1, 3, 0, 2]
3 0 2 0
After:  [0, 3, 0, 2]

Before: [3, 1, 1, 0]
7 2 1 0
After:  [0, 1, 1, 0]

Before: [1, 1, 0, 1]
0 1 0 1
After:  [1, 1, 0, 1]

Before: [3, 1, 3, 0]
9 1 2 1
After:  [3, 0, 3, 0]

Before: [1, 2, 0, 1]
3 0 2 3
After:  [1, 2, 0, 0]

Before: [3, 0, 2, 1]
13 3 3 0
After:  [0, 0, 2, 1]

Before: [2, 1, 2, 2]
13 3 3 2
After:  [2, 1, 0, 2]

Before: [1, 1, 3, 1]
9 1 2 3
After:  [1, 1, 3, 0]

Before: [1, 1, 3, 1]
1 1 3 2
After:  [1, 1, 1, 1]

Before: [2, 1, 2, 1]
8 0 1 0
After:  [1, 1, 2, 1]

Before: [3, 1, 3, 3]
6 1 3 3
After:  [3, 1, 3, 0]

Before: [0, 3, 1, 2]
10 0 0 3
After:  [0, 3, 1, 0]

Before: [0, 1, 2, 0]
12 1 2 0
After:  [0, 1, 2, 0]

Before: [2, 0, 3, 1]
13 3 3 0
After:  [0, 0, 3, 1]

Before: [0, 1, 1, 3]
6 1 3 1
After:  [0, 0, 1, 3]

Before: [0, 1, 2, 2]
12 1 2 1
After:  [0, 0, 2, 2]

Before: [2, 0, 2, 2]
7 0 1 2
After:  [2, 0, 1, 2]

Before: [1, 0, 2, 2]
2 0 2 1
After:  [1, 0, 2, 2]

Before: [3, 0, 2, 1]
4 3 2 0
After:  [1, 0, 2, 1]

Before: [1, 1, 1, 0]
0 1 0 2
After:  [1, 1, 1, 0]

Before: [3, 3, 2, 1]
4 3 2 2
After:  [3, 3, 1, 1]

Before: [1, 1, 2, 2]
12 1 2 3
After:  [1, 1, 2, 0]

Before: [3, 2, 3, 3]
15 3 1 2
After:  [3, 2, 0, 3]

Before: [0, 1, 3, 2]
9 1 2 1
After:  [0, 0, 3, 2]

Before: [2, 1, 0, 1]
1 1 3 1
After:  [2, 1, 0, 1]

Before: [0, 1, 3, 1]
9 1 2 1
After:  [0, 0, 3, 1]

Before: [1, 2, 1, 2]
14 2 1 2
After:  [1, 2, 2, 2]

Before: [3, 1, 0, 1]
1 1 3 1
After:  [3, 1, 0, 1]

Before: [2, 1, 1, 3]
5 2 1 3
After:  [2, 1, 1, 2]

Before: [3, 2, 2, 1]
4 3 2 2
After:  [3, 2, 1, 1]

Before: [2, 1, 2, 1]
4 3 2 2
After:  [2, 1, 1, 1]

Before: [0, 1, 1, 2]
13 3 3 3
After:  [0, 1, 1, 0]

Before: [1, 2, 2, 0]
2 0 2 3
After:  [1, 2, 2, 0]

Before: [0, 2, 1, 3]
6 2 3 2
After:  [0, 2, 0, 3]

Before: [0, 1, 2, 1]
4 3 2 1
After:  [0, 1, 2, 1]

Before: [2, 2, 1, 1]
14 2 1 1
After:  [2, 2, 1, 1]

Before: [2, 1, 2, 3]
12 1 2 2
After:  [2, 1, 0, 3]

Before: [3, 1, 2, 1]
12 1 2 2
After:  [3, 1, 0, 1]

Before: [2, 1, 2, 1]
1 1 3 1
After:  [2, 1, 2, 1]

Before: [1, 2, 2, 0]
2 0 2 1
After:  [1, 0, 2, 0]

Before: [2, 1, 2, 2]
8 0 1 1
After:  [2, 1, 2, 2]

Before: [2, 1, 1, 3]
5 2 1 0
After:  [2, 1, 1, 3]

Before: [3, 1, 3, 3]
9 1 2 2
After:  [3, 1, 0, 3]

Before: [2, 3, 2, 1]
4 3 2 2
After:  [2, 3, 1, 1]

Before: [3, 3, 1, 1]
13 3 3 1
After:  [3, 0, 1, 1]

Before: [0, 1, 1, 2]
10 0 0 3
After:  [0, 1, 1, 0]

Before: [2, 0, 1, 1]
11 0 3 3
After:  [2, 0, 1, 1]

Before: [3, 1, 3, 1]
1 1 3 1
After:  [3, 1, 3, 1]

Before: [2, 1, 3, 1]
9 1 2 2
After:  [2, 1, 0, 1]

Before: [0, 1, 2, 1]
10 0 0 3
After:  [0, 1, 2, 0]

Before: [1, 0, 2, 2]
2 0 2 0
After:  [0, 0, 2, 2]

Before: [0, 1, 3, 3]
9 1 2 2
After:  [0, 1, 0, 3]

Before: [1, 1, 0, 3]
0 1 0 3
After:  [1, 1, 0, 1]

Before: [3, 3, 2, 0]
8 0 2 0
After:  [1, 3, 2, 0]

Before: [1, 1, 2, 3]
12 1 2 1
After:  [1, 0, 2, 3]

Before: [2, 1, 2, 1]
12 1 2 0
After:  [0, 1, 2, 1]

Before: [1, 0, 2, 1]
4 3 2 0
After:  [1, 0, 2, 1]

Before: [1, 2, 0, 2]
3 0 2 2
After:  [1, 2, 0, 2]

Before: [2, 3, 2, 1]
4 3 2 3
After:  [2, 3, 2, 1]

Before: [0, 1, 2, 1]
1 1 3 1
After:  [0, 1, 2, 1]

Before: [2, 1, 2, 1]
11 0 3 3
After:  [2, 1, 2, 1]

Before: [0, 0, 2, 1]
4 3 2 1
After:  [0, 1, 2, 1]

Before: [2, 1, 2, 2]
15 2 0 0
After:  [1, 1, 2, 2]

Before: [2, 1, 3, 1]
9 1 2 3
After:  [2, 1, 3, 0]

Before: [1, 1, 3, 0]
9 1 2 1
After:  [1, 0, 3, 0]

Before: [0, 1, 1, 1]
13 3 3 3
After:  [0, 1, 1, 0]

Before: [2, 3, 1, 3]
6 2 3 2
After:  [2, 3, 0, 3]

Before: [2, 1, 1, 1]
1 1 3 1
After:  [2, 1, 1, 1]

Before: [0, 3, 1, 3]
10 0 0 3
After:  [0, 3, 1, 0]

Before: [2, 1, 3, 2]
9 1 2 1
After:  [2, 0, 3, 2]

Before: [2, 2, 2, 1]
13 3 3 0
After:  [0, 2, 2, 1]

Before: [3, 3, 2, 3]
8 0 2 2
After:  [3, 3, 1, 3]

Before: [1, 1, 0, 2]
0 1 0 1
After:  [1, 1, 0, 2]

Before: [1, 2, 2, 3]
2 0 2 2
After:  [1, 2, 0, 3]

Before: [1, 1, 1, 3]
5 2 1 3
After:  [1, 1, 1, 2]

Before: [2, 1, 1, 1]
8 0 1 3
After:  [2, 1, 1, 1]

Before: [0, 2, 1, 3]
14 2 1 1
After:  [0, 2, 1, 3]

Before: [1, 1, 0, 3]
3 0 2 1
After:  [1, 0, 0, 3]

Before: [0, 1, 1, 0]
5 2 1 3
After:  [0, 1, 1, 2]

Before: [3, 0, 0, 1]
7 0 2 0
After:  [1, 0, 0, 1]

Before: [2, 1, 3, 0]
9 1 2 1
After:  [2, 0, 3, 0]

Before: [2, 1, 1, 3]
6 1 3 2
After:  [2, 1, 0, 3]

Before: [1, 1, 0, 0]
0 1 0 2
After:  [1, 1, 1, 0]

Before: [2, 1, 0, 1]
1 1 3 0
After:  [1, 1, 0, 1]

Before: [3, 1, 1, 1]
1 1 3 2
After:  [3, 1, 1, 1]

Before: [0, 3, 1, 1]
13 2 3 3
After:  [0, 3, 1, 0]

Before: [2, 2, 1, 0]
14 2 1 3
After:  [2, 2, 1, 2]

Before: [1, 1, 3, 0]
9 1 2 3
After:  [1, 1, 3, 0]

Before: [2, 2, 0, 1]
11 0 3 0
After:  [1, 2, 0, 1]

Before: [1, 1, 2, 1]
4 3 2 1
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 1]
11 0 3 2
After:  [2, 1, 1, 1]

Before: [2, 0, 3, 3]
7 2 0 2
After:  [2, 0, 1, 3]

Before: [3, 1, 2, 1]
1 1 3 1
After:  [3, 1, 2, 1]

Before: [1, 1, 2, 1]
1 1 3 2
After:  [1, 1, 1, 1]

Before: [2, 1, 3, 2]
7 2 0 3
After:  [2, 1, 3, 1]

Before: [1, 1, 3, 0]
0 1 0 2
After:  [1, 1, 1, 0]

Before: [0, 2, 3, 3]
15 0 0 1
After:  [0, 1, 3, 3]

Before: [3, 1, 1, 1]
1 1 3 3
After:  [3, 1, 1, 1]

Before: [0, 0, 1, 3]
6 2 3 3
After:  [0, 0, 1, 0]

Before: [2, 1, 0, 1]
7 3 1 1
After:  [2, 0, 0, 1]

Before: [1, 1, 3, 1]
15 2 3 3
After:  [1, 1, 3, 0]

Before: [1, 1, 3, 2]
0 1 0 1
After:  [1, 1, 3, 2]

Before: [0, 1, 3, 3]
6 1 3 1
After:  [0, 0, 3, 3]

Before: [0, 1, 2, 3]
6 2 3 3
After:  [0, 1, 2, 0]

Before: [0, 2, 3, 3]
10 0 0 0
After:  [0, 2, 3, 3]

Before: [2, 1, 0, 0]
8 0 1 2
After:  [2, 1, 1, 0]

Before: [2, 1, 3, 0]
15 2 1 1
After:  [2, 0, 3, 0]

Before: [0, 2, 1, 3]
15 3 1 0
After:  [0, 2, 1, 3]

Before: [0, 1, 3, 1]
1 1 3 0
After:  [1, 1, 3, 1]

Before: [2, 0, 2, 1]
13 3 3 1
After:  [2, 0, 2, 1]

Before: [2, 2, 1, 3]
6 1 3 3
After:  [2, 2, 1, 0]

Before: [2, 0, 2, 2]
7 3 2 0
After:  [0, 0, 2, 2]

Before: [3, 1, 1, 0]
5 2 1 2
After:  [3, 1, 2, 0]

Before: [2, 1, 3, 1]
8 0 1 3
After:  [2, 1, 3, 1]

Before: [1, 2, 2, 1]
4 3 2 1
After:  [1, 1, 2, 1]

Before: [0, 1, 2, 3]
12 1 2 0
After:  [0, 1, 2, 3]

Before: [1, 1, 2, 1]
0 1 0 0
After:  [1, 1, 2, 1]

Before: [1, 1, 1, 3]
5 2 1 1
After:  [1, 2, 1, 3]

"""


inputlatermarek =
    """2 2 3 3
2 0 3 2
2 2 1 0
15 0 3 3
10 3 1 3
5 1 3 1
2 2 3 3
10 1 0 0
14 0 0 0
7 2 3 2
10 2 3 2
5 1 2 1
4 1 1 0
2 3 2 1
10 1 0 2
14 2 0 2
12 1 3 3
10 3 3 3
5 3 0 0
4 0 0 3
10 0 0 0
14 0 2 0
2 1 1 1
2 3 0 2
13 0 2 1
10 1 1 1
5 1 3 3
2 0 3 2
2 1 1 1
3 1 0 2
10 2 1 2
10 2 2 2
5 3 2 3
4 3 0 1
2 1 3 3
10 3 0 2
14 2 3 2
3 3 0 3
10 3 1 3
10 3 3 3
5 3 1 1
2 0 1 3
2 2 0 2
10 0 0 0
14 0 3 0
0 2 3 3
10 3 1 3
5 1 3 1
4 1 2 3
2 1 1 0
2 0 0 1
4 0 2 2
10 2 3 2
5 2 3 3
4 3 2 1
10 2 0 3
14 3 3 3
2 3 1 2
10 2 0 0
14 0 2 0
12 3 0 0
10 0 3 0
5 0 1 1
4 1 0 2
2 3 2 0
2 2 0 1
2 1 0 3
12 0 1 1
10 1 3 1
5 2 1 2
2 2 3 1
2 3 0 3
2 1 3 0
12 3 1 3
10 3 2 3
5 2 3 2
4 2 2 3
2 3 1 1
2 1 0 2
9 1 2 2
10 2 1 2
5 2 3 3
4 3 2 1
10 1 0 2
14 2 2 2
2 3 1 3
4 0 2 3
10 3 3 3
5 3 1 1
2 0 2 0
2 0 1 3
6 3 2 0
10 0 2 0
5 0 1 1
2 3 2 0
10 1 0 3
14 3 2 3
2 1 3 2
2 2 0 3
10 3 3 3
10 3 2 3
5 3 1 1
2 1 0 3
2 1 2 0
2 3 0 2
10 2 2 2
5 1 2 1
4 1 1 0
2 3 3 1
2 3 0 3
10 2 0 2
14 2 3 2
9 1 2 1
10 1 3 1
5 1 0 0
4 0 3 3
10 1 0 1
14 1 2 1
10 0 0 0
14 0 0 0
1 1 2 0
10 0 1 0
5 0 3 3
4 3 0 1
2 0 3 2
10 0 0 3
14 3 1 3
10 3 0 0
14 0 2 0
11 0 3 2
10 2 2 2
5 1 2 1
2 2 3 3
2 3 1 0
2 2 3 2
12 0 3 2
10 2 1 2
5 1 2 1
2 1 1 0
10 1 0 2
14 2 0 2
7 2 3 0
10 0 1 0
5 0 1 1
4 1 1 3
2 1 1 1
2 2 0 0
2 3 1 2
13 0 2 2
10 2 2 2
10 2 1 2
5 2 3 3
2 1 1 0
2 2 0 2
2 2 1 1
4 0 2 2
10 2 3 2
10 2 3 2
5 3 2 3
4 3 3 1
2 1 3 3
2 1 2 2
2 2 2 0
11 0 3 0
10 0 2 0
5 1 0 1
4 1 2 2
2 2 2 3
2 1 2 1
2 2 2 0
15 0 3 3
10 3 1 3
10 3 3 3
5 3 2 2
4 2 2 1
2 0 0 0
2 0 3 3
2 2 2 2
6 3 2 2
10 2 1 2
5 1 2 1
4 1 1 0
2 2 2 2
2 2 1 1
10 3 0 3
14 3 2 3
0 1 3 2
10 2 2 2
5 2 0 0
10 0 0 2
14 2 0 2
2 1 0 3
2 0 1 1
14 3 1 1
10 1 2 1
10 1 2 1
5 1 0 0
4 0 3 1
10 1 0 0
14 0 0 0
2 0 1 3
2 2 2 2
0 2 3 0
10 0 3 0
10 0 2 0
5 1 0 1
2 2 2 0
2 1 1 3
11 0 3 3
10 3 2 3
5 3 1 1
4 1 2 2
10 1 0 3
14 3 2 3
10 1 0 1
14 1 3 1
15 0 3 3
10 3 3 3
5 3 2 2
4 2 0 1
10 0 0 3
14 3 1 3
2 3 3 2
13 0 2 0
10 0 3 0
5 0 1 1
2 2 3 3
2 1 0 0
10 0 2 3
10 3 2 3
5 1 3 1
2 1 1 2
2 1 3 3
2 2 0 0
11 0 3 2
10 2 2 2
5 1 2 1
4 1 3 0
2 3 1 1
10 2 0 2
14 2 0 2
9 1 2 3
10 3 1 3
5 0 3 0
4 0 1 1
2 0 0 3
2 1 3 2
2 2 0 0
0 0 3 3
10 3 1 3
5 3 1 1
4 1 3 3
10 1 0 1
14 1 2 1
10 1 0 0
14 0 3 0
2 3 0 2
1 1 0 0
10 0 3 0
5 0 3 3
10 2 0 2
14 2 1 2
2 3 0 1
2 3 0 0
9 1 2 1
10 1 1 1
10 1 1 1
5 3 1 3
4 3 1 1
2 1 2 0
10 1 0 2
14 2 2 2
2 2 2 3
3 0 3 3
10 3 1 3
10 3 2 3
5 1 3 1
4 1 2 2
2 3 2 1
2 2 3 0
10 2 0 3
14 3 1 3
3 3 0 0
10 0 2 0
10 0 3 0
5 0 2 2
4 2 0 1
10 0 0 3
14 3 0 3
2 3 2 2
2 0 0 0
7 3 2 0
10 0 2 0
5 1 0 1
4 1 3 2
2 3 3 1
2 2 0 0
2 3 3 3
8 0 1 0
10 0 1 0
10 0 3 0
5 0 2 2
4 2 2 3
2 2 3 0
2 2 1 2
8 0 1 0
10 0 1 0
10 0 1 0
5 3 0 3
4 3 1 1
2 3 1 0
2 1 2 2
2 0 3 3
9 0 2 3
10 3 3 3
5 3 1 1
2 2 0 2
10 2 0 0
14 0 1 0
2 1 3 3
5 0 3 0
10 0 3 0
5 1 0 1
4 1 2 3
2 1 2 0
10 3 0 2
14 2 0 2
2 0 3 1
10 0 2 1
10 1 1 1
5 1 3 3
4 3 1 1
2 2 0 2
10 2 0 3
14 3 0 3
6 3 2 0
10 0 1 0
10 0 2 0
5 0 1 1
4 1 1 3
2 3 0 1
2 0 2 2
2 3 2 0
13 2 0 0
10 0 1 0
5 3 0 3
4 3 0 2
2 1 0 0
10 3 0 3
14 3 2 3
2 1 2 1
3 1 3 1
10 1 1 1
5 2 1 2
4 2 3 1
2 0 1 3
2 2 1 2
2 0 1 0
6 3 2 3
10 3 1 3
5 3 1 1
4 1 2 2
2 3 2 3
2 2 2 1
2 2 2 0
12 3 1 0
10 0 1 0
5 0 2 2
2 1 0 0
2 2 2 3
3 0 3 3
10 3 1 3
5 2 3 2
4 2 3 1
2 1 3 2
2 3 3 0
2 2 3 3
12 0 3 3
10 3 1 3
10 3 3 3
5 1 3 1
2 0 3 2
2 2 2 0
2 1 0 3
11 0 3 3
10 3 3 3
5 3 1 1
2 3 3 2
2 0 3 3
1 0 2 2
10 2 1 2
5 2 1 1
4 1 0 2
2 3 0 1
10 2 0 3
14 3 2 3
15 0 3 1
10 1 3 1
10 1 1 1
5 1 2 2
4 2 1 1
2 3 3 2
2 0 1 3
2 0 1 0
7 3 2 2
10 2 1 2
5 1 2 1
4 1 0 0
2 0 2 1
2 2 3 2
6 3 2 2
10 2 1 2
5 2 0 0
4 0 0 1
2 0 0 2
2 3 1 0
13 2 0 0
10 0 2 0
5 1 0 1
4 1 3 0
2 2 3 2
2 3 2 1
6 3 2 3
10 3 1 3
10 3 2 3
5 0 3 0
2 1 3 3
2 0 1 2
14 3 1 2
10 2 3 2
5 2 0 0
2 2 0 3
2 0 1 2
7 2 3 1
10 1 1 1
10 1 3 1
5 1 0 0
4 0 3 1
2 0 1 3
10 3 0 0
14 0 3 0
2 2 2 2
6 3 2 3
10 3 2 3
5 3 1 1
2 1 2 0
2 0 3 3
2 3 2 0
10 0 2 0
5 1 0 1
2 3 1 0
2 3 1 3
8 2 0 2
10 2 3 2
5 1 2 1
4 1 2 3
2 0 2 2
2 2 1 1
13 2 0 2
10 2 1 2
10 2 2 2
5 2 3 3
4 3 2 1
2 1 3 0
2 0 3 3
2 2 2 2
0 2 3 0
10 0 2 0
10 0 1 0
5 1 0 1
2 1 1 0
2 1 3 3
5 3 0 2
10 2 1 2
5 2 1 1
4 1 0 3
10 1 0 2
14 2 1 2
2 2 1 0
2 1 3 1
3 1 0 0
10 0 3 0
5 0 3 3
2 2 2 2
10 3 0 1
14 1 2 1
2 1 3 0
4 0 2 1
10 1 3 1
10 1 2 1
5 3 1 3
4 3 0 1
2 3 0 3
2 2 2 0
2 3 2 2
12 3 0 3
10 3 2 3
5 3 1 1
4 1 1 3
2 3 0 1
9 1 2 0
10 0 3 0
5 0 3 3
4 3 2 1
2 0 2 2
10 2 0 3
14 3 2 3
10 1 0 0
14 0 3 0
7 2 3 0
10 0 1 0
10 0 1 0
5 0 1 1
2 2 1 0
2 2 2 2
15 0 3 0
10 0 2 0
5 0 1 1
4 1 0 0
2 0 2 1
2 0 2 3
2 3 3 2
7 3 2 3
10 3 2 3
5 0 3 0
4 0 2 2
2 2 0 0
2 1 3 3
2 2 1 1
11 0 3 3
10 3 2 3
5 2 3 2
2 1 3 3
2 0 1 1
11 0 3 1
10 1 1 1
5 1 2 2
4 2 3 0
2 2 2 2
2 3 1 1
8 2 1 3
10 3 2 3
10 3 1 3
5 0 3 0
10 0 0 3
14 3 1 3
2 0 1 2
10 1 0 1
14 1 0 1
14 3 1 2
10 2 1 2
10 2 1 2
5 0 2 0
4 0 1 1
2 1 2 2
2 2 0 3
10 3 0 0
14 0 1 0
3 0 3 2
10 2 3 2
5 2 1 1
2 0 2 2
2 1 2 3
10 1 0 0
14 0 0 0
10 3 2 3
10 3 3 3
5 1 3 1
4 1 3 0
2 1 0 2
10 1 0 1
14 1 3 1
2 1 0 3
5 3 3 3
10 3 1 3
10 3 2 3
5 0 3 0
2 0 3 2
2 2 0 3
2 2 3 1
7 2 3 1
10 1 2 1
10 1 2 1
5 0 1 0
4 0 1 2
2 1 1 1
2 3 2 3
10 0 0 0
14 0 2 0
3 1 0 1
10 1 2 1
10 1 2 1
5 2 1 2
4 2 3 0
2 0 1 1
2 3 3 2
2 1 0 3
10 3 2 1
10 1 2 1
5 0 1 0
4 0 3 1
2 1 3 2
2 2 0 0
2 2 0 3
15 0 3 2
10 2 2 2
5 1 2 1
2 1 0 0
2 2 2 2
2 1 0 3
4 0 2 0
10 0 3 0
5 0 1 1
2 1 2 0
2 0 0 3
4 0 2 0
10 0 1 0
10 0 2 0
5 0 1 1
4 1 1 0
2 0 0 1
2 2 1 3
2 0 2 2
7 2 3 2
10 2 1 2
5 0 2 0
2 2 0 1
2 0 3 2
7 2 3 2
10 2 3 2
5 2 0 0
4 0 1 3
2 1 0 0
2 2 1 2
2 1 1 1
5 1 0 0
10 0 1 0
5 0 3 3
4 3 2 0
2 3 2 1
2 1 0 2
10 0 0 3
14 3 3 3
9 1 2 2
10 2 1 2
10 2 2 2
5 0 2 0
4 0 1 2
2 2 3 1
2 0 2 3
2 1 2 0
5 0 0 3
10 3 2 3
5 2 3 2
4 2 3 3
10 1 0 1
14 1 0 1
2 2 2 2
4 0 2 1
10 1 3 1
10 1 3 1
5 3 1 3
4 3 2 0
2 2 0 1
2 3 1 3
10 0 0 2
14 2 0 2
9 3 2 1
10 1 1 1
10 1 2 1
5 0 1 0
4 0 2 2
2 2 0 3
2 3 1 1
2 2 2 0
15 0 3 0
10 0 1 0
10 0 2 0
5 0 2 2
2 2 1 0
2 0 0 1
2 1 0 3
14 3 1 1
10 1 1 1
5 2 1 2
2 1 3 1
11 0 3 0
10 0 1 0
5 0 2 2
4 2 1 0
2 0 3 1
2 0 1 3
2 2 1 2
0 2 3 3
10 3 2 3
5 3 0 0
4 0 2 1
10 3 0 3
14 3 1 3
2 2 3 0
2 1 1 2
3 3 0 0
10 0 1 0
10 0 2 0
5 1 0 1
2 1 3 0
2 2 0 2
4 0 2 2
10 2 1 2
5 1 2 1
10 2 0 3
14 3 0 3
2 2 2 2
6 3 2 2
10 2 1 2
5 2 1 1
2 2 1 2
2 3 2 0
8 2 0 0
10 0 2 0
5 1 0 1
4 1 2 3
2 0 0 1
2 3 2 0
2 3 3 2
9 0 2 1
10 1 2 1
10 1 1 1
5 3 1 3
4 3 2 0
10 1 0 1
14 1 1 1
2 2 2 3
10 1 2 3
10 3 3 3
10 3 3 3
5 0 3 0
4 0 1 2
2 2 2 0
10 3 0 1
14 1 2 1
2 1 2 3
11 0 3 1
10 1 2 1
5 1 2 2
4 2 2 1
2 3 1 2
11 0 3 0
10 0 3 0
5 0 1 1
4 1 1 3
10 3 0 1
14 1 1 1
2 2 0 0
1 0 2 1
10 1 3 1
5 1 3 3
4 3 0 1
2 1 3 0
2 3 0 3
2 2 2 2
4 0 2 2
10 2 3 2
5 1 2 1
10 0 0 2
14 2 3 2
2 2 0 0
13 0 2 0
10 0 1 0
10 0 3 0
5 1 0 1
4 1 0 3
2 1 2 0
10 1 0 1
14 1 3 1
9 1 2 0
10 0 1 0
5 3 0 3
4 3 0 1
2 2 3 2
2 2 3 0
2 0 2 3
6 3 2 0
10 0 2 0
5 1 0 1
4 1 3 0
2 3 3 2
2 1 1 1
10 2 0 3
14 3 3 3
10 1 2 1
10 1 3 1
10 1 2 1
5 0 1 0
4 0 2 3
2 0 1 2
10 1 0 1
14 1 1 1
2 2 3 0
3 1 0 0
10 0 3 0
5 0 3 3
4 3 3 1
2 3 0 0
2 0 1 3
2 3 2 2
9 0 2 3
10 3 3 3
5 1 3 1
4 1 0 0
2 2 0 3
10 3 0 2
14 2 2 2
2 1 0 1
3 1 3 2
10 2 1 2
5 0 2 0
2 0 0 2
10 3 0 1
14 1 2 1
2 3 3 3
12 3 1 2
10 2 1 2
5 0 2 0
4 0 0 1
2 1 2 3
2 3 1 2
2 1 1 0
5 0 3 3
10 3 3 3
5 1 3 1
4 1 0 0
2 3 1 1
2 0 2 3
7 3 2 2
10 2 3 2
10 2 1 2
5 2 0 0
4 0 2 1
2 1 2 3
2 3 3 0
2 2 3 2
8 2 0 3
10 3 1 3
5 3 1 1
10 3 0 2
14 2 0 2
2 1 0 3
13 2 0 2
10 2 1 2
5 1 2 1
4 1 0 0
10 0 0 2
14 2 1 2
2 2 2 1
2 3 0 3
9 3 2 1
10 1 2 1
5 1 0 0
4 0 1 1
10 3 0 2
14 2 3 2
2 3 2 0
2 2 3 2
10 2 3 2
10 2 1 2
5 2 1 1
4 1 0 0"""
