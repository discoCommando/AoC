module E19 exposing (..)

import Array exposing (Array, isEmpty)
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




stringToOperationType : String -> OperationType
stringToOperationType string =
    case string of
        "addr" ->
            Addr

        "addi" ->
            Addi

        "mulr" ->
            Mulr

        "muli" ->
            Muli

        "banr" ->
            Banr

        "bani" ->
            Bani

        "borr" ->
            Borr

        "bori" ->
            Bori

        "setr" ->
            Setr

        "seti" ->
            Seti

        "gtir" ->
            Gtir

        "gtri" ->
            Gtri

        "gtrr" ->
            Gtrr

        "eqir" ->
            Eqir

        "eqri" ->
            Eqri

        "eqrr" ->
            Eqrr

        _ ->
            Debug.crash ""


type alias Operation =
    { opcode : OperationType
    , inputA : Int
    , inputB : Int
    , outputA : Int
    }


type alias Registers =
    Dict Int Int


parseOperation : Parser Operation
parseOperation =
    succeed Operation
        |= (keep (Parser.Exactly 4) (\_ -> True) |> Parser.map stringToOperationType)
        |. H.spaces
        |= H.pInt
        |. H.spaces
        |= H.pInt
        |. H.spaces
        |= H.pInt


getUnsafe : Int -> Dict Int Int -> Int
getUnsafe int intIntDict =
    intIntDict |> Dict.get int |> H.uM


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1
    else
        0


interpretOperation : Operation -> Dict Int Int -> Dict Int Int
interpretOperation { opcode, inputA, inputB, outputA } initialRegister =
    case opcode of
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



-- isOperation : Test -> OperationType -> Bool
-- isOperation test operationType =
--     interpretOperation test.operation operationType test.before == test.after
--
--
-- getOperations : Test -> List OperationType
-- getOperations test =
--     allOperations |> List.filter (isOperation test)


ip : Int
ip =
    1


part1 =
    input
        |> String.lines
        |> List.map (Parser.run parseOperation >> H.uR)
        |> initialState
        |> run 200
        |> Debug.log "answer1"


type alias State =
    { operations : Array Operation
    , registers : Dict Int Int
    }


initialState : List Operation -> State
initialState operationList =
    { operations = operationList |> Array.fromList, registers = [ 1, 0, 0, 0, 0, 0 ] |> List.indexedMap (, ) |> Dict.fromList }



test2 =
    List.range 1 10551306 |> List.filter (\x -> 10551306 % x == 0) |> List.sum |> Debug.log "part 2 answer"


run : Int -> State -> State
run i state =
    let
        _ =
            Debug.log (toString i) state.registers
    in
    if i == 0 then
        state
    else
        case Array.get (Dict.get ip state.registers |> H.uM) state.operations of
            Nothing ->
                state

            Just o ->
                run (i - 1) { state | registers = interpretOperation o state.registers |> Dict.update ip (Maybe.map ((+) 1)) }


inpute : String



-- ip 0


inpute =
    """seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5"""


input : String



-- ip 1


input =
    """addi 1 16 1
seti 1 2 5
seti 1 2 2
mulr 5 2 3
eqrr 3 4 3
addr 3 1 1
addi 1 1 1
addr 5 0 0
addi 2 1 2
gtrr 2 4 3
addr 1 3 1
seti 2 8 1
addi 5 1 5
gtrr 5 4 3
addr 3 1 1
seti 1 1 1
mulr 1 1 1
addi 4 2 4
mulr 4 4 4
mulr 1 4 4
muli 4 11 4
addi 3 3 3
mulr 3 1 3
addi 3 4 3
addr 4 3 4
addr 1 0 1
seti 0 0 1
setr 1 5 3
mulr 3 1 3
addr 1 3 3
mulr 1 3 3
muli 3 14 3
mulr 3 1 3
addr 4 3 4
seti 0 0 0
seti 0 1 1"""
