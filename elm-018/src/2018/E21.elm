module E21 exposing (..)

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


ip =
    5


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


interpretOperationOriginal : Operation -> Dict Int Int -> Dict Int Int
interpretOperationOriginal { opcode, inputA, inputB, outputA } initialRegister =
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


interpretOperation : Operation -> Dict Int Int -> Dict Int Int
interpretOperation { opcode, inputA, inputB, outputA } initialRegister =
    case opcode of
        Addr ->
            initialRegister |> Dict.insert outputA ((getUnsafe inputA initialRegister + getUnsafe inputB initialRegister) |> Bitwise.and ("0xFFFFFF" |> H.toI))

        Addi ->
            initialRegister |> Dict.insert outputA ((getUnsafe inputA initialRegister + inputB) |> Bitwise.and ("0xFFFFFF" |> H.toI))

        Mulr ->
            initialRegister |> Dict.insert outputA ((getUnsafe inputA initialRegister * getUnsafe inputB initialRegister) |> Bitwise.and ("0xFFFFFF" |> H.toI))

        Muli ->
            initialRegister |> Dict.insert outputA ((getUnsafe inputA initialRegister * inputB) |> Bitwise.and ("0xFFFFFF" |> H.toI))

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
-- part1 =
--     input
--         |> String.lines
--         |> List.map (Parser.run parseOperation >> H.uR)
--         |> initialState
--         |> run -1
--         |> Debug.log "answer1"


type alias State =
    { operations : Array Operation
    , registers : Dict Int Int
    , states : Dict (List ( Int, Int )) (Dict Int Int)
    }


initialState : List Operation -> State
initialState operationList =
    { operations = operationList |> Array.fromList
    , registers = initialRegisters
    , states = Dict.empty
    }


initialRegisters =
    [ 7891345, 0, 0, 0, 0, 0 ] |> List.indexedMap (,) |> Dict.fromList


findState : Int -> State -> Int
findState value state =
    if value == 100 then
        value
    else if (run 100 state |> .registers |> Dict.get ip |> H.uM) == 18 then
        findState (value + 1) { state | registers = initialRegisters |> Dict.insert 0 value }
    else
        value


run : Int -> State -> State
run i state =
    if i == 0 then
        state
    else
        let
            operationIndex =
                Dict.get ip state.registers |> H.uM
        in
        if operationIndex == 26 then
            state
        else
            case Array.get operationIndex state.operations of
                Nothing ->
                    state

                Just o ->
                    let
                        _ =
                            Debug.log (toString (Dict.get ip state.registers |> H.uM) ++ " " ++ toString o) state.registers

                        newRegisters =
                            interpretOperationOriginal o state.registers |> Dict.update ip (Maybe.map ((+) 1))
                    in
                    run (i - 1)
                        { state
                            | registers = newRegisters
                        }


initialX =
    3173684


type alias State2 =
    { existing : Dict Int Int }


findX : Int -> Int -> State2 -> State2
findX value steps state =
    case state.existing |> Dict.get value of
        Just _ ->
            state

        Nothing ->
            let
                newValue =
                    value |> Bitwise.and 16777215 |> (*) 65899 |> Bitwise.and 16777215
            in
            findX newValue (steps + 1) <| State2 (state.existing |> Dict.insert value steps)



-- ((3173684 & 16777215) * 65899 ) & 16777215
-- test2 =
--     findX 3173684 0 (State2 Dict.empty)
--         |> Debug.log "end"
--         |> .existing
--         |> Dict.toList
--         |> List.sortBy Tuple.second
--         |> List.reverse
--         |> H.at 0
--         |> Tuple.first
--         |> Debug.log "result"


input =
    """seti 123 0 3
bani 3 456 3
eqri 3 72 3
addr 3 5 5
seti 0 0 5
seti 0 9 3
bori 3 65536 1
seti 14906355 8 3
bani 1 255 4
addr 3 4 3
bani 3 16777215 3
muli 3 65899 3
bani 3 16777215 3
gtir 256 1 4
addr 4 5 5
addi 5 1 5
seti 27 8 5
seti 0 4 4
addi 4 1 2
muli 2 256 2
gtrr 2 1 2
addr 2 5 5
addi 5 1 5
seti 25 1 5
addi 4 1 4
seti 17 2 5
setr 4 9 1
seti 7 0 5
eqrr 3 0 4
addr 4 5 5
seti 5 3 5"""


pseudocode =
    """0: 3 to 123
1: 3 to 123 & 456 = 72
2: 3 to 1
3: if 3 then goto 5
4: seti 0 0 5
5: 3 to 0
6: 1 to 65536
7: 3 to 14906355
8: 4 to 0
9: 3 to 14906355 --addr 3 4 3
10: 3 to  14906355 & 16777215
11: 3 to 14906355 *65899 = 982313888145
12: 3 to 982313888145 & 16777215 = 7891345
13: 4 to 0
14: goto 15 -- addr 4 5 5
15: goto 17 -- addi 5 1 5
16: goto 28 -- seti 27 8 5
17: 4 to 0 -- seti 0 4 4
18: 2 to get 4 + 1 -- addi 4 1 2
19: 2 *= 256 -- muli 2 256 2
20: 2 to get 2 > get 1 -- gtrr 2 1 2
21: if 2 then goto 23 -- addr 2 5 5
22: goto 24 -- addi 5 1 5
23: goto 26 -- seti 25 1 5
24: 4 += 1 -- addi 4 1 4
25: goto 18 seti 17 2 5
26: 1 to get 4 -- setr 4 9 1
27: goto 8 -- seti 7 0 5
28: 4 to get 3 == get 0 -- eqrr 3 0 4
29: if get 4 then finish -- addr 4 5 5
30: goto 6 -- seti 5 3 5

3 to 7891345
4 to 0
1 to 65536
while get 2 <= get 1
    2 to get 4 + 1
    2 *= 256

if get 3 == get 0 then finish


0: 3 = 123 seti 123 0 3
1: 3 = v(3) & 456 = 72 bani 3 456 3
2: 3 = 72 == 72 eqri 3 72 3
3: goto 5 addr 3 5 5
4: --seti 0 0 5
5: 3 = 0 seti 0 9 3
6: 1 = v(3) | 65536 = 65536 bori 3 65536 1
7: 3 = 14906355 seti 14906355 8 3
8: 4 = v(1) & 255 = 0 bani 1 255 4
9: 3 = v(4) + v(3) = v(3) addr 3 4 3
10: 3 &= 16777215 =14906355 & 16777215 = 14906355 bani 3 16777215 3
11: 3 *= 65899 muli 3 65899 3
12: 3 &= 16777215 bani 3 16777215 3
13: 4 = 256 > v(1) = 0 gtir 256 1 4
14: if 4 then goto 16 addr 4 5 5
15: goto 17 addi 5 1 5
16: goto 28 seti 27 8 5
17: 4 = 0 seti 0 4 4
18: 2 = v 4 + 1 addi 4 1 2
19: 2 *= 256 muli 2 256 2
20: 2 = v(2) > v(1) gtrr 2 1 2
21: if 2 then goto 23 addr 2 5 5
22: goto 24 addi 5 1 5
23: goto 26 seti 25 1 5
24: 4 += 1 addi 4 1 4
25: goto 18 seti 17 2 5
26: 1 = v(4) setr 4 9 1
27: goto 8 seti 7 0 5
28: 4 = v(3) == v(0) eqrr 3 0 4
29: if 4 then finish addr 4 5 5
30: goto 6 seti 5 3 5

seti 123 0 3
bani 3 456 3
eqri 3 72 3
addr 3 5 5
seti 0 0 5
seti 0 9 3
bori 3 65536 1
seti 14906355 8 3
bani 1 255 4
addr 3 4 3
bani 3 16777215 3
muli 3 65899 3
bani 3 16777215 3
gtir 256 1 4
addr 4 5 5
addi 5 1 5
seti 27 8 5
seti 0 4 4
addi 4 1 2
muli 2 256 2
gtrr 2 1 2
addr 2 5 5
addi 5 1 5
seti 25 1 5
addi 4 1 4
seti 17 2 5
setr 4 9 1
seti 7 0 5
eqrr 3 0 4
addr 4 5 5
seti 5 3 5

3 = 0

z:1 = v 3 | 65536
3 = 14906355
    x: 4 = v 1 & 255
    3 = v 4 + v 3
    3 &= 16777215
    3 *= 65899
    3 &= 16777215
    if 256 > v 1
        if v 3 == v 0
            return
        else
            goto z:

    4 = 0
    y: 2 = v 4 + 1
    2 *= 256
    if v 2 > v 1
        1 = v 4
        goto x
    4 += 1
    goto y


    1 256*256, 3 ((14906355 & 16777215) * 65899) & 16777215 = 7891345
    y loop
    4 256, 1 256, 3 ((7891345 & 16777215) * 65899 ) & 16777215 = 5157019
    y loop
    4 1, 1 1, 3 (((5157019 + 1) & 16777215) * 65899 ) & 16777215 = 3173684
    y loop
    4 0, 1 0, 3 ((3173684 & 16777215) * 65899 ) & 16777215
    end

"""


type alias State3 =
    { t1 : Int
    , t2 : Int
    , t3 : Int
    , t4 : Int
    , state : Dict Int Int
    , step : Int
    , lastt3 : Int
    }


statetolist state =
    [ state.t1, state.t2, state.t3, state.t4 ]


mainloop : State3 -> State3
mainloop state =
    let
        recurse s =
            let
                s_ =
                    loopx s
            in
            if 256 > s_.t1 then
                if Dict.member s_.t3 s_.state then
                    s_ |> Debug.log "a"
                else
                    recurse
                        { s_
                            | state = s_.state |> Dict.insert s_.t3 s_.step
                            , lastt3 = s_.t3
                            , t1 = s_.t3 |> Bitwise.or 65536
                            , t3 = 14906355
                            , step = s_.step + 1
                        }
            else
                recurse (loopy { s_ | t4 = 0 })
    in
    recurse { state | t1 = state.t3 |> Bitwise.or 65536, t3 = 14906355 }


loopx : State3 -> State3
loopx state =
    let
        s1 =
            { state | t4 = state.t1 |> Bitwise.and 255 }

        s2 =
            { s1 | t3 = s1.t4 + s1.t3 }
    in
    { s2 | t3 = s2.t3 |> Bitwise.and 16777215 |> (*) 65899 |> Bitwise.and 16777215, t4 = boolToInt (256 > s2.t1) }


loopy : State3 -> State3
loopy state =
    { state | t4 = state.t1 // 256, t2 = 1, t1 = state.t1 // 256 }


test3 =
    State3 0 0 0 0 Dict.empty 1 0
        |> mainloop
        |> (\s ->
                let
                    _ =
                        Debug.log "res" s.lastt3
                in
                s
           )
        |> Debug.log "result"
        |> .state
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> (\x ->
                let
                    _ =
                        Debug.log "as" ( H.at 0 x, H.at 1 x )
                in
                x
           )
        |> H.at 0
        |> Tuple.first
        |> Debug.log "result2"
