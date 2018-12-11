module E10 exposing (..)

import Char
import Dict exposing (Dict)
import Helpers as H
import List
import List.Extra
import Parser exposing ((|.), (|=), Parser, keep, keyword, succeed, symbol)
import Regex
import Set exposing (Set)
import String


type alias Register =
    Char


type From
    = RegisterI Register
    | Value Int


type Command
    = Copy From Register
    | Inc Register
    | Dec Register
    | Jump From Int


registerParser : Parser Register
registerParser =
    keep (Parser.Exactly 1) (always True) |> Parser.map (String.toList >> H.at 0)


fromParser : Parser From
fromParser =
    Parser.oneOf
        [ succeed Value
            |= H.pInt
        , succeed RegisterI
            |= registerParser
        ]


commandParser : Parser Command
commandParser =
    Parser.oneOf
        [ succeed Copy
            |. keyword "cpy "
            |= fromParser
            |. H.spaces
            |= registerParser
        , succeed Inc
            |. keyword "inc "
            |= registerParser
        , succeed Dec
            |. keyword "dec "
            |= registerParser
        , succeed Jump
            |. keyword "jnz "
            |= fromParser
            |. H.spaces
            |= H.pInt
        ]


type alias State =
    { register : Dict Register Int
    , commands : Dict Int Command
    }


prepareState : List Command -> State
prepareState commands =
    let
        commandsDict =
            commands |> List.indexedMap (,) |> Dict.fromList
    in
    State ([ ( 'a', 0 ), ( 'b', 0 ), ( 'c', 1 ), ( 'd', 0 ) ] |> Dict.fromList) commandsDict


recurse : Int -> State -> State
recurse index state =
    case Dict.get index state.commands of
        Nothing ->
            state

        Just command ->
            case command of
                Copy from register ->
                    case from of
                        RegisterI register2 ->
                            case Dict.get register2 state.register of
                                Nothing ->
                                    Debug.crash ""

                                Just value ->
                                    recurse (index + 1) { state | register = state.register |> Dict.insert register value }

                        Value value ->
                            recurse (index + 1) { state | register = state.register |> Dict.insert register value }

                Inc register ->
                    case Dict.get register state.register of
                        Nothing ->
                            Debug.crash ""

                        Just value ->
                            recurse (index + 1) { state | register = state.register |> Dict.insert register (value + 1) }

                Dec register ->
                    case Dict.get register state.register of
                        Nothing ->
                            Debug.crash ""

                        Just value ->
                            recurse (index + 1) { state | register = state.register |> Dict.insert register (value - 1) }

                Jump from offset ->
                    case from of
                        Value v ->
                            case v of
                                0 ->
                                    recurse (index + 1) state

                                _ ->
                                    recurse (index + offset) state

                        RegisterI register ->
                            case Dict.get register state.register of
                                Nothing ->
                                    recurse (index + 1) state

                                Just 0 ->
                                    recurse (index + 1) state

                                Just x ->
                                    recurse (index + offset) state


res1 =
    input
        |> String.lines
        |> List.map (Parser.run commandParser >> H.uR)
        |> Debug.log "parsed"
        |> prepareState
        |> recurse 0
        |> Debug.log "state"


inpute =
    """cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a"""


input =
    """cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 16 c
cpy 17 d
inc a
dec d
jnz d -2
dec c
jnz c -5"""
