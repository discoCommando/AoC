module E07 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Intcode exposing (..)
import Parser exposing (..)
import Set exposing (Set)


main =
    Helpers.makeMain [ Debug.toString result1, Debug.toString result2 ]


parsed =
    String.split "," input |> List.map Helpers.toI |> Array.fromList


type alias Configuration =
    List Int


mkConfig : Int -> Int -> Int -> Int -> Int -> Configuration
mkConfig i1 i2 i3 i4 i5 =
    [ i1, i2, i3, i4, i5 ]


configurationLength =
    5


configurationMaxValue =
    4


initialInput =
    0


testConfig : Configuration
testConfig =
    mkConfig 4 3 2 1 0


initialState : State
initialState =
    { array = parsed, input = [], output = [], status = Going 0 }


foldState : Configuration -> State -> State
foldState config state =
    case config of
        [] ->
            case state.output of
                [] ->
                    Debug.todo "foldState"

                [ x ] ->
                    state

                _ ->
                    { state | input = state.output, output = [], status = Going 0 } |> walk

        input1 :: rest ->
            { state | input = input1 :: state.output, output = [], status = Going 0 }
                |> walk
                |> foldState rest


createStates : Maybe Int -> Configuration -> List State
createStates additionalInput configuration =
    case configuration of
        [] ->
            []

        i :: rest ->
            { initialState | input = additionalInput |> Maybe.map List.singleton |> Maybe.withDefault [] |> (\x -> [ i ] ++ x) } :: createStates Nothing rest


cycle : Int -> List State -> Int
cycle index states =
    let
        state =
            Helpers.at index states

        previous =
            Helpers.at ((index - 1) |> modBy configurationLength) states

        current =
            { state | input = state.input ++ previous.output } |> walk

        newStates =
            states
                |> List.indexedMap
                    (\i s ->
                        if i == index then
                            current
                        else if i == ((index - 1) |> modBy configurationLength) then
                            { previous | output = [] }
                        else
                            s
                    )
    in
    case state.status of
        Halted ->
            if index + 1 == List.length states then
                Helpers.at 0 current.output
            else
                cycle (index + 1) newStates

        Going _ ->
            cycle ((index + 1) |> modBy configurationLength) newStates


normalMode =
    [ 0, 1, 2, 3, 4 ]


cycleMode =
    [ 5, 6, 7, 8, 9 ]


generateConfigurations : Int -> List Int -> List Configuration
generateConfigurations l available =
    if l == 0 then
        [ [] ]
    else
        available
            |> List.map
                (\i ->
                    generateConfigurations (l - 1) available
                        |> List.filter (List.filter ((==) i) >> (==) [])
                        >> List.map ((::) i)
                )
            |> List.concat


result1 =
    generateConfigurations configurationLength normalMode
        |> List.map (foldState >> (|>) { initialState | output = [ 0 ] })
        |> List.sortBy (.output >> Helpers.at 0 >> (*) -1)


result2 =
    generateConfigurations configurationLength cycleMode
        |> List.map (Debug.log "cc" >> createStates (Just 0) >> Debug.log "vefore" >> cycle 0 >> Debug.log "after")
        |> List.sortBy ((*) -1)


testInput =
    "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"


testInput2 =
    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"


input =
    """3,8,1001,8,10,8,105,1,0,0,21,34,47,72,93,110,191,272,353,434,99999,3,9,102,3,9,9,1001,9,3,9,4,9,99,3,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,2,9,1002,9,2,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,101,5,9,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,102,4,9,9,1001,9,3,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99"""
