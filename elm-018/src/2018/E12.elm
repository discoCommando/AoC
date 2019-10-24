module E12 exposing (..)

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


type alias Plant =
    Bool


type alias View =
    List Plant


nextView : Plant -> View -> View
nextView plant view =
    List.drop 1 view ++ [ plant ]


type alias State =
    { views : Set Int
    , plants : Set Int
    , lowest : Int
    , highest : Int
    }


viewToInt : View -> Int
viewToInt view =
    view
        |> List.indexedMap
            (\i x ->
                if x then
                    2 ^ i
                else
                    0
            )
        |> List.sum


growStep : Int -> Int -> State -> State
growStep view index state =
    case state.views |> Set.member view of
        False ->
            { state | plants = state.plants |> Set.remove index }

        True ->
            { state | plants = state.plants |> Set.insert index }


getPlant : Int -> State -> Plant
getPlant index state =
    if state.plants |> Set.member index then
        True
    else
        False


getNewLowest : Int -> State -> Int
getNewLowest oldLowest state =
    case getPlant oldLowest state of
        False ->
            getNewLowest (oldLowest + 1) state

        True ->
            oldLowest


getNewHighest : Int -> State -> Int
getNewHighest oldHighest state =
    case getPlant oldHighest state of
        False ->
            getNewHighest (oldHighest - 1) state

        True ->
            oldHighest


stepUntil : Int -> Int -> View -> State -> State
stepUntil current highest view state =
    if current - 1 == highest then
        state
    else
        stepUntil (current + 1) highest (nextView (getPlant (current + 3) state) view) (growStep (view |> viewToInt) current state)


step : State -> State
step state =
    List.range (state.lowest - 2) (state.highest + 2)
        |> List.foldl (\index ( state, view ) -> ( growStep (view |> viewToInt) index state, nextView (getPlant (index + 3) state) view )) ( state, [ False, False, False, False, getPlant state.lowest state ] )
        |> (\( newState, _ ) ->
                { newState
                    | lowest = getNewLowest (state.lowest - 2) newState
                    , highest = getNewHighest (state.highest + 2) newState
                }
           )


step2 : State -> State
step2 state =
    stepUntil (state.lowest - 2) (state.highest + 2) [ False, False, False, False, getPlant state.lowest state ] state
        |> (\newState ->
                { newState
                    | lowest = getNewLowest (state.lowest - 2) newState
                    , highest = getNewHighest (state.highest + 2) newState
                }
           )


noOfSteps1 =
    20


noOfSteps2 =
    50000000000


parseInitial : String -> Set Int
parseInitial string =
    string
        |> String.toList
        |> List.foldl
            (\char ( set, index ) ->
                case char of
                    '#' ->
                        ( set |> Set.insert index, index + 1 )

                    _ ->
                        ( set, index + 1 )
            )
            ( Set.empty, 0 )
        |> Tuple.first


parseView : Parser.Parser ( Int, Plant )
parseView =
    succeed (,)
        |= (keep (Parser.Exactly 5) (always True)
                |> Parser.map
                    (String.toList
                        >> List.indexedMap
                            (\i x ->
                                case x of
                                    '#' ->
                                        2 ^ i

                                    _ ->
                                        0
                            )
                        >> List.sum
                    )
           )
        |. keyword " => "
        |= (keep (Parser.Exactly 1) (always True)
                |> Parser.map
                    (String.toList
                        >> H.at 0
                        >> (\char ->
                                case char of
                                    '#' ->
                                        True

                                    _ ->
                                        False
                           )
                    )
           )


parseViews : String -> Set Int
parseViews string =
    string |> String.lines |> List.map (Parser.run parseView >> H.uR) |> List.filter Tuple.second |> List.map Tuple.first |> Set.fromList


initialState =
    { views = parseViews states2
    , plants = parseInitial initial2
    , highest = String.length initial2
    , lowest = 0
    }


test1 =
    initialState
        |> stepXTimes noOfSteps1 Dict.empty
        -- |> Debug.log "result1"
        -- |> .plants
        -- |> Set.toList
        -- |> List.sum
        |> Debug.log "answer1"


setToX : Int -> Set Int -> List Int
setToX lowest set =
    set
        |> Set.toList
        |> List.map (\x -> x - lowest)


stepXTimes : Int -> Dict (List Int) Int -> State -> ( Int, State )
stepXTimes int states state =
    case states |> Dict.get (setToX state.lowest state.plants) of
        Just i ->
            let
                _ =
                    Debug.log ((noOfSteps2 - int) |> toString) state.plants
            in
            ( i - 1, state )

        Nothing ->
            case int of
                0 ->
                    ( 0, state )

                _ ->
                    stepXTimes (int - 1) (states |> Dict.insert (state.plants |> Debug.log (int |> toString) |> setToX state.lowest) int) (step2 state)


test2 =
    initialState
        |> Debug.log "init"
        |> stepXTimes noOfSteps2 Dict.empty
        |> Debug.log "asd"
        |> (\( i, newState ) ->
                newState.plants
                    |> Set.toList
                    |> List.map ((+) i)
                    |> List.sum
           )
        -- |> Debug.log "result1"
        -- |> .plants
        -- |> (==) initialState.plants
        -- |> Set.toList
        -- |> List.sum
        |> Debug.log "answer1"


initial =
    """###.#..#..##.##.###.#.....#.#.###.#.####....#.##..#.#.#..#....##..#.##...#.###.#.#..#..####.#.##.#"""


initial2 =
    """#...#...##..####..##.####.#...#...#.#.#.#......##....#....######.####.##..#..#..##.##..##....#######"""


states =
    """#.... => .
#.##. => #
..#.. => .
#.#.# => .
.#.## => #
...## => #
##... => #
###.. => #
#..## => .
.###. => .
###.# => #
..... => .
#..#. => .
.#.#. => #
##..# => #
.##.. => .
...#. => .
#.### => .
..### => .
####. => .
#.#.. => #
.##.# => #
.#... => #
##.#. => #
....# => .
..#.# => #
#...# => #
..##. => .
.#..# => #
.#### => .
##### => #
##.## => #"""


states2 =
    """.#### => .
...#. => .
.##.. => #
#.##. => .
#..## => .
##### => #
####. => #
.##.# => #
#.### => .
...## => #
.#.## => #
#..#. => #
#.#.. => #
.###. => #
##.## => #
##..# => .
.#... => #
###.# => .
..##. => .
..... => .
###.. => #
..#.# => .
.#..# => #
##... => #
#.... => .
##.#. => .
..#.. => #
....# => .
#...# => .
#.#.# => #
..### => .
.#.#. => #"""


initiale =
    """#..#.#..##......###...###"""


statese =
    """...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"""
