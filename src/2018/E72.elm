module E72 exposing (..)

import Char
import Dict exposing (Dict)
import Helpers as H
import List
import List.Extra
import Parser exposing ((|.), (|=), keep, keyword, succeed, symbol)
import Regex
import Set exposing (Set)
import String


main =
    H.makeMain []


type Step
    = Step Char Char


stepParser =
    succeed Step
        |. symbol "Step "
        |= (keep (Parser.Exactly 1) Char.isUpper |> Parser.andThen (String.toList >> H.at 0 >> succeed))
        |. symbol " must be finished before step "
        |= (keep (Parser.Exactly 1) Char.isUpper |> Parser.andThen (String.toList >> H.at 0 >> succeed))


type alias State =
    { pointsToGo : Dict Char (Set Char) -- Step J K => J -> [K]
    , pointsLeft : Dict Char (Set Char) -- Step J K => K -> [J]
    }


getFreePoints : State -> List Char
getFreePoints { pointsToGo, pointsLeft } =
    let
        _ =
            Debug.log "pointsToGo" pointsToGo

        _ =
            Debug.log "pointsLeft" pointsLeft
    in
    [ pointsToGo
        |> Dict.toList
        |> List.filter
            (\( key, _ ) ->
                case pointsLeft |> Dict.get key of
                    Nothing ->
                        -- no points are pointing at the key
                        True

                    Just set ->
                        set |> Set.isEmpty
            )
        |> List.map H.tf
    , pointsLeft
        |> Dict.filter (\_ -> Set.isEmpty)
        |> Dict.toList
        |> List.map H.tf
    ]
        |> List.concat
        |> List.Extra.unique
        |> List.sort


step : Char -> State -> State
step ch state =
    let
        newPointsToGo =
            -- just removing current char from the dictionary
            state.pointsToGo |> Dict.remove ch

        newPointsLeft =
            -- removing ch from all keys
            state.pointsLeft |> Dict.map (\_ -> Set.remove ch) |> Dict.remove ch
    in
    { state | pointsToGo = newPointsToGo, pointsLeft = newPointsLeft }


recursing : State -> ( List Char, State )
recursing state =
    let
        freePoints =
            getFreePoints state |> Debug.log "freePoints"
    in
    case freePoints of
        [] ->
            ( [], state )

        ch :: _ ->
            let
                _ =
                    Debug.log ([ ch ] |> String.fromList) freePoints

                ( output, newState ) =
                    recursing <| step ch state
            in
            ( ch :: output, newState )


createState : List Step -> State
createState steps =
    { pointsToGo = steps |> List.foldl (\(Step from to) -> H.updD from (Set.singleton to) (Set.insert to)) Dict.empty
    , pointsLeft = steps |> List.foldl (\(Step from to) -> H.updD to (Set.singleton from) (Set.insert from)) Dict.empty
    }


part1 =
    input
        |> String.lines
        |> List.map (Parser.run stepParser >> H.uR)
        |> Debug.log "parsed"
        |> createState
        |> Debug.log "createState"
        |> recursing
        |> Debug.log "recursing"
        |> H.tf
        |> String.fromList
        |> Debug.log "PART 1 ANSWER"


type Worker
    = Worker Char Int


type alias WorkersStepResult =
    { working : List Worker
    , done : List Char
    }


workersStep : List Worker -> WorkersStepResult
workersStep workers =
    workers
        |> List.map (\(Worker ch s) -> Worker ch <| s - 1)
        |> List.partition (\(Worker ch s) -> s == 0)
        |> (\( done, working ) -> { working = working, done = done |> List.map (\(Worker ch s) -> ch) })


numberOfWorkers =
    5


secondsOffset =
    60


getSeconds : Char -> Int
getSeconds ch =
    (ch |> Char.toCode) - ('A' |> Char.toCode) + 1 + secondsOffset


initialWorkers : State -> List Worker
initialWorkers state =
    state
        |> getFreePoints
        |> List.take numberOfWorkers
        |> List.map (\ch -> Worker ch (getSeconds ch))


recursing2 : List Worker -> Int -> State -> Int
recursing2 workers seconds state =
    let
        { working, done } =
            workersStep workers |> Debug.log "workers"
    in
    case done of
        [] ->
            case working of
                [] ->
                    seconds

                _ ->
                    recursing2 working (seconds + 1) state

        _ ->
            let
                newState =
                    List.foldl step state done

                additionalWorkers =
                    getFreePoints newState
                        |> List.filter (\ch -> working |> List.map (\(Worker ch2 _) -> ch2) |> List.member ch |> not)
                        |> List.take (numberOfWorkers - List.length working)
                        |> List.map (\ch -> Worker ch (getSeconds ch))
            in
            recursing2 (working ++ additionalWorkers) (seconds + 1) newState


part2 =
    input
        |> String.lines
        |> List.map (Parser.run stepParser >> H.uR)
        |> Debug.log "parsed"
        |> createState
        |> Debug.log "createState"
        |> (\state -> recursing2 (initialWorkers state) 0 state)
        |> Debug.log "recursing"


inpute =
    """Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin."""


input =
    """Step J must be finished before step K can begin.
Step N must be finished before step X can begin.
Step S must be finished before step G can begin.
Step T must be finished before step R can begin.
Step H must be finished before step L can begin.
Step V must be finished before step W can begin.
Step G must be finished before step U can begin.
Step K must be finished before step A can begin.
Step D must be finished before step Z can begin.
Step C must be finished before step E can begin.
Step X must be finished before step P can begin.
Step Y must be finished before step U can begin.
Step R must be finished before step O can begin.
Step W must be finished before step U can begin.
Step O must be finished before step Q can begin.
Step A must be finished before step P can begin.
Step B must be finished before step E can begin.
Step F must be finished before step E can begin.
Step Q must be finished before step U can begin.
Step M must be finished before step E can begin.
Step P must be finished before step U can begin.
Step L must be finished before step Z can begin.
Step Z must be finished before step U can begin.
Step U must be finished before step E can begin.
Step I must be finished before step E can begin.
Step H must be finished before step G can begin.
Step X must be finished before step I can begin.
Step K must be finished before step X can begin.
Step Z must be finished before step I can begin.
Step S must be finished before step M can begin.
Step L must be finished before step U can begin.
Step A must be finished before step M can begin.
Step W must be finished before step A can begin.
Step N must be finished before step A can begin.
Step S must be finished before step E can begin.
Step W must be finished before step Q can begin.
Step J must be finished before step L can begin.
Step Q must be finished before step L can begin.
Step M must be finished before step U can begin.
Step H must be finished before step E can begin.
Step D must be finished before step E can begin.
Step V must be finished before step P can begin.
Step Q must be finished before step M can begin.
Step X must be finished before step W can begin.
Step K must be finished before step I can begin.
Step T must be finished before step H can begin.
Step Y must be finished before step L can begin.
Step G must be finished before step O can begin.
Step M must be finished before step Z can begin.
Step F must be finished before step Z can begin.
Step Q must be finished before step E can begin.
Step H must be finished before step C can begin.
Step Q must be finished before step P can begin.
Step D must be finished before step U can begin.
Step Z must be finished before step E can begin.
Step O must be finished before step M can begin.
Step L must be finished before step I can begin.
Step J must be finished before step A can begin.
Step Q must be finished before step Z can begin.
Step P must be finished before step I can begin.
Step K must be finished before step O can begin.
Step R must be finished before step E can begin.
Step W must be finished before step F can begin.
Step D must be finished before step Q can begin.
Step R must be finished before step U can begin.
Step W must be finished before step P can begin.
Step S must be finished before step Z can begin.
Step T must be finished before step P can begin.
Step B must be finished before step Q can begin.
Step S must be finished before step T can begin.
Step R must be finished before step A can begin.
Step K must be finished before step R can begin.
Step N must be finished before step G can begin.
Step C must be finished before step W can begin.
Step T must be finished before step A can begin.
Step B must be finished before step Z can begin.
Step C must be finished before step P can begin.
Step D must be finished before step P can begin.
Step B must be finished before step P can begin.
Step F must be finished before step U can begin.
Step V must be finished before step X can begin.
Step K must be finished before step W can begin.
Step Y must be finished before step I can begin.
Step C must be finished before step B can begin.
Step X must be finished before step L can begin.
Step X must be finished before step M can begin.
Step H must be finished before step P can begin.
Step S must be finished before step F can begin.
Step J must be finished before step Y can begin.
Step Y must be finished before step Z can begin.
Step B must be finished before step I can begin.
Step S must be finished before step C can begin.
Step K must be finished before step E can begin.
Step N must be finished before step Q can begin.
Step A must be finished before step Z can begin.
Step J must be finished before step I can begin.
Step Y must be finished before step O can begin.
Step Y must be finished before step F can begin.
Step S must be finished before step U can begin.
Step D must be finished before step W can begin.
Step V must be finished before step D can begin."""
