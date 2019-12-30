module E182 exposing (..)

import Array exposing (Array)
import Board exposing (..)
import Dict exposing (Dict)
import E18
import Helpers
import PriorityQueue as PQ
import Queue as Q
import Set exposing (Set)


type alias Blockss =
    Dict Char (Set Char)


blockss : E18.KeyDoors -> Char -> Blockss -> ( Set Char, Blockss )
blockss keydoors key blocks =
    case Dict.get key blocks of
        Just x ->
            ( x, blocks )

        Nothing ->
            let
                firsts =
                    keydoors |> Dict.get key |> Helpers.uM |> .blockers

                firsts2 =
                    keydoors
                        |> Dict.get (Char.toLower key)
                        |> Helpers.uM
                        |> .blockers
                        |> Set.union firsts

                ( newss, newblocks ) =
                    firsts2
                        |> Set.toList
                        |> List.foldl
                            (\ch ( ss, bb ) ->
                                let
                                    ( ss1, bb1 ) =
                                        blockss keydoors ch bb
                                in
                                ( ss1 |> Set.union ss, bb1 )
                            )
                            ( firsts2, blocks )
            in
            ( newss, newblocks |> Dict.insert key newss )


type alias Cell =
    { keyDoor : Char, visited : Set Char, length : Int, trace : String, previous : Position }


type alias PriorityQueue =
    PQ.PriorityQueue Cell


type alias LensState =
    { length : Int, trace : String }


type alias State2 =
    { lens : Dict String LensState
    , priorityQueue : PriorityQueue
    , keyDoors : E18.KeyDoors
    }


toS : Set Char -> String
toS chars =
    chars |> Set.toList |> String.fromList


init : E18.KeyDoors -> State2
init keyDoors =
    { keyDoors = keyDoors
    , lens = Dict.empty |> Dict.insert "@" { length = 0, trace = "@" }
    , priorityQueue = PQ.singleton { keyDoor = '@', visited = Set.empty, length = 0, trace = "", previous = Position -1 -1 } 0
    }


recurse : Set Char -> State2 -> State2
recurse allKeys state2 =
    case PQ.isEmpty state2.priorityQueue of
        Nothing ->
            state2

        Just x ->
            let
                ( cell, qq ) =
                    PQ.pop x
            in
            if cell.visited == allKeys then
                state2

            else
                let
                    neighbours =
                        getNeighbours cell state2.keyDoors
                            |> (\nnn ->
                                    nnn
                               )

                    ( newLens, newq ) =
                        neighbours
                            |> List.foldl
                                (\( ch, kds ) ( lenss, qqq ) ->
                                    let
                                        stringPath =
                                            toS (Set.insert ch cell.visited)

                                        shortestPath =
                                            kds.shortestPath
                                                |> Helpers.uG (Char.toCode cell.keyDoor)

                                        weight =
                                            shortestPath
                                                |> List.length

                                        previous =
                                            Helpers.at 1 shortestPath

                                        len =
                                            cell.length + (weight - 1)

                                        trace =
                                            cell.trace ++ String.fromList [ ch ]

                                        --
                                        --                                        qValue =
                                        --                                            cell.qValue + weight * 4 - 1500
                                        nqq =
                                            PQ.push
                                                { keyDoor = ch
                                                , visited = cell.visited |> Set.insert ch
                                                , length = len
                                                , trace = trace
                                                , qValue = qValue
                                                }
                                                qValue
                                                qqq

                                        add _ =
                                            lenss |> Dict.insert stringPath { length = len, trace = trace }
                                    in
                                    ( case Dict.get stringPath state2.lens of
                                        Just { length } ->
                                            if length >= len then
                                                add ()

                                            else
                                                --                                            ( lenss, qqq )
                                                lenss

                                        Nothing ->
                                            add ()
                                    , nqq
                                    )
                                )
                                ( state2.lens, qq )
                in
                recurse allKeys { state2 | priorityQueue = newq, lens = newLens }


getNeighbours : Cell -> E18.KeyDoors -> List ( Char, E18.KeyDoorState )
getNeighbours cell keyDoors =
    let
        withoutVisited =
            keyDoors
                |> Dict.filter (\k _ -> k |> Char.isLower)
                |> Dict.filter (\k _ -> cell.visited |> Set.member k |> not)
                |> Dict.filter
                    (\k kds ->
                        Set.diff (Set.diff kds.blockers cell.visited)
                            (cell.visited |> Set.map Char.toUpper)
                            --                    |> Debug.log (( cell.visited, cell.keyDoor, k ) |> Debug.toString)
                            |> Set.isEmpty
                    )

        withoutGoingBack =
            withoutVisited

        result =
            withoutGoingBack
    in
    result |> Dict.toList



--setLevels : E18.KeyDoors -> E18.KeyDoors
--setLevels k


result1 : String -> Helpers.Main -> Helpers.Main
result1 s =
    let
        board =
            E18.init s

        entrance =
            E18.findEntrance s

        ( blocking, unsafePositions ) =
            E18.getBlocking
                board
                Set.empty
                Dict.empty
                Set.empty
                (Q.singleton
                    ( { blockers = Set.empty
                      , pathToEntrance = []
                      , shortestPath = Array.empty
                      , point = Position 0 0
                      , level = 0
                      }
                    , entrance
                    )
                )

        newBlocking =
            E18.shortestPaths unsafePositions blocking

        blocks =
            newBlocking
                |> Dict.keys
                |> List.foldl (\ch -> blockss newBlocking ch >> Tuple.second) Dict.empty
                |> Dict.toList
                |> List.sortBy (Tuple.second >> Set.size)

        allKeys =
            blocking |> Dict.keys |> List.map Char.toLower |> Set.fromList

        state2 =
            init newBlocking |> recurse allKeys

        shortest =
            state2.lens |> Dict.get (toS allKeys) |> Helpers.uM |> .length

        --        state =
        --            State1 entrance newBlocking board unsafePositions
        --
        --        results =
        --            startRecursion state
        --        shortest =
        --            shortestPath results
    in
    identity
        >> Helpers.addMultiline (E18.addGrid s)
        --        >> Helpers.addToMain state
        --        >> Helpers.addToMain (newBlocking |> Dict.map (\_ kd -> kd.point))
        --        >> Helpers.addToMain (newBlocking |> Dict.keys)
        --        >> Helpers.addToMain blocks
        >> Helpers.addToMain shortest


main =
    Helpers.initMain
        |> result1 (E18.test E18.Main identity)
        --        |> Helpers.addToMain result2
        |> Helpers.makeMain2
