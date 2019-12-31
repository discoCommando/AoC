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
    { keyDoor : Char, length : Int, previous : Position, keyDoors : E18.KeyDoors, visited : Set Char }


type alias PriorityQueue =
    PQ.PriorityQueue Cell


type alias LensState =
    { length : Int, trace : String }


type alias State2 =
    { visited : Set ( List Char, Char )
    , priorityQueue : PriorityQueue
    , keyDoors : E18.KeyDoors
    }


toS : Set Char -> String
toS chars =
    chars |> Set.toList |> String.fromList


init : E18.KeyDoors -> State2
init keyDoors =
    { keyDoors = keyDoors
    , visited = Set.empty
    , priorityQueue = PQ.singleton { keyDoor = '@', length = 0, previous = Position -1 -1, keyDoors = keyDoors, visited = Set.empty } 0
    }


recurse : Int -> E18.ShortestPaths -> State2 -> Int
recurse count shortestPaths state2 =
    case PQ.isEmpty state2.priorityQueue of
        Nothing ->
            0

        Just x ->
            let
                ( cell, qq ) =
                    PQ.pop x

                setKey =
                    ( cell.visited |> Set.toList, cell.keyDoor )
            in
            if Dict.isEmpty cell.keyDoors then
                let
                    _ =
                        Debug.log "count" count
                in
                cell.length

            else if state2.visited |> Set.member setKey then
                recurse (count + 1) shortestPaths { state2 | priorityQueue = qq }

            else
                let
                    neighbours =
                        getNeighbours shortestPaths cell

                    newq =
                        neighbours
                            |> List.foldl
                                (\( ch, kds ) qqq ->
                                    let
                                        shortestPath =
                                            shortestPaths
                                                |> Dict.get ( ch, cell.keyDoor )
                                                |> Helpers.uM

                                        weight =
                                            shortestPath
                                                |> List.length

                                        previous =
                                            Helpers.at 1 shortestPath

                                        len =
                                            cell.length
                                                + (weight - 1)

                                        --                                                |> Debug.log "len"
                                        visited =
                                            cell.visited |> Set.insert ch

                                        nqq : PriorityQueue
                                        nqq =
                                            PQ.push
                                                { keyDoor = ch
                                                , length = len
                                                , previous = previous
                                                , keyDoors =
                                                    cell.keyDoors
                                                        |> Dict.remove ch
                                                        |> Dict.remove (Char.toUpper ch)
                                                        |> Dict.map (\k kds_ -> { kds_ | blockers = kds_.blockers |> Set.remove ch |> Set.remove (Char.toUpper ch) })
                                                , visited = visited
                                                }
                                                len
                                                qqq
                                    in
                                    nqq
                                )
                                qq
                in
                recurse (count + 1) shortestPaths { state2 | priorityQueue = newq, visited = state2.visited |> Set.insert setKey }


getNeighbours : E18.ShortestPaths -> Cell -> List ( Char, E18.KeyDoorState )
getNeighbours shortestPaths cell =
    let
        withNoBlocking =
            cell.keyDoors
                |> Dict.filter
                    (\k kds ->
                        Char.isLower k
                            && Set.isEmpty kds.blockers
                    )

        withoutGoingBack =
            withNoBlocking
                |> Dict.filter
                    (\k kds ->
                        let
                            shortestPath =
                                shortestPaths
                                    |> Dict.get ( k, cell.keyDoor )
                                    |> Helpers.uM

                            --                                    |> Debug.log (Debug.toString ( "shortest", cell, k ))
                            newPrevious =
                                shortestPath
                                    |> Helpers.at 1

                            --                                    |> Debug.log (Debug.toString ( "newPrevious", cell.previous, k ))
                        in
                        newPrevious /= cell.previous
                    )

        result =
            if Dict.isEmpty withoutGoingBack then
                withNoBlocking

            else
                --                let
                --                    _ =
                --                        Debug.log (Debug.toString cell.keyDoor) { from = withNoBlocking |> Dict.keys, to = withoutGoingBack |> Dict.keys }
                --                in
                withoutGoingBack

        --                    |> Debug.log (Debug.toString cell)
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
                      , point = Position 0 0
                      , level = 0
                      }
                    , entrance
                    )
                )

        shortestPaths =
            E18.shortestPaths unsafePositions blocking

        --        blocks =
        --            newBlocking
        --                |> Dict.keys
        --                |> List.foldl (\ch -> blockss newBlocking ch >> Tuple.second) Dict.empty
        --                |> Dict.toList
        --                |> List.sortBy (Tuple.second >> Set.size)
        state2 =
            init blocking |> recurse 0 shortestPaths

        --        shortest =
        --            state2.lens |> Dict.get (toS allKeys) |> Helpers.uM |> .length
        --        state =
        --            State1 entrance newBlocking board unsafePositions
        --
        --        results =
        --            startRecursion state
        --        shortest =
        --            shortestPath results
    in
    identity
        --        >> Helpers.addMultiline (E18.addGrid s)
        >> Helpers.addToMain state2



--        >> Helpers.addToMain (newBlocking |> Dict.map (\_ kd -> kd.point))
--        >> Helpers.addToMain (newBlocking |> Dict.keys)
--        >> Helpers.addToMain blocks
--        >> Helpers.addToMain shortest


main =
    Helpers.initMain
        --        |> result1 (E18.test E18.Test81 identity)
        |> result1 (E18.test E18.Main identity)
        --        |> Helpers.addToMain result2
        |> Helpers.makeMain2
