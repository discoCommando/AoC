module E18 exposing (..)

import Array exposing (Array)
import Board exposing (..)
import Dict exposing (Dict)
import Helpers
import Html
import Intcode
import Parser exposing (..)
import PriorityQueue as PQ
import Queue as Q
import Set exposing (Set)


isKey : Char -> Bool
isKey =
    Char.isLower


isDoor : Char -> Bool
isDoor =
    Char.isUpper


type alias Path =
    List Position


type alias KeyDoorState =
    { blockers : Set Char, pathToEntrance : Path, shortestPath : Array Path }


type alias KeyDoors =
    Dict Char KeyDoorState


type alias UnsafePositions =
    Visited


getBlocking : Board -> Visited -> KeyDoors -> UnsafePositions -> Q.Queue ( KeyDoorState, Position ) -> ( KeyDoors, UnsafePositions )
getBlocking board visited blocking unsafePositions queue =
    if Q.isEmpty queue then
        Tuple.pair
            blocking
            unsafePositions

    else
        let
            ( ( { blockers, pathToEntrance }, position ), restQueue ) =
                Q.pop queue
        in
        if visited |> isVisited position then
            getBlocking board visited blocking (unsafePositions |> visit position) restQueue

        else
            let
                newPath =
                    position :: pathToEntrance

                ( newBlocking, newBlockers ) =
                    case board |> get position of
                        Empty (Just k) ->
                            Tuple.pair (blocking |> Dict.insert k { blockers = blockers, pathToEntrance = newPath |> List.reverse, shortestPath = Array.initialize 200 (\_ -> []) })
                                (Set.insert k blockers)

                        _ ->
                            ( blocking, blockers )

                newVisited =
                    visited |> visit position

                possiblePositions =
                    getPossiblePositions position (unsafeIf ((==) Wall)) board
                        |> List.filter (\p -> isVisited p visited |> not)
            in
            getBlocking board
                newVisited
                newBlocking
                unsafePositions
                (possiblePositions
                    |> List.foldl (\p -> Q.push ( { blockers = newBlockers, pathToEntrance = newPath, shortestPath = Array.empty }, p )) restQueue
                )


shortestPaths : UnsafePositions -> KeyDoors -> KeyDoors
shortestPaths unsafePositions keyDoors =
    keyDoors
        |> Dict.map
            (\kd kdState ->
                { kdState
                    | shortestPath =
                        let
                            newKd =
                                keyDoors
                                    |> Dict.map
                                        (\kd2 kdState2 ->
                                            if kd == kd2 then
                                                []

                                            else
                                                movePath unsafePositions kdState.pathToEntrance kdState2.pathToEntrance
                                                    |> List.reverse
                                        )
                                    |> Dict.insert '@' kdState.pathToEntrance
                        in
                        newKd
                            |> Dict.foldl
                                (\kd2 path arr ->
                                    Array.set (Char.toCode kd2) path arr
                                )
                                kdState.shortestPath
                }
            )


type alias State1 =
    { entrance : Position
    , blocking : KeyDoors
    , board : Board
    , unsafePositions : UnsafePositions
    }


type Cell
    = Empty (Maybe Char)
    | Wall


type alias Board =
    ABoard Cell



{-
   1. get dict from key/door to which door is blocking it
   2. recursion
       if all doors opened
           return something
       if not
           get all available doors
               try out recursively

   problems:
       somehow we need to have the way to get all available doors
           dict door/key (blocking doors)
       in each cycle of the function u have to be able to get the path to each step quickly (o(1))
-}


init : String -> Board
init =
    Board.init
        (\c ->
            case c of
                '#' ->
                    Wall

                '@' ->
                    Empty Nothing

                '.' ->
                    Empty Nothing

                x ->
                    Empty (Just x)
        )


findEntrance : String -> Position
findEntrance =
    Board.init
        (\c ->
            case c of
                '@' ->
                    True

                _ ->
                    False
        )
        >> find ((==) True)
        >> Helpers.at 0
        >> Tuple.first



-- add unsafe points
-- move normally
-- fix unsafe points
-- RECURSION


type alias Accumulator =
    List ( Char, Path )


accumulate : Char -> Path -> Accumulator -> Accumulator
accumulate key path acc =
    ( key, path ) :: acc


findCandidates : State1 -> List ( Char, KeyDoorState )
findCandidates state1 =
    state1.blocking
        |> Dict.filter
            (\key v ->
                isKey key && Set.isEmpty v.blockers
            )
        |> Dict.toList


type alias InProgressState =
    { from : Char, state1 : State1, accumulator : Accumulator, length : Int, done : Bool }


type RecursionResult inProgress done
    = InProgress inProgress
    | Done done


type alias RecursionAccumulator =
    { shortestSoFar : Maybe InProgressState
    }


startRecursion : State1 -> InProgressState
startRecursion state1 =
    recursion 0 (PQ.singleton { from = '@', state1 = state1, accumulator = [], length = 0, done = False } 0)



--3856


recursion : Int -> PQ.PriorityQueue InProgressState -> InProgressState
recursion count pq =
    case PQ.isEmpty pq of
        Nothing ->
            Helpers.crash "recursion" ""

        Just res ->
            let
                ( recState, newq ) =
                    PQ.pop res

                _ =
                    Helpers.logIf "count" count (count |> modBy 1000 |> (==) 0)
            in
            if recState.done then
                recState

            else
                let
                    foldRes =
                        recState
                            |> stepR
                            |> List.foldl
                                (\x -> PQ.push x x.length)
                                --                                (\r npq ->
                                --                                    case r of
                                --                                        Done acc ->
                                --                                            --                                                ( newS
                                --                                            --                                                , RecursionAccumulator <|
                                --                                            --                                                    case newR.shortestSoFar of
                                --                                            --                                                        Nothing ->
                                --                                            --                                                            Just acc |> Debug.log "shortest"
                                --                                            --
                                --                                            --                                                        Just acc2 ->
                                --                                            --                                                            Just <|
                                --                                            --                                                                if acc.length < acc2.length then
                                --                                            --                                                                    Debug.log "shortest" acc.length |> (\_ -> acc)
                                --                                            --
                                --                                            --                                                                else
                                --                                            --                                                                    acc2
                                --                                            --                                                )
                                --                                            npq |> PQ.push acc
                                --
                                --                                        InProgress ip ->
                                --                                            npq |> PQ.push ip
                                --                                 --                                                case newR.shortestSoFar of
                                --                                 --                                                    Nothing ->
                                --                                 --                                                        ( ip :: newS, newR )
                                --                                 --
                                --                                 --                                                    Just ir ->
                                --                                 --                                                        if ir.length <= ip.length then
                                --                                 --                                                            ( newS, newR )
                                --                                 --
                                --                                 --                                                        else
                                --                                 --                                                            ( ip :: newS, newR )
                                --                                )
                                newq
                in
                recursion (count + 1) foldRes


stepR : InProgressState -> List InProgressState
stepR inProgressState =
    let
        candidates =
            findCandidates inProgressState.state1
    in
    candidates
        |> List.map (step inProgressState)


step : InProgressState -> ( Char, KeyDoorState ) -> InProgressState
step { state1, accumulator, length, from } ( key, keyDoorState ) =
    let
        newState1 : State1
        newState1 =
            { state1
                | blocking =
                    state1.blocking
                        |> Dict.remove key
                        |> Dict.remove (Char.toUpper key)
                        |> Dict.map
                            (\_ kd ->
                                { kd
                                    | blockers = kd.blockers |> Set.remove (Char.toUpper key) |> Set.remove key
                                }
                            )
            }

        path =
            keyDoorState.shortestPath |> Helpers.uG (Char.toCode from)

        newAccumulator : Accumulator
        newAccumulator =
            accumulate key path accumulator

        newLength =
            length + (List.length path - 1)

        newDone =
            newState1.blocking |> Dict.isEmpty

        newInProgressState =
            { state1 = newState1, accumulator = newAccumulator, length = newLength, from = key, done = newDone }
    in
    newInProgressState


movePath : UnsafePositions -> Path -> Path -> Path
movePath unsafePositions from other =
    let
        p1 =
            case movePathHelper from other of
                IP path ->
                    path

                F _ path ->
                    path
    in
    case findUnsafePath unsafePositions p1 of
        Safe ->
            p1

        Unsafe res ->
            case fixUnsafes res of
                Nothing ->
                    p1

                Just p2 ->
                    p2


type MovePathHelperResult
    = IP Path
    | F Path Path


movePathHelper : Path -> Path -> MovePathHelperResult
movePathHelper from other =
    case from of
        [] ->
            F [] other

        fromFirst :: fromRest ->
            case other of
                [] ->
                    F from []

                --                    Helpers.crash "from longer than other" ( from, [] )
                otherFirst :: otherRest ->
                    if fromFirst == otherFirst then
                        case movePathHelper fromRest otherRest of
                            F from_ other_ ->
                                IP
                                    (List.reverse (fromFirst :: from_)
                                        ++ other_
                                    )

                            IP res ->
                                IP res

                    else
                        F from other


type alias UnsafePath =
    { before : Path, first : Position, last : Position, rest : Path }


type UnsafePathsResult
    = Safe
    | Unsafe UnsafePath


findUnsafePath : UnsafePositions -> Path -> UnsafePathsResult
findUnsafePath unsafes path =
    case path of
        [] ->
            Safe

        first :: rest ->
            if isVisited first unsafes then
                case findUnsafePathHelper unsafes rest of
                    Just ( last, rest_ ) ->
                        Unsafe { before = [], first = first, last = last, rest = rest_ }

                    Nothing ->
                        let
                            result =
                                findUnsafePath unsafes rest
                        in
                        case result of
                            Safe ->
                                Safe

                            Unsafe r ->
                                Unsafe { r | before = first :: r.before }

            else
                let
                    result =
                        findUnsafePath unsafes rest
                in
                case result of
                    Safe ->
                        Safe

                    Unsafe r ->
                        Unsafe { r | before = first :: r.before }


findUnsafePathHelper : UnsafePositions -> Path -> Maybe ( Position, Path )
findUnsafePathHelper unsafes path =
    case path of
        [] ->
            Nothing

        first :: rest ->
            if isVisited first unsafes then
                Just <| Tuple.pair first rest

            else
                findUnsafePathHelper unsafes rest


fixUnsafes : UnsafePath -> Maybe Path
fixUnsafes { first, last, before, rest } =
    if first.x == last.x || first.y == last.y then
        Just <|
            before
                ++ [ first, { x = (first.x + last.x) // 2, y = (first.y + last.y) // 2 }, last ]
                ++ rest

    else
        Nothing


addGrid : String -> String
addGrid s =
    let
        len =
            String.lines s |> Helpers.at 0 |> String.length

        firstRow =
            List.range 0 (len - 1)
                |> List.map String.fromInt
                |> String.join ""
                |> (++) "#"
    in
    s
        |> String.lines
        |> List.indexedMap (\i l -> Debug.toString i ++ l)
        |> (::) firstRow
        |> String.join "\n"



-- RESULTS


result1 : String -> Helpers.Main -> Helpers.Main
result1 s =
    let
        board =
            init s

        entrance =
            findEntrance s

        ( blocking, unsafePositions ) =
            getBlocking
                board
                Set.empty
                Dict.empty
                Set.empty
                (Q.singleton ( { blockers = Set.empty, pathToEntrance = [], shortestPath = Array.empty }, entrance ))

        newBlocking =
            shortestPaths unsafePositions blocking

        state =
            State1 entrance newBlocking board unsafePositions

        results =
            startRecursion state

        --        shortest =
        --            shortestPath results
    in
    identity
        >> Helpers.addMultiline (addGrid s)
        --        >> Helpers.addToMain state
        >> Helpers.addToMain results.length



--        >> Helpers.addToMain shortest


result2 =
    ""


main =
    Helpers.initMain
        |> result1 (test Main identity)
        |> Helpers.addToMain result2
        |> Helpers.makeMain2


type Test
    = Main
    | Simple
    | Normal
    | Trivial
    | Test1
    | Test81


test : Test -> (String -> a) -> a
test t f =
    f <|
        case t of
            Main ->
                input

            Normal ->
                """########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"""

            Simple ->
                """########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################"""

            Trivial ->
                """#########
#b.A.@.a#
#########"""

            Test1 ->
                """#######
#a...b#
##.@.##
#c...d#
#######"""

            Test81 ->
                """########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"""


input =
    """#################################################################################
#.......#...#...#...........#.....#...#.#.....#m......#.......#.....#........u..#
#####.#.#.#.#.#.###.#######.###.#.#.#.#.#.###.###.#####.#.###.###.#.###########.#
#.....#...#...#.....#.#.....#...#...#...#.#....e#.......#...#.....#.....#.......#
#.###################.#.#####.#########.#.#####.###########.###########.#.#######
#.#...#...#...........#.#.....#.......#.#.....#.....#.....#...#.......#.#.#.....#
#.#S#.###.#.#####.#####.#.###.#.###.###.#####.#####.#.###.###.#.#.#####.#.#.###.#
#.#.#...#.#...#.#.......#.#.#.#.#...#...#.D.#.#...#...#.#.#...#.#.#.....#j..#...#
#.#.###.#.###.#.#########.#.#.###.###.###.#.#.#.#.#####.#.#.###.###.#########.###
#.#...#.#...#...#.#.........#...#.#...#.#.#...#.#.....#...#...#...#.#.#.....#...#
#.###.#.###.###.#.#.#########.#.#.#.###.#.#####.#####.#.#####.###.#.#.#.#.#.###.#
#...#.#c..#.....#.......#...#.#.#.#.#...#...#.....#.#.#.#.....#...#.#...#.#...#.#
#.###.###.#.#############.#.###.#.#.#.#.###.#####.#.#.#.#.#####.###.#.###.###.#.#
#.#...#.#.#.........#.....#...#.#.....#.#.#...#.#...#.#...#...#.....#.#...#...K.#
#.#.###.#.#########.#.#######.#.#########.###.#.###.#.#####.#.#.#######.#########
#.......#.#.......#h..#.....#.#.........#...#.#.....#.......#...#.....#.#...#...#
#.#######.#O#####.#######.#.#.#.#######.###.#.#.#######.#######.#.###F#.#.#.#.#.#
#.#....y..#...#...#.....#.#.#.#.#.#.....#...#.#.#.....#.#.....#...#.#.#...#.#.#.#
###.#####.###.#.###.###.#.###.#.#.#.###.#.#.#.###.###.#.#.###.#####.#.#.###.#.#.#
#...#.....#.#.#...#.#.Z.#...#.#.#.#.#...#.#.#.....#.#.#.#.#.#...#.....#...#...#.#
#.###.#####.#.###A#.#.###.#.#.#.#.#.#####.#########.#.#.#.#.###.#.#########.###.#
#.#.#.#.....#.#.#...#.#...#...#...#.....#.#.........#.#.#.#.#...#.#.......#.#...#
#.#.#.#####.#.#.#######.#########.#####.#.#.#####.#.#.#.#.#.#####.#.###.#.###.#.#
#.#.#...#...#.#...#.....#.....#.......#.#.#.#.#...#.#.#.#.#.....#.#.#...#.....#.#
#.#.###.#.#.#.###.#.#######.#.#####.###.#.#.#.#.#####.#.#.#.#.###.###.#########X#
#.#...#...#.#...#.#...#.....#.....#.#...#.....#.#...#.#.#.#.#...#...#.....#.....#
#.#W#.#####.###N#.###.#.#.#######.###.#.#######.#.#.#.###.#.###.###.#####.###.###
#i#.#.....#.#.#.#...#.#.#.#.....#.....#.#..r..#...#.#...#.#.#.....#.#...#...#.#.#
#.#######.#.#.#.#.###.###.#.###########.#.###.#.###.###.#.###.#####.#.#.###.#.#.#
#.......#...#.#.#...#...#.#.............#.#.#.#.#.#...#...#...#.....#.#.....#.#.#
#.#####.#.###.#.#.#.###.#.#.#############.#.#.#.#.###.#####.###.#####.#######.#.#
#.#.#...#.....#.#.#...#..t#...#.#.......#.#.#.#...#.....#.....#...#...#.....#.#.#
#.#.#.#######.#.#.###.#######.#.#.#.#####.#.#.#.###.###.#####.###.###.#.###.#.#.#
#...#.#...#...#.#...#.....#.#.#...#.....#.#.#.#.#...#.........#.#...#...T.#.#...#
###.#.#.#.#.###.###.#.###.#.#.#########.#.#.#.###.###########.#.###.#####.#.###.#
#...#...#.#.#...#...#...#...#.......#...#.#.#...#.#.....#...#...#...#...#.#...#.#
#.#######.###.###.#####.###########.#.###.#.###.#.#.###.#.#.###.#.###.#.#####.###
#.#...#v#...#.#.#.....#.........#...#...#.#.......#...#..l#...#.#.....#.....#.P.#
#.#.#.#.###.#.#.#####.#########.#.#####.#.###########.#######.#############.###.#
#...#.....#...#...............#.............................#....b..............#
#######################################.@.#######################################
#...#.#.........#.....#...........#...........#...#.....#.......#...#.....Q.#...#
#.#.#.#.#####.###.#.#.#.#####.###.#.###.#.###.#.#.#.###.#.#####.#.#.#.#.###.###.#
#.#.#.#.#...#.....#.#.#.#.#...#...#...#.#.#.....#.#...#...#.....#.#.#.#...#.#...#
#.#.#.#.#.#.#######.###.#.#.#####.###.#.#.#######.###.#####.#####.#.#.###.#.#.#.#
#.#...#.#.#.....#.#.#.B...#.....#.#...#.#.....#.....#.#...#.......#.#.#...#...#.#
#.#####.#.#####.#.#.#.#######.#.###.###.#.###.###.###.#.#####.#####.#.#.#######.#
#.#...#q#..f#.....#...#.....#.#...#.#...#.#.#...#.#.....#...#.#...#...#.#.....#.#
#.#.#.#.#.#.#######.###.###.###.#.#.#.###.#.###.###.#####.#.#.###.#####.#.###.#.#
#...#.#.#.#...#...#p..#.#.#...#.#...#...#.....#...#.#.....#.#.....#...#...#...#.#
#####.#.#####.#.#.###.#.#.###.#########.#####.###.#.#.#####.#####.###.#####.###.#
#.....#.......#.#.#...#.#...#.#.......#.#...#.#.#...#.#...#.....#.....#.#...#.#.#
#.###########.#.#.#####.###.#L#.#####R#.#.#.#.#.#####.#.#.#####.#####.#.#.###.#.#
#...........#.#.#.......#...#...#...#...#.#.#.....#...#.#.....#...#...#.#...#...#
#######.###.#.#.#########.#.#####.#.#######.#####.#.#######.#.###.#.###.###.###.#
#.......#...#.#.#.........#.......#.....#...#.....#.#.....#.#...#.#...#...#...#.#
#.#######.###.#.#.###.#################.#.###.#####.#.#.#.###.###.###.###.###.#.#
#.#.........#.#.#...#.............#.....#.........#.#.#.#.....#...#.#.......#.#.#
#.###.#######.#.###########.#####.#.###############.###.#####.#.###.#.#####.#.###
#...#.#.......#.#.........#.#...#.#.#...#.......#...#...#.#...#.#.....#...#.#...#
#.#G###.#######.#.#######.#.###.#.###.#.#.#####.#.###.###.#.###.#######.#.#####.#
#.#...#....x#.#.#.#z#.....#.....#.....#.#.....#...#...#...#.#...#.......#.#...#.#
#.###.#####.#.#.#.#.#########.#########.#####.#######.#.###.#####.#######.#.#.#.#
#k#.#.....#...#...#.#.....#...#.......#.#...#.#.......#.#...#.....#.#...#...#.#.#
#.#.#####.###.#####.#.###.#.###.#####.#.#.#.#.#.#.#####.#.###.#####.#.#.#####.#H#
#.#.....#...#.....#.#...#.#.#...#...#...#.#...#.#.#.....#.#...#...#...#...#.#.#.#
#.#.###.###.#####.#.###.#.#.#.###.#.###.#.#######.#.###.#.#.###.#.#.###.#Y#.#.#.#
#...#...#.#...#...#.....#..o#.#.#.#.....#.#.......#.#...#...#...#...#...#...#..g#
#####.###.#.###.#######.#####.#.#.#######.#####.###.###.#############.#####.###.#
#...#.#...#.....#.....#.....#.#.#...#...#.......#.....#...#...........#.#...#...#
#.#.#.###.#######.###.#######.#.###.#.###########.###.###.#.###########.#.###.###
#.#.#.#.....#.#.....#.#.......#...#.#...#...#...#...#.#...#.......#...#.I.#.#...#
#.#.#.#.###.#.#.###.#.#.#######.#.#.###.#.###.#.#.###.#.#########.###.#.###.###.#
#.#.#.#.#...#s....#.#.#.......#a#.#.....#.#...#.#.#...#.....#.....#...#.....#.#.#
#.###M#U###.#####.#.#.###.###.###.#####.#.#.###.#.#.#.#####.#.#####.#V#####.#.#.#
#.....#...#...#...#.#...#...#.#...#....n#.#...#d..#.#.#.J.#...#.....#...#..w#.#.#
#.#######.###.#####.###.#####.#.###.#####.###.#####.#.#.#.###.#####.#####.###.#.#
#.......#.#.#...#...#.#.....#.#...#.#...#...#.#.#...#.#.#.#...#...#...........#.#
#######.#.#.###E#.###.#####.#.#.#.#.###.#.###.#.#.#####.#.#####.#.#############.#
#.........#.......#.........C.#.#.......#.......#.......#.......#...............#
#################################################################################"""
