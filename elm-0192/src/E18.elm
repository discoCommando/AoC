module E18 exposing (..)

import Array exposing (Array)
import Board exposing (..)
import Dict exposing (Dict)
import Helpers
import Html
import Intcode
import Parser exposing (..)
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
    { blockers : Set Char, path : Path }


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
            ( ( { blockers, path }, position ), restQueue ) =
                Q.pop queue
        in
        if visited |> isVisited position then
            getBlocking board visited blocking (unsafePositions |> visit position) restQueue

        else
            let
                newPath =
                    position :: path

                ( newBlocking, newBlockers ) =
                    case board |> get position of
                        Empty (Just k) ->
                            Tuple.pair (blocking |> Dict.insert k { blockers = blockers, path = newPath |> List.reverse })
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
                    |> List.foldl (\p -> Q.push ( { blockers = newBlockers, path = newPath }, p )) restQueue
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
    { state1 : State1, accumulator : Accumulator, length : Int }


type RecursionResult
    = InProgress InProgressState
    | Done InProgressState


type alias RecursionAccumulator =
    { shortestSoFar : Maybe InProgressState
    }


startRecursion : State1 -> RecursionAccumulator
startRecursion state1 =
    recursion [ { state1 = state1, accumulator = [], length = 0 } ] { shortestSoFar = Nothing }


recursion : List InProgressState -> RecursionAccumulator -> RecursionAccumulator
recursion states recAcc =
    case states of
        [] ->
            recAcc

        recState :: rest ->
            let
                ( newStates, newRecAcc ) =
                    recState
                        |> stepR
                        |> List.foldl
                            (\r ( newS, newR ) ->
                                case r of
                                    Done acc ->
                                        ( newS
                                        , RecursionAccumulator <|
                                            case newR.shortestSoFar of
                                                Nothing ->
                                                    Just acc |> Debug.log "shortest"

                                                Just acc2 ->
                                                    Just <|
                                                        if acc.length < acc2.length then
                                                            acc |> Debug.log "shortest"

                                                        else
                                                            acc2
                                        )

                                    InProgress ip ->
                                        case newR.shortestSoFar of
                                            Nothing ->
                                                ( ip :: newS, newR )

                                            Just ir ->
                                                if ir.length <= ip.length then
                                                    ( newS, newR )

                                                else
                                                    ( ip :: newS, newR )
                            )
                            ( rest, recAcc )
            in
            recursion newStates newRecAcc


stepR : InProgressState -> List RecursionResult
stepR inProgressState =
    let
        candidates =
            findCandidates inProgressState.state1
    in
    candidates
        |> List.map (step inProgressState)


step : InProgressState -> ( Char, KeyDoorState ) -> RecursionResult
step { state1, accumulator, length } ( key, keyDoorState ) =
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
                                    | path = movePath state1.unsafePositions keyDoorState.path kd.path
                                    , blockers = kd.blockers |> Set.remove (Char.toUpper key) |> Set.remove key
                                }
                            )
            }

        newAccumulator : Accumulator
        newAccumulator =
            accumulate key keyDoorState.path accumulator

        newLength =
            length + (List.length keyDoorState.path - 1)

        newInProgressState =
            { state1 = newState1, accumulator = newAccumulator, length = newLength }
    in
    if newState1.blocking |> Dict.isEmpty then
        Done newInProgressState

    else
        InProgress newInProgressState


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
                    Helpers.crash "from longer than other" ( from, [] )

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



--fixUnsafesHelper : Q.Queue Path -> Position -> Visited -> Board -> Path
--fixUnsafesHelper queue end board =
--    if queue |> Q.isEmpty then
--        Helpers.crash "empty queue for fix unsafes" end
--    else
--        let
--            (v, newq) = Q.pop queue
--        in


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



--shortestPath : RecursionAccumulator -> ( Accumulator, Int )
--shortestPath recursionAccumulator =
--    recursionAccumulator
--        |> List.map (\acc -> acc |> List.map (Tuple.second >> List.length >> (-) 1 >> abs) |> List.sum |> Tuple.pair (List.reverse acc))
--        |> List.sortBy Tuple.second
--        |> Helpers.at 0


result1 : String -> Helpers.Main -> Helpers.Main
result1 s =
    let
        board =
            init s

        entrance =
            findEntrance s

        ( blocking, unsafePositions ) =
            getBlocking board Set.empty Dict.empty Set.empty (Q.singleton ( { blockers = Set.empty, path = [] }, entrance ))

        state =
            State1 entrance blocking board unsafePositions

        results =
            startRecursion state

        --        shortest =
        --            shortestPath results
    in
    identity
        >> Helpers.addMultiline (addGrid s)
        >> Helpers.addToMain state
        >> Helpers.addToMain results



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
#a....#
##.@.##
#c....#
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
