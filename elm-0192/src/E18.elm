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
    { blockers : Set Char
    , pathToEntrance : Path
    , point : Position
    , level : Int
    }


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
                            Tuple.pair
                                (blocking
                                    |> Dict.insert k
                                        { blockers = blockers
                                        , pathToEntrance = newPath |> List.reverse
                                        , point = Helpers.at 0 newPath
                                        , level = 0
                                        }
                                )
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
                    |> List.foldl
                        (\p ->
                            Q.push
                                ( { blockers = newBlockers
                                  , pathToEntrance = newPath
                                  , point = Position 0 0
                                  , level = 0
                                  }
                                , p
                                )
                        )
                        restQueue
                )


type alias ShortestPaths =
    Dict ( Char, Char ) Path


getShortestPath : UnsafePositions -> KeyDoors -> ShortestPaths
getShortestPath unsafePositions keyDoors =
    keyDoors
        |> Dict.map
            (\kd kdState ->
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
                            |> Dict.insert '@' (kdState.pathToEntrance |> List.reverse)
                in
                newKd
            )
        |> Dict.foldl (\k v dd -> Dict.foldl (\k2 p dd2 -> dd2 |> Dict.insert ( k, k2 ) p) dd v) Dict.empty


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


findEntrances : String -> List Position
findEntrances =
    Board.init
        (\c ->
            case c of
                '@' ->
                    True

                _ ->
                    False
        )
        >> find ((==) True)
        >> List.map Tuple.first


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


result2 =
    ""


type alias PQCell =
    { keyDoor : List Char, length : Int, previous : Position, keyDoors : List KeyDoors, visited : Set Char, boardNo : Int }


type alias PriorityQueue =
    PQ.PriorityQueue PQCell


type alias State2 =
    { visited : Set ( List Char, List Char )
    , priorityQueue : PriorityQueue
    }


toS : Set Char -> String
toS chars =
    chars |> Set.toList |> String.fromList


type alias SingleState =
    { keyDoors : KeyDoors, shortestPaths : ShortestPaths }


pqinit : List KeyDoors -> State2
pqinit keyDoors =
    { visited = Set.empty
    , priorityQueue =
        keyDoors
            |> List.indexedMap
                (\i _ ->
                    { keyDoor = keyDoors |> List.map (\_ -> '@')
                    , length = 0
                    , previous = Position -1 -1
                    , keyDoors = keyDoors
                    , visited = Set.empty
                    , boardNo = i
                    }
                )
            |> List.foldl (\x -> PQ.push x 0) PQ.empty
    }


recurse : Int -> ShortestPaths -> State2 -> Int
recurse count shortestPaths state2 =
    case PQ.isEmpty state2.priorityQueue of
        Nothing ->
            Debug.todo "recurse"

        Just x ->
            let
                ( cell, qq ) =
                    PQ.pop x

                setKey =
                    ( cell.visited |> Set.toList, cell.keyDoor )
            in
            if List.all Dict.isEmpty cell.keyDoors then
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
                                (\( boardNo, ch ) qqq ->
                                    let
                                        cellKeyDoor =
                                            cell.keyDoor |> Helpers.at boardNo

                                        shortestPath =
                                            shortestPaths
                                                |> Dict.get ( ch, cellKeyDoor )
                                                |> Helpers.uM

                                        weight : Int
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
                                                { keyDoor = Helpers.replaceAt boardNo (\_ -> ch) cell.keyDoor
                                                , length = len
                                                , previous = previous
                                                , keyDoors =
                                                    cell.keyDoors
                                                        |> List.map
                                                            (\kd ->
                                                                kd
                                                                    |> Dict.remove ch
                                                                    |> Dict.remove (Char.toUpper ch)
                                                                    |> Dict.map (\k kds_ -> { kds_ | blockers = kds_.blockers |> Set.remove ch |> Set.remove (Char.toUpper ch) })
                                                            )
                                                , visited = visited
                                                , boardNo = boardNo
                                                }
                                                len
                                                qqq
                                    in
                                    nqq
                                )
                                qq
                in
                recurse (count + 1) shortestPaths { state2 | priorityQueue = newq, visited = state2.visited |> Set.insert setKey }


getNeighbours : ShortestPaths -> PQCell -> List ( Int, Char )
getNeighbours shortestPaths cell =
    let
        withNoBlocking =
            cell.keyDoors
                |> List.map
                    (Dict.filter
                        (\k kds ->
                            Char.isLower k
                                && Set.isEmpty kds.blockers
                        )
                    )

        withoutGoingBack =
            withNoBlocking
                |> List.indexedMap
                    (\i ->
                        Dict.filter
                            (\k kds ->
                                let
                                    shortestPath =
                                        shortestPaths
                                            |> Dict.get ( k, Helpers.at i cell.keyDoor )
                                            |> Helpers.uM

                                    newPrevious =
                                        shortestPath
                                            |> Helpers.at 1
                                in
                                newPrevious /= cell.previous
                            )
                    )

        result =
            if List.all Dict.isEmpty withoutGoingBack then
                withNoBlocking

            else
                withoutGoingBack
    in
    result |> List.indexedMap (\i x -> Dict.keys x |> List.map (Tuple.pair i)) |> List.concat


result1 : String -> Helpers.Main -> Helpers.Main
result1 s =
    let
        board =
            init s

        entrances =
            findEntrances s

        ( keyDoors, shortestPaths ) =
            entrances
                |> List.map
                    (\entrance ->
                        getBlocking
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
                    )
                |> List.map
                    (\( keyDoors_, unsafePositions_ ) ->
                        { keyDoors = keyDoors_, shortestPaths = getShortestPath unsafePositions_ keyDoors_ }
                    )
                |> List.foldl (\ss ( kds, acc ) -> ( kds ++ [ ss.keyDoors ], acc |> Dict.union ss.shortestPaths )) ( [], Dict.empty )

        shortest =
            pqinit keyDoors |> recurse 0 shortestPaths
    in
    identity
        --        >> Helpers.addMultiline (addGrid s)
        >> Helpers.addToMain shortest


main =
    Helpers.initMain
        --        |> result1 (test Test81 identity)
        |> result1 (test Main identity)
        |> result1 (test Main2 identity)
        --        |> Helpers.addToMain result2
        |> Helpers.makeMain2


type Test
    = Main
    | Simple
    | Normal
    | Trivial
    | Trivial2
    | Test1
    | Test81
    | Main2


test : Test -> (String -> a) -> a
test t f =
    f <|
        case t of
            Main ->
                input

            Main2 ->
                input2

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

            Trivial2 ->
                """###########
#b.A.@.a.c#
###########"""

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


input2 =
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
#...#.....#...#...............#........@#@..................#....b..............#
#################################################################################
#...#.#.........#.....#...........#....@#@....#...#.....#.......#...#.....Q.#...#
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
