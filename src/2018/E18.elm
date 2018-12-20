module E18 exposing (..)

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


type Tile
    = Open
    | Tree
    | Lumberyard


type alias State =
    { board : Array (Array Tile), turn : Int, hashes : Dict String (Array (Array Tile)) }


size =
    50


getAt : ( Int, Int ) -> State -> Maybe Tile
getAt ( y, x ) state =
    state.board |> Array.get y |> Maybe.andThen (Array.get x)


getAdjacent : ( Int, Int ) -> State -> List Tile
getAdjacent ( y, x ) state =
    [ ( y - 1, x )
    , ( y - 1, x - 1 )
    , ( y - 1, x + 1 )
    , ( y, x - 1 )
    , ( y, x + 1 )
    , ( y + 1, x )
    , ( y + 1, x - 1 )
    , ( y + 1, x + 1 )
    ]
        |> List.filterMap (\p -> getAt p state)


next : ( Int, Int ) -> State -> Tile
next ( y, x ) state =
    let
        tile =
            getAt ( y, x ) state |> H.uM

        adjacent =
            getAdjacent ( y, x ) state
    in
    case tile of
        Open ->
            if (adjacent |> List.filter ((==) Tree) |> List.length) >= 3 then
                Tree
            else
                Open

        Tree ->
            if (adjacent |> List.filter ((==) Lumberyard) |> List.length) >= 3 then
                Lumberyard
            else
                Tree

        Lumberyard ->
            if
                (adjacent |> List.filter ((==) Tree) |> List.length)
                    >= 1
                    && (adjacent |> List.filter ((==) Lumberyard) |> List.length)
                    >= 1
            then
                Lumberyard
            else
                Open


parseInput : String -> State
parseInput string =
    { board =
        string
            |> String.lines
            |> List.map
                (\s ->
                    s
                        |> String.toList
                        |> List.map
                            (\c ->
                                case c of
                                    '|' ->
                                        Tree

                                    '#' ->
                                        Lumberyard

                                    '.' ->
                                        Open

                                    _ ->
                                        Debug.crash ""
                            )
                        |> Array.fromList
                )
            |> Array.fromList
    , turn = 0
    , hashes = Dict.empty
    }


step : State -> State
step state =
    let
        newBoard =
            List.range 0 (size - 1)
                |> List.map
                    (\y ->
                        List.range 0 (size - 1) |> List.map (\x -> next ( y, x ) state) |> Array.fromList
                    )
                |> Array.fromList
    in
    { board = newBoard
    , turn = state.turn + 1
    , hashes = state.hashes |> Dict.insert (hash state) newBoard
    }


stepXTimes : Int -> State -> State
stepXTimes int state =
    let
        _ =
            if int % 1000000 == 0 then
                Debug.log "time" int
            else
                int
    in
    if int == 0 then
        state
    else
        case state.hashes |> Dict.get (hash state) of
            Nothing ->
                stepXTimes (int - 1) (step state)

            Just n ->
                let
                    _ =
                        Debug.log "cycle at" ( int, times - int )

                    _ =
                        Debug.log "findCycle" (findCycle state (step state) 1)
                in
                cycle ((int - 1) % 28) { state | board = n }


findCycle : State -> State -> Int -> Int
findCycle state new i =
    if hash state == hash new then
        i
    else
        findCycle state (cycle 1 new) (i + 1)


cycle : Int -> State -> State
cycle int state =
    let
        _ =
            Debug.log (hash state) (int |> toString)
    in
    if int == 0 then
        state
    else
        cycle (int - 1) { state | board = state.hashes |> Dict.get (hash state) |> H.uM }


result : State -> Int
result state =
    let
        x =
            state.board |> Array.toList |> List.map Array.toList |> List.concat
    in
    List.length (List.filter ((==) Tree) x) * List.length (List.filter ((==) Lumberyard) x)


times =
    1000000000


test1 =
    input |> parseInput |> stepXTimes times |> result |> Debug.log "step11"


hash : State -> String
hash state =
    state.board
        |> Array.toList
        |> List.map Array.toList
        |> List.indexedMap
            (\y l ->
                l
                    |> List.indexedMap
                        (\x v ->
                            let
                                val =
                                    case v of
                                        Open ->
                                            '.'

                                        Tree ->
                                            '|'

                                        Lumberyard ->
                                            '#'
                            in
                            val
                        )
                    |> String.fromList
            )
        |> String.join "\n"



-- 1 at 2 ->
-- 1 open
-- 2 tree t
-- 3 lumberyard
-- 5 - open open 11
-- 6 - open tree 12
-- 7 - open lum 13
-- printState : State -> String
-- printState state =
--


inpute =
    """.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|."""


input =
    """|.|.##|....|#|.#...#.|#.#.....|..#..#|.#..|##.##.|
.|....|.#|.#..|#|..#.|.....#....##.||..#......||.#
|.|...|#..|....#..#.......#......#..#|#|..||.|....
..|||#..#.#||.#..#......||.|#...#.#.|..||.#..|.#||
#.##....##.#|........||..#||#.##.#.|||...|#..|||.#
..#||.#......#||..|.#.##..|##..|#...#|...#......|.
|##.#|#...|||####.##....|.....#....#######.......#
.##..#.......|.#.#|##...|..##......#..|.#..#.#|..#
###..###|#..|.#.#..|.#........|.#||.......#.||||..
|.....|.....|##..|..||.......#|#.#.##.#.##..|.....
|#..|#......|##.......##.|#.|.#......#.|#...|.|..|
..|...##..#..|#.|||##...|.||..|||||||||........#..
#......##...|##|.##..|...........#|.#....#..#.....
.|#..#..|..##.#.||..||....#|##...|#..|.#.........#
......|||.#..........#..|....#|.#|||..||...#...#..
|..#.#|....||.|#.#.|.##.|.||.|#.||..||.|..|..|||..
..#........#.........|..|||.......#..#..#..#||..#|
#..#|..#.....#..###..#||..##..........##......#..|
||......|##....|..|.##....||.|.||.#....|.|...#|.|.
.#|.#.#...|#..#.####..||#.|..|##|.....||.#..||.#..
#.#..........#......#....#..|.#....||.#||.|..|#.||
|###............|#....|.........#.....|###|.....|#
#||...|..|.||..#..|.....#.#..||....#.|##...||.##.#
...|......|.|.|...|#...#|.##..||.|.|.#|....#...##|
.|#....#....#.....#...#...||#|.|...#..|..|###..#..
...#...#|.#.|..#.....|..#..|...#|.....|||..#.|..#|
..|....|..||.....|.#.|.#.||.|.|...|||||.||.....|..
.|...|...|......#|.#...#|..#.|...##.#....#...#..|#
..|||##..#...|.####..|...#.#...|..|#.......||..|.|
..#....#|.|....#...|#|.|||||#####....||.#....|...#
|#..|##..#.....||.##....|...||.....|#....#.#..|..#
#..|#.|||#.#..#|......||#.##..#|#.|..#|.|#|.|.....
....#...#.|.......|....#.#....|...#.|..|#.#.....#.
###..#....|#.|.#.||.##|..#...#..#.|#.|.#.##......|
..#..|##.|.|.|....||....##.|||.#...#.|........#...
.|..#.......|#...#||.|...#..#.#........#.|.##|.#|.
..|||#..|.....#.....##......|...|.|#.|..#..||...#.
..|#.#...##...#.#.|...||.||..|||#..#......|..|##..
.|..#||##|...|..|...||.||#..#||.|##...#....|..|..|
|...||....#.|........||.#..|.|.|..##.#.##.#....#|.
.....#.....|.#...|.#..|.#|..|...#|..|....|.|...|.|
|...|....|......|||...##...|..#.#.#...|........###
##.#......#.......||.#....#.#.....|....|..##.|.##.
|.#.#|......#.......|..#.|...###.||......|#.|....|
#|.||.|.....||.#.|..|..##|.|.#...#..|.##.|....#.#.
||..||.|..#....#..#....|#.........#|...#.||#|#|.|.
|.#..|....#.|#|..#.....|##.|......#.#....#|#.#.#.#
##..||.#|...#...|.#|.|..#|#...|.#..|...|....#.#.#|
.#..#......|.#.#|##.|#........###....##..|.......#
.|.#.#.#||.#...#|..#...............#..#...||##...."""
