module E10 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Parser exposing (..)
import Queue exposing (Queue)
import Set exposing (Set)


main =
    Helpers.makeMain [ Debug.toString result1, Debug.toString result2 ]


type FieldType
    = Asteroid
    | Space
    | Base

type InSight = Unknown | InSight | NotInSight

type alias Cell =
    { inSight : InSight, visited: Bool, type_ : FieldType }


type alias Board =
    Array (Array Cell)

type alias Position =
   (Int, Int)

type alias State =
    { queue : Queue Position
    , board : Board
    , base : Position
    }


initialArray : String -> Array (Array Cell)
initialArray i =
    i
        |> String.lines
        |> List.map
            (String.toList
                >> List.map
                    (\c ->
                        Cell Unknown False <| if c == '#' then
                             Asteroid

                        else
                             Space
                    )
                >> Array.fromList
            )
        |> Array.fromList

get : Position -> Board -> Maybe Cell
get (x, y) board =
    board |> Array.get x |> Maybe.andThen (Array.get y)

blockView : Position -> State -> State
blockView asteroidPosition state =



bfs : State -> State
bfs state =
    if Queue.isEmpty state.queue then
        state
    else
        let
            (pos, newQueue) =
                state.queue |> Queue.pop
        in
        bfs <| case get pos state.board of
            Nothing ->
                { state | queue = newQueue}

            Just cell ->
                if cell.visited then
                    { state | queue = newQueue }
                else
                    state





result1 =
    ""


result2 =
    ""


testInput =
    """.#..#
.....
#####
....#
...##"""


input =
    """#.#.###.#.#....#..##.#....
.....#..#..#..#.#..#.....#
.##.##.##.##.##..#...#...#
#.#...#.#####...###.#.#.#.
.#####.###.#.#.####.#####.
#.#.#.##.#.##...####.#.##.
##....###..#.#..#..#..###.
..##....#.#...##.#.#...###
#.....#.#######..##.##.#..
#.###.#..###.#.#..##.....#
##.#.#.##.#......#####..##
#..##.#.##..###.##.###..##
#..#.###...#.#...#..#.##.#
.#..#.#....###.#.#..##.#.#
#.##.#####..###...#.###.##
#...##..#..##.##.#.##..###
#.#.###.###.....####.##..#
######....#.##....###.#..#
..##.#.####.....###..##.#.
#..#..#...#.####..######..
#####.##...#.#....#....#.#
.#####.##.#.#####..##.#...
#..##..##.#.##.##.####..##
.##..####..#..####.#######
#.#..#.##.#.######....##..
.#.##.##.####......#.##.##"""
