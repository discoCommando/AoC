module E10 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Parser exposing (..)
import Queue exposing (Queue)
import Set exposing (Set)


main =
    Helpers.makeMain
        [ Debug.toString (result1 input)
        , Debug.toString (result2 input 200)
        , Debug.toString tests
        ]


tests =
    [ ( 2, 3 ), ( 3, 3 ), ( 3, 2 ), ( 2, 1 ), ( 1, 1 ), ( 1, 3 ), ( 1, 2 ), ( 3, 2 ), ( 3, 1 ) ]
        |> order (angle ( 2, 2 ))


tests2 =
    [ ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 0, -1 ), ( -1, -1 ), ( -1, 1 ), ( -1, 0 ), ( 1, -1 ) ] |> order (angle ( 0, 0 ))


type FieldType
    = Asteroid
    | Space
    | Base


type InSight
    = Unknown
    | InSight
    | NotInSight


type alias Cell =
    { inSight : InSight, visited : Bool, type_ : FieldType }


type alias Board =
    Array (Array Cell)


type alias Position =
    ( Int, Int )


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
                        Cell Unknown False <|
                            if c == '#' then
                                Asteroid

                            else
                                Space
                    )
                >> Array.fromList
            )
        |> Array.fromList


get : Position -> Board -> Maybe Cell
get ( x, y ) board =
    board |> Array.get x |> Maybe.andThen (Array.get y)


set : Position -> Cell -> Board -> Board
set ( x, y ) cell board =
    case board |> Array.get x of
        Nothing ->
            Debug.todo "set"

        Just row ->
            Array.set y cell row |> Array.set x |> (|>) board


type alias Vector =
    { mx : Int, my : Int }


nextPosition : Position -> Vector -> Position
nextPosition ( x, y ) v =
    ( x + v.mx, y + v.my )


blockView : Position -> Vector -> State -> State
blockView ( x, y ) vector state =
    case get ( x, y ) state.board of
        Nothing ->
            state

        Just cell ->
            blockView (nextPosition ( x, y ) vector) vector <|
                case cell.type_ of
                    Asteroid ->
                        { cell | inSight = NotInSight }
                            |> set ( x, y )
                            |> (|>) state.board
                            |> (\b -> { state | board = b })

                    _ ->
                        state


nwd : Int -> Int -> Int
nwd a b =
    if b == 0 then
        a

    else
        nwd b (a |> modBy b)


getVector : Position -> Position -> Vector
getVector ( x1, y1 ) ( x2, y2 ) =
    let
        x3 =
            x2 - x1

        y3 =
            y2 - y1

        bcd =
            nwd (abs x3) (abs y3)
    in
    Vector (x3 // bcd) (y3 // bcd)


getNeighbours : Position -> List Position
getNeighbours ( px, py ) =
    [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ) ]
        |> List.map (\( x, y ) -> ( px + x, py + y ))


bfs : State -> State
bfs state =
    if Queue.isEmpty state.queue then
        state

    else
        let
            ( pos, newQueue ) =
                state.queue |> Queue.pop
        in
        bfs <|
            case get pos state.board of
                Nothing ->
                    { state | queue = newQueue }

                Just cell ->
                    if cell.visited then
                        { state | queue = newQueue }

                    else
                        let
                            state_ =
                                { state | queue = List.foldl Queue.push newQueue (getNeighbours pos) }
                        in
                        bfs <|
                            case cell.type_ of
                                Asteroid ->
                                    case cell.inSight of
                                        Unknown ->
                                            let
                                                vector =
                                                    getVector state_.base pos

                                                newState =
                                                    blockView pos vector state_
                                            in
                                            { newState
                                                | board =
                                                    newState.board
                                                        |> set pos { cell | inSight = InSight, visited = True }
                                            }

                                        _ ->
                                            { state_ | board = state_.board |> set pos { cell | visited = True } }

                                Base ->
                                    { state_ | board = state_.board |> set pos { cell | visited = True } }

                                Space ->
                                    { state_ | board = state_.board |> set pos { cell | visited = True } }


results : State -> Int
results state =
    state.board |> Array.toList |> List.map Array.toList |> List.concat |> List.filter (\cell -> cell.inSight == InSight) |> List.length


createState : Position -> Board -> State
createState ( x, y ) board =
    { board = board |> set ( x, y ) (Cell Unknown False Base)
    , base = ( x, y )
    , queue = Queue.singleton ( x, y )
    }


foldOver : Board -> Array (Array State)
foldOver board =
    board
        |> Array.indexedMap
            (\x row ->
                row
                    |> Array.indexedMap
                        (\y cell ->
                            case cell.type_ of
                                Asteroid ->
                                    createState ( x, y ) board |> bfs

                                _ ->
                                    createState ( x, y ) board
                        )
            )


result1 i =
    initialArray i |> foldOver |> Array.indexedMap (\x -> Array.indexedMap (\y s -> ( ( x, y ), results s )) >> Array.toList) |> Array.toList |> List.concat |> List.sortBy Tuple.second |> List.reverse


angle : Position -> Position -> Float
angle ( x1, y1 ) ( x2, y2 ) =
    Tuple.pair (x2 - x1) (y2 - y1)
        |> Tuple.mapBoth toFloat toFloat
        |> toPolar
        |> Tuple.second
        |> (\x -> x * 180 / pi + 90)


order : (a -> Float) -> List a -> List a
order f l =
    l
        |> List.sortWith
            (\a1 a2 ->
                let
                    a11 =
                        f a1

                    a22 =
                        f a2
                in
                if a11 == 0 then
                    LT

                else if a22 == 0 then
                    GT

                else if a11 > 0 && a22 > 0 then
                    if a11 < a22 then
                        LT

                    else
                        GT

                else if a11 < 0 && a22 < 0 then
                    if a11 < a22 then
                        LT

                    else
                        GT

                else if a11 < 0 && a22 > 0 then
                    GT

                else
                    LT
            )


result2 i j =
    let
        ( base, _, state ) =
            initialArray i
                |> foldOver
                |> Array.indexedMap (\y -> Array.indexedMap (\x s -> ( ( x, y ), results s, s )) >> Array.toList)
                |> Array.toList
                |> List.concat
                |> List.sortBy (\( _, x, _ ) -> x)
                |> List.reverse
                |> Helpers.at 0
    in
    state.board
        |> Array.indexedMap (\y -> Array.indexedMap (\x r -> ( ( x, y ), r )) >> Array.toList)
        |> Array.toList
        |> List.concat
        |> List.filter (\( _, c ) -> c.inSight == InSight)
        |> order (Tuple.first >> angle base)
        |> List.indexedMap Tuple.pair


testInput =
    """.#..#
.....
#####
....#
...##"""


testInputBig =
    """.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"""


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
