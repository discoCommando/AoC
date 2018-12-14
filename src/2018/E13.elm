module E13 exposing (..)

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


type TrackDirection
    = Intersection
    | UpDown
    | RightLeft
    | LeftDown
    | RightDown


type Move
    = Up
    | Down
    | Right
    | Left


type alias Point =
    ( Int, Int )


type IntersectionCounter
    = First
    | Second
    | Third


type alias CartState =
    { position : Point
    , intersectionCounter : IntersectionCounter
    , direction : Move
    }


type alias State =
    { tracks : Dict Point TrackDirection
    , carts : List CartState
    , crashes : List Point
    }


newCartState : Point -> Move -> CartState
newCartState point direction =
    { position = point, intersectionCounter = First, direction = direction }


parseSingleChar : Point -> Char -> State -> State
parseSingleChar point char state =
    case char of
        '-' ->
            { state | tracks = state.tracks |> Dict.insert point RightLeft }

        '|' ->
            { state | tracks = state.tracks |> Dict.insert point UpDown }

        '\\' ->
            { state | tracks = state.tracks |> Dict.insert point RightDown }

        '/' ->
            { state | tracks = state.tracks |> Dict.insert point LeftDown }

        '>' ->
            { state
                | tracks = state.tracks |> Dict.insert point RightLeft
                , carts = newCartState point Right :: state.carts
            }

        '<' ->
            { state
                | tracks = state.tracks |> Dict.insert point RightLeft
                , carts = newCartState point Left :: state.carts
            }

        '^' ->
            { state
                | tracks = state.tracks |> Dict.insert point UpDown
                , carts = newCartState point Up :: state.carts
            }

        'v' ->
            { state
                | tracks = state.tracks |> Dict.insert point UpDown
                , carts = newCartState point Down :: state.carts
            }

        ' ' ->
            state

        '+' ->
            { state | tracks = state.tracks |> Dict.insert point Intersection }

        _ ->
            Debug.crash "ooops"


parseSingle : Int -> String -> State -> State
parseSingle row s state =
    s
        |> String.toList
        |> List.foldl (\ch ( col, state ) -> ( col + 1, parseSingleChar ( col, row ) ch state )) ( 0, state )
        |> Tuple.second


sortCarts : State -> State
sortCarts state =
    { state | carts = state.carts |> List.sortBy .position }


parseInput : List String -> State -> State
parseInput ls state =
    ls
        |> List.foldl (\s ( row, state ) -> ( row + 1, parseSingle row s state )) ( 0, state )
        |> Tuple.second
        |> sortCarts


addVector : Point -> Point -> Point
addVector vector point =
    ( Tuple.first point + Tuple.first vector, Tuple.second point + Tuple.second vector )


turn : Move -> IntersectionCounter -> Move
turn direction intersectionCounter =
    case intersectionCounter of
        First ->
            -- Left
            case direction of
                Up ->
                    Left

                Left ->
                    Down

                Right ->
                    Up

                Down ->
                    Right

        Second ->
            direction

        Third ->
            -- Right
            case direction of
                Up ->
                    Right

                Left ->
                    Up

                Right ->
                    Down

                Down ->
                    Left


move : State -> CartState -> CartState
move state cartState =
    let
        vector =
            case cartState.direction of
                Up ->
                    ( 0, -1 )

                Down ->
                    ( 0, 1 )

                Left ->
                    ( -1, 0 )

                Right ->
                    ( 1, 0 )

        nextPosition =
            cartState.position |> addVector vector

        nextTrack =
            case state.tracks |> Dict.get nextPosition of
                Just track ->
                    track

                Nothing ->
                    let
                        _ =
                            Debug.log (nextPosition |> toString) cartState

                        _ =
                            Debug.log (nextPosition |> toString) state.tracks
                    in
                    Debug.crash ""

        ( nextInersectionCounter, nextDirection ) =
            case nextTrack of
                Intersection ->
                    case cartState.intersectionCounter of
                        First ->
                            ( Second, turn cartState.direction cartState.intersectionCounter )

                        Second ->
                            ( Third, turn cartState.direction cartState.intersectionCounter )

                        Third ->
                            ( First, turn cartState.direction cartState.intersectionCounter )

                RightLeft ->
                    ( cartState.intersectionCounter, cartState.direction )

                UpDown ->
                    ( cartState.intersectionCounter, cartState.direction )

                RightDown ->
                    case cartState.direction of
                        Right ->
                            ( cartState.intersectionCounter, Down )

                        Up ->
                            ( cartState.intersectionCounter, Left )

                        Down ->
                            ( cartState.intersectionCounter, Right )

                        Left ->
                            ( cartState.intersectionCounter, Up )

                LeftDown ->
                    case cartState.direction of
                        Left ->
                            ( cartState.intersectionCounter, Down )

                        Up ->
                            ( cartState.intersectionCounter, Right )

                        Right ->
                            ( cartState.intersectionCounter, Up )

                        Down ->
                            ( cartState.intersectionCounter, Left )
    in
    { direction = nextDirection, intersectionCounter = nextInersectionCounter, position = nextPosition }


detectCrashes : List CartState -> List Point -> List Point
detectCrashes carts crashes =
    case carts of
        [] ->
            crashes

        cart :: rest ->
            let
                samePosition =
                    rest |> List.filter (.position >> (==) cart.position) |> List.map .position
            in
            detectCrashes rest (crashes ++ samePosition)


tick : State -> State
tick state =
    let
        dictCarts =
            state.carts
                |> List.indexedMap (,)
                |> Dict.fromList

        newCarts =
            List.range 0 (List.length state.carts - 1)
                |> List.foldl
                    (\index dictCarts ->
                        case dictCarts |> Dict.get index of
                            Nothing ->
                                dictCarts

                            Just cart ->
                                let
                                    newCartState =
                                        move state cart

                                    hasCollisions =
                                        dictCarts |> Dict.filter (\_ c -> c.position == newCartState.position) |> Dict.isEmpty |> not

                                    newDictCarts =
                                        if hasCollisions then
                                            dictCarts
                                                |> Dict.remove index
                                                |> Dict.filter (\i c -> c.position /= newCartState.position)
                                        else
                                            dictCarts
                                                |> Dict.insert index newCartState
                                in
                                newDictCarts
                    )
                    dictCarts
                |> Dict.toList
                |> List.map Tuple.second
    in
    { state | carts = newCarts }
        |> sortCarts


firstCrash : Int -> State -> ( Int, List Point )
firstCrash turn state =
    case state.crashes of
        [] ->
            firstCrash (turn + 1) (tick state)

        _ ->
            ( turn, state.crashes )


lastCart : Int -> State -> CartState
lastCart counter state =
    case state.carts of
        [] ->
            Debug.crash "impossible"

        [ last ] ->
            let
                _ =
                    Debug.log "counter" counter
            in
            last

        _ ->
            let
                newState =
                    state |> tick
            in
            lastCart (counter + 1) newState



-- test =
--     input |> String.lines |> (\ls -> parseInput ls (State Dict.empty [] [])) |> Debug.log "initialState" |> firstCrash 0 |> Debug.log "crash"


test2 =
    input |> String.lines |> (\ls -> parseInput ls (State Dict.empty [] [])) |> lastCart 0 |> Debug.log "last cart"


inpute : String
inpute =
    """/->-\\
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/   """


inpute2 : String
inpute2 =
    """/>-<\\
|   |
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/"""


inpute3 : String
inpute3 =
    "->>>-"


input : String
input =
    """                                                                                 /-------------------------------------\\
          /----------------------------------\\                   /---------------+-------------------------------------+--------\\
         /+----------------------------------+-------------------+---------------+----------------------------------\\  |        |
         ||                        /---------+-------------------+-\\ /-----------+--------------\\                   |  |        |    /--------\\
         ||                 /------+--\\      |            /------+-+-+-----------+--------------+--------------\\    |  |        |    |        |
         ||                 |      |  |      |/-----------+------+-+-+-----------+--------------+--------------+----+--+--\\     |    |        |
         || /---------------+------+--+------++-----------+--\\   | | |           |      /-------+----\\         |    |  |  |     |    |        |
         || |           /---+------+--+-\\    ||           |  |   | | |           |      |       |   /+---------+----+--+--+-----+-\\  |        |
         || |           |   |      |  | |    ||           |  |   | | |           |    /-+-------+---++---------+----+--+--+-----+-+--+\\       |
         || |           |   |      |  | |    ||           | /+---+-+-+-----------+----+-+-------+---++\\        |    |  |  |     | |  ||       |
         || |           | /-+------+--+-+----++-----------+\\||   | | |           |    | |/------+---+++--------+----+--+\\ |     | |  ||       |
         || |           | | |      |  | |    ||           ||||   | | |           |    | ||      |   |||        |    |  || |     | |  ||       |
         || |           | | | /----+--+-+----++-----------++++---+-+-+-----------+----+-++--\\   |   |||        |    |/-++-+-----+-+--++-------+----\\
/--------++-+-----------+-+-+-+----+--+-+----++-----------++++---+-+-+----\\      |    |/++--+---+---+++--------+----++-++-+-----+-+\\ ||       |    |
|        || |         /-+-+-+-+----+--+-+----++------\\    ||||   | | |    |      |    ||||  |   |   |||        |    || || |     | || ||       |    |
|        || |         | | | | |    |  | |    ||      |    ||||   | | | /--+------+----++++--+---+---+++--------+----++\\|| |     | || ||       |    |
|    /---++-+-------\\ | | | | |    |  | |    ||      |    ||||   | | | |  |      |    ||||  |   |   ||| /------+----+++++-+-----+-++-++-----\\ |    |
|    |   || |       | | | | | |    |  | |   /++------+----++++-\\ | | | |  |      |    ||||  |   |   ||| |      |    ||||| |     | || ||     | |    |
|    |   || |       | | | | | |    |  | |   |||      |    ||||/+-+-+-+-+--+------+----++++--+---+---+++-+------+----+++++-+---\\ | || ||     | |    |
|    |   || |   /---+-+-+-+-+-+----+-\\| |   |||      |    |||||| | | | |  |      |    ||||  |   |   ||| |  /---+----+++++-+---+-+-++-++-----+-+-\\  |
|/---+---++-+---+---+-+-+-+-+-+----+-++-+---+++------+----++++++-+-+\\| |  |      |    ||||  |   |   ||| |  |   |    ||||| |   | | || ||     | | |  |
||   |   || |   |   | | | | | |    | || |  /+++------+----++++++-+-+++-+--+------+----++++--+---+---+++-+--+---+----+++++-+---+-+-++-++---\\ | | |  |
||   |   || |   |   | | | | | |    | || |  ||||      |    |||||| | ||| |  |      |    ||||  |   |   ||| |  |   |    ||||| |   | | || ||   | | | |  |
||   |   || |   |   | | | | | |    | || |  ||||      | /--++++++-+-+++-+--+------+----++++--+---+---+++-+-\\|   |    ||||| |   | | || ||   ^ | | |  |
||   |   |v |   |   | | | | | |    | || |  ||||      | |  |||||| | ||| |  |      |    ||||  |   |   ||| | ||   |    ||||| |   |/+-++-++---+-+-+-+\\ |
||   |   || |   |  /+-+-+-+-+-+----+-++-+--++++------+-+--++++++-+\\||\\-+--+------+----++++--+---//--+++-+-++---+--\\ ||||| |   ||| || \\+---+-+-/ || |
||   |   || |   |  || | | | \\-+----+-+/ |  ||||      | |  |||||| |||| /+--+----\\ |    ||||  |    |  ||| | ||   |  | ||||| |   ||| ||  |   | |   || |
||/--+---++-+---+--++-+-+-+---+----+-+--+-\\||||      | |  |||||| |||| ||  |    | |    ||||  |    |  ||| | ||   |/-+-+++++-+--\\||| ||  |   | |   || |
|||  |   \\+-+---+--++-+-+-+---+----+-+--+-+++++------+-+--++++++-++++-++--+----+-+----++++--+----+--+++-+-++---++-+-/|||| |  |||| ||  |   | |   || |
|||/-+----+-+---+--++-+-+-+---+--\\ | |  | |||||      | |  |||||| |||| ||  | /--+-+----++++--+----+--+++-+-++---++-+-\\|||| |  |||| ||  |   | |   || |
\\+++-+----+-+---+--++-+-+-+---+--+-+-+--+-+++++------+-+--++++++-++++-++--/ |  | |    |||| /+----+--+++-+-++---++-+\\||||| |  |||| ||  |   | |   || |
 ||| |    | |   |  || | | |   |  | | |  | |||||      |/+--++++++-++++-++----+--+-+\\   |||| ||    |  ||| | ||   || ||||||| |  |||| ||  |   | |   || |
 ||^ |    | |   |  || | \\-+---+--+-+-+--/ |||||      |||  |||||| |||| ||    |  | ||   |||| ||    |  ||| | ||   || ||||||| |  |||| ||  |   | |   || |
 ||| |    | |   |/-++-+---+---+--+-+-+----+++++------+++--++++++-++++-++----+\\ | ||   |||| ||    |  ||| | ||   ||/+++++++-+--++++-++--+---+\\|   || |
 ||| | /--+-+---++-++\\|   |   |  | | |    |||||      |||  \\+++++-++++-++----++-+-++---++++-++----+--+++-+-++---/||||||||| |  |||| ||  |   |||   || |
 ||| | |  | |   || ||||   |   |  | | |    |||||      ||| /-+++++-++++-++----++\\| || /-++++-++----+--+++-+-++----+++++++++-+--++++\\||  |   |||   || |
 ||| | |  | |   || ||||   |   |/-+-+-+----+++++------+++-+-+++++-++++-++----++++-++-+-++++-++----+--+++-+-++----+++++++++-+--+++++++--+-\\ |||   || |
 ||| | |  | |   ||/++++---+---++-+-+-+----+++++------+++-+-+++++-++++-++----++++-++-+-++++-++----+--+++-+-++\\   ||||||||| |  |||||||  | | |||   || |
 ||| | |  | |   |||||||   | /-++-+-+-+----+++++\\     ||| | ||||| |||| ||    |||| || | |||| ||    |  ||| | |||   \\++++++++-+--/||||||  | | |||   || |
 ||| | |  | |   |||||||   | | || |/+-+----++++++-----+++-+-+++++-++++-++----++++-++-+-++++-++----+--+++-+-+++----++++++++-+---++++++--+-+\\|||   || |
 ||| | |  | |   |||||||   | | || ||| |  /-++++++-----+++-+-+++++-++++-++----++++-++-+-++++-++----+--+++-+-+++-\\/-++++++++\\|   ||||||  | |||||   || |
 ||| | |  | |   |||||||   | | || ||| |  | ||||||     ||| | ||||| |||| ||    |||| || | ||||/++----+--+++\\| ||| || ||||||||||   ||||||  | |||||   || |
 ||| | |  | |   |||||||   | | || ||| |  | ||||||/----+++-+-+++++-++++-++----++++-++\\| |||||||    |  ||||| ||| || ||||||||||   ||||||  | |||||   || |
 |||/+-+--+-+-\\ |||||||   | | || ||| |  | |||||||    ||| | ||||| |||| ||    |||| |||| |||||||    |  ||||| ||| || ||||||||||   ||||||  | |||||   || |
 |||||/+--+-+-+-+++++++---+-+-++-+++-+--+-+++++++----+++-+-+++++-++++-++----++++-++++-+++++++----+--+++++-+++-++\\||||||||||   ||||||  | ||^||   || |
 |||||||  | | | |||||||   | | || ||| |  | |||||||    ||| | ||||| |||| ||    |||| |||| |||||||    |  |||||/+++-+++++++++++++---++++++--+-+++++---++-+\\
 |||||||  | | | |||||||   | | || ||| |  | |||||||    ||| | |||\\+-++++-++----++++-++++-+++++++----+--+++++++++-+++++++++++++---/|||||  | |||||   || ||
 |||||||  | | | |||||||   | | || |||/+--+-+++++++----+++-+-+++-+-++++-++----++++-++++-+++++++--\\ |  ||||||||| |||||||||||||    |||||  | |||||   || ||
 |||||||  | | | |||||||   | | || ||\\++--+-+++++++----+++-+-+++-+-++/| ||    |||| |||| ||||||| /+-+--+++++++++-+++++++++++++---\\|||||  | |||||   || ||
 |||||||  | | | |||\\+++---+-+-++-++-++--+-+++++++----+++-+-+++-+-+/ | ||    |||| |||| ||||||| || |  ||||||||| |||||||||||||   ||||||  | |||||   || ||
 ||^||||  | | | ||| |||   | | || || ||  | |||||||    ||| | ||| | |  | ||    |||| |||| |||||\\+-++-+--+++++++++-+++++/|||||||   ||||||  | |||||   || ||
 |||||||  | | | ||| |||/--+-+-++-++-++--+-+++++++----+++-+-+++-+-+\\ | ||    |||| |||| ||||| | || |  ||||||||| ||||| |||||||   ||||||  | |||||   || ||
 |||\\+++--+-+-/ ||| ||||  | | || || ||  | |||||||    ||| | ||| | || | ||    |||| |||| ||||| | || |  |||||||\\+-+++++-+++++++---++++++--+-+++++---/| ||
 ||| |||  | |   ||| ||||  | | || || ||  | |||||||    ||| | ||| | || | ||    |||| |||| ||||| | || |  ||||||| | ||||| |||||||   ||||||  | |||||    | ||
 ||| |||  | |   ||| ||||  | | || || ||  | |||||||/---+++-+-+++-+-++-+-++----++++-++++-+++++-+-++-+--+++++++-+-+++++\\|||||||   ||||||  | |||||    | ||
 ||| |||  | |   ||| ||||  | | || || ||  | ||||||||   ||| | ||| | || | ||    |||| |||| ||||| | || |  ||||||| | |||||||||||||   ||||||  | |||||    | ||
 ||| |||  | |   ||| ||||  | | || || ||  | ||||||||   ||| | |\\+-+-++-+-++----++++-++++-+++++-+-++-+--++/|||| | |||||||||||||   ||||||  | |||||    | ||
 ||| |||  |/+---+++-++++--+-+-++-++-++--+-++++++++---+++-+\\| | | || | ||    |||| |||| ||||| | || |  || |||| | |||||||||||||   ||||||  | |||||    | ||
 ||| |||  |||   ||| ||||  | | || || || /+-++++++++---+++-+++-+-+-++-+-++----++++-++++-+++++-+-++-+--++-++++-+-+++++++++++++---++++++--+-+++++--\\ | ||
 ||| |||  |||   ||| |||| /+-+-++-++-++-++-++++++++---+++-+++-+-+-++-+-++----++++-++++-+++++-+-++-+--++-++++-+-+++++++++++++---++++++\\ | |||||  | | ||
 ||| |||  |||   ||| |||| || |/++-++-++-++-++++++++---+++-+++-+-+-++-+-++----++++-++++-+++++-+-++-+--++\\|||| | ||||||||^||||   ||||||| | |||||  | | ||
 ||| |||  |||   ||| |||| || |||| || || || ||||\\+++---+++-+++-+-+-++-+-++----++++-++++-+++++-+-++-+--+++++++-+-++++++++++++/   ||||||| | |||||  | | ||
 ||| |||  |||   ||| ||\\+-++-++++-++-++-++-++++-+++---/|| ||| | | || | ||    |||| |||| ||||| | \\+-+--+++++++-+-++++++++++++----/|||||| | |||||  | | ||
 ||| |||  |||   ||| || | || |||| || || || |||| |||/---++-+++-+-+-++-+-++-\\  |||| |||| ||||| |  | |  ||||||| | |\\+++++++++/     |||||| | |||||  | | ||
 ||| |||  |||   ||| || | || |||| || || || |||| ||||   || ||| | | ||/+-++-+--++++-++++-+++++-+--+-+--+++++++-+-+-+++++++++------++++++-+-+++++--+-+\\||
 ||| |||  |||   ||| || | || |||| || || || |||| ||||   || ||| | | |||| || |  |||| |||| ||||| |  | |  ||||||| | | |||||||||      |||||| | |||||  | ||||
 ||| ||\\--+++---+++-+/ | || |||| || || || |||| ||||   ^| ||| | | |||| || |  |||| |||| ||||| |  | |  ||||||| | | |||||||||      |||||| | |||||  | ||||
 ||| ||   |||   ||| |  | || |||| || || || |||| ||||   || \\++-+-+-++++-++-+--++/| |||| v|||| |/-+-+--+++++++-+-+-+++++++++\\     |||||| | |||||  | ||||
 ||| ||   |||   ||| |  | || |||| || || || |||| ||||   \\+--++-+-+-++++-++-+--++-+-+/|| |||||/++-+-+--+++++++-+-+-++++++++++\\    |||||| | |||||  | ||||
 ||| ||   |||   ||| |  | || |||| || || || |||| ||||    |/-++-+-+-++++\\|| |  || | | || |||||||| | |  ||||||| | | |||||||||||    |||||| | |||||  | ||||
 |||/++---+++\\  ||| |  | || |||| || || ||/++++-++++----++-++-+-+-+++++++-+--++-+-+-++-++++++++-+-+--+++++++-+-+-+++++++++++--\\ |||||| | |||||  | ||||
 ||||||   ||||  ||| |  | ^| |||| || ||/+++++++-++++----++-++-+-+-+++++++-+--++-+-+-++-++++++++-+-+--+++++++-+-+-+++++++++++--+-++++++\\| |||||  | ||||
 ||||||   ||||  ||| |  | || ||||/++-++++++++++-++++----++-++-+-+-+++++++-+--++-+-+-++-++++++++-+-+->+++++++-+-+-+++++++++++--+-++++++++\\|||||  | ||||
 ||||||   ||||  ||| |  | || ||||||| |||||||||| ||||    || || | | ||||||| |  || | | || |||||||| | |  ||||||| | | |||||||||||  | ||||||||||||||  | ||||
 ||||||   ||||  ||| |  | || ||||||| |||||||||| ||||    || || | | ||||||| |  || | | || ||\\+++++-+-+--+/||||| | | |||||||||||  | ||||||||||||||  | ||||
 ||||||   ||||  ||| |  | || |\\+++++-++++++++++-++++----++-++-+-+-+++++++-+--++-+-+-++-++-+++++-+-+--+-/|||| | | |||||||||||  | ||||||||||||||  | ||||
 ||||||   ||||  ||| |  | || | ||||| |||||||||| ||||    || || | | ||||||| |  || | | || || ||||\\-+-+--+--++++-+-+-+++++++++/|  | ||||||||||||||  | ||||
 ||||||   ||||  ||| |  | || | ||||| |||\\++++++-++++----++-++-+-+-+++++++-+--++-+-+-++-++-++++--+-+--+--++++-+-+-+++++++++-+--+-++++++++++++++--/ ||||
 ||||||   ||||  ||| |  | ||/+-+++++-+++-++++++-++++----++-++-+-+-+++++++-+--++-+-+-++-++-++++--+-+--+--++++-+-+-+++++++++-+\\ | ||||||||||||||    ||||
 ||||||   ||||  ||| |  | |||\\-+++++-+++-++++++-/|||    || || | |/+++++++-+--++-+-+-++-++-++++--+-+--+--++++-+\\| ||||||||| || | |||||v||||||v|    ||||
 ||||||   \\+++--+++-+--+-+++--+++++-+++-+++++/  |||    || || | ||||||||| |  \\+-+-+-++-++-++++--+-+--+--++++-+++-++++/|||| || | ||||||||||||||    ||||
 ||||||    |||  ||| |  | |||  ||||| ||| |||||   |||    || || | ||||||||| |   | | | || || ||||  | |  |  |||| ||| |||| |||| || | ||||||||||||||    ||||
/++++++----+++--+++-+--+-+++--+++++-+++-+++++---+++----++-++-+-+++++++++-+\\  | | |/++-++-++++--+-+--+--++++-+++-++++\\|||| || | ||||||||||||||    ||||
|||||||    |||  ||| |  | \\++--+++++-+++-+++++---+++----++-++-+-+++++++++-++--+-+-++++-++-++++--+-+--+--++++-+++-+++++++++-++-+-+++++/||||||||    ||||
|||||||    |||  ||| |  |  ||  ||||| |||/+++++---+++----++\\||/+-+++++++++-++--+-+-++++-++-++++--+-+--+--++++-+++-+++++++++-++-+-+++++-++++++++\\   ||||
|||||||    |||  ||| |  |  ||  ||||| |||||||||   |||    |||||||/+++++++++-++--+-+\\|||| || ||||  | |  |  |||| ||| ||||||||| || | ||||| |||||||||   ||||
|||||||    |||  ||| |  |  ||  \\++++-+++++++++---+++----+++++++++++++++++-++--+-++++++-++-+++/  | |  \\--++++-+++-+++++++++-++-+-+++/| |||||||||   ||||
|||||||/---+++--+++-+--+--++---++++-+++++++++---+++----+++++++++++++++++-++--+-++++++-++-+++--\\| |     |||| ||| ||||||||| || | ||| | |||||||||   ||||
||||||||   |||  \\++-+--+--++---++++-+/|||||||   |||    \\++++++++++++++++-++--+-++++++-++-+++--++-+-----+++/ ||| ||||||||| || | ||| | |||||||||   ||||
||||||||   |||   || |  |  ||   |||| | ||||||\\---+++-----+++++++/|\\++++++-++--+-++++++-++-+++--++-+-----+++--+++-+++++++++-++-+-+/| | |||||||||   ||||
||||||||   |||   || |  |  ||   |||| | ||||||    |||     ||||||| | |||||| ||  | |||||| || |||  || |    /+++--+++<+++++++++-++\\| | | | |||||||||   ||||
||||||||   |||   || |  |  ||   |||| | ||||||    |||     ||||||| | |||||| ||  | |||||| || |||  || |    ||||  ||| ||||||||| |||| | | | |||||||||   ||||
||||||||/--+++---++\\|  |  ||   |||| | ||||||    |||     ||||||| | |||||| ||  | ||\\+++-++-+++--++-+----++++--+++-+++++++/| |||| | | | |||||||||   ||||
|||||||||  |||   ||||  |  ||   |||| | ||||||    \\++-----+++++++-+-++++++-++--+-++-+/| || |||  || |    ||||  ||| ||||||| | |||| | | | |||||||||   ||||
|||||||||  |||   ||||  |  ||   |||| | ||||||     ||     ||||||| | |||||| ||  | || | | || ||| /++-+----++++--+++-+++++++-+\\|||| | | | |||||||||   ||||
|||||||||  |||   ||||  |  ||   |||| | |\\++++-----++-----+/||||| | |||||| ||  | || | | || ||| ||| |    ||||  ||| ||||||| |||||| | | | |||||||||   ||||
|||||||||  |||   ||||  |  ||   |||| | | ||||/----++-----+-+++++-+-++++++-++--+-++-+-+\\|| ||| ||| |    ||||  ||| |||||\\+-++++++-+-+-+-+++++++++---++/|
|||||||||  |||   ||||  |  ||   |||\\-+-+-+++++----++-----+-+++++-+-++++++-++--+-++-+-++++-+++-+++-+----++++--+++-+++++-+-++++++-+-+-+-++++/||||   || |
|||||||||  ||| /-++++--+--++---+++--+-+-+++++---\\||     \\-+++++-+-+++/|| ||  | || | |||| ||| ||| |    ||||  ||| ||||| | |||||| | | | |||| ||||   || |
|||||\\+++--+++-+-+++/  |  |\\---+++--+-+-+++++---+++-------+++++-+-+++-++-++--+-++<+-++++-+++-+++-+----++++--+++-+++++-+-+++/|| | | | |||| ||||   || |
|||\\+-+++--+++-+-+++---+--+----++/  | | |||||   |||       ||||| | ||| || ||  | || | |||| ||| ||| |    ||||  ||| ||||| | ||| || | | | |||| ||||   || |
||| | |||  ||| | |||   |  |    ||   | | |||||   |||       ||||| | ||| || ||  | || | |||| ||| ||| \\----++++--+++-++/|| | ||| || | | | |||| ||||   || |
||| | |||  ||| | \\++---+--+----++---+-+-+++++---+++-------+++++-+-+++-++-++--/ || | |||| \\++-+++------++++--+++-++-++-+-/|| || | | | |||| ||||   || |
||| | |||  ||| |  ||   |  |   /++---+-+\\|||||   |||       ||||| | ||| || ||    || | ||\\+--++-+++------++++--+++-++-++-+--++-++-+-+-+-+/|| ||||   || |
||| | |||  ||| |  ||   |  |   |||   | |||||||   |||       ||||| | ||| || || /--++-+-++-+--++-+++------++++--+++-++-++\\|  || || | | | | || ||||   || |
||| | |||  |\\+-+--++---+--+---+++---+-+++++++---+++-------+++/| | ||| |\\-++-+--++-+-++-+--++-+++------++++--+++-++-+++/  || || | | | | || ||||   || |
||| | |||  | | |  ||   |  |   |||   | ||||||v   |||       ||| | | ||| |  || |  || | || |  || |||      ||||  ||| || |||   || || | | | | || ||||   || |
||| | |||  | | |  ||   |  \\---+++---+-+++++++---+++-------+/| | | ||| |  || |  || | || |  || |||      ||||  ||| || |||   || || | | | | || ||||   || |
||| | |||  | | |/-++---+---\\  |||   | |||||||   |||       | | | | ||| |  || |  || | || |  || |||      \\+++--+++-++-+++---++-/| | | | | || ||||   || |
||| | |||  | | || ||   |   |  |||   | |||||||  /+++------\\| | | | ||| |  || |  || | || |  || |||       |||  ||| || |||   ||  | | | | | || ||||   || |
||| | |||  | | || ||   |   |  |||   | ||\\++++--++++------++-+-+-+-+++-+--++-+--++-+-++-+--++-+++-------+++--++/ || |||   ||  | | | | | || ||||   || |
||| | |||  | | || ||   |   |  |||  /+-++-++++--++++------++-+-+-+-+++-+--++-+--++-+-++-+-\\|| |||       ||| /++--++-+++---++--+-+-+-+-+-++-++++--\\|| |
||| \\-+++--+-/ || ||   |   |  ^|\\--++-++-++++--++++------++-+-+-+-+++-+--++-+--++-+-++-+-+++-+++-------+++-+++--++-+++---++--+-+-+-+-+-/| ||||  ||| |
|\\+---+++--+---++-++---+---+--++---++-++-++++--++++------++-+-+-+-++/ |  || |  || | || | ||| |||       ||| |||  || |||   ||  | | | | |  | ||||  ||| |
\\-+---+++--+---++-++---+---+--++---++-++-++++--++++------++-+-+-+-++--+--+/ |  || \\-++-+-+++-+++-------+++-+++--++-+/|   ||  | | | | |  | ||||  ||| |
  |   ||| /+---++-++---+---+--++---++-++-++++--++++------++-+-+-+-++--+--+--+--++--\\|| | ||| |||       ||| |||  || | |   ||  | | | | |  | ||||  ||| |
  |   ||| ||   ||/++---+---+--++---++-++-++++--++++------++-+-+-+-++--+-\\|  \\--++--+++-+-+++-+++-------+++-+++--++-+-/   ||  | | | | |  | ||||  ||| |
  |   ||| ||   |\\+++---+---/  ||   || || ||||  ||||      || | | | ||  | ||     ||  ||| | ||\\-+++-------+++-+++--++-+-----+/  | | | | |  | ||||  ||| |
  |   ||| ||   | |||   |      ||   || || ||||  ||||      || | | | ||  | ||     ||  ||| \\-++--+++-------+++-+++--++-+-----+---+-+-+-/ |  | ||||  ||| |
/-+---+++-++--\\| |||   |    /-++---++-++-++++--++++------++-+-+-+-++\\ | ||     ||  |||   ||  |||       ||| |||  || |     |   | \\-+---+--+-++++--+/| |
| |   ||| ||  || |||   |    | ||   || || ||||  ||||      || | | | ||| | ||     ||  |||   ||  |||       ||| |||  || |     |   |   |   |  | ||||  | | |
| |   ||| ||  || |||   |    | ||   || || ||||  ||||      || | | | ||| | ||     ||  |||   ||  |||       |\\+-+++--++-+-----+---+---+---+--+-++/|  | | |
| \\---+++-++--++-+++---+----+-++---++-++-+/||  ||||      || | | | |\\+-+-++-----++--+++---++--+++-------+-+-+++--++-+-----+---+---+---+--+-++-+--+-/ |
|     ||| ||  || |||   |    | \\+---++-+/ | ||  ||||      || | | | | | | ||     ||  |||/--++--+++-------+-+-+++--++-+-----+--\\|   |   |  | || |  |   |
|     ||| ||  || |||   |    |  |   || |  | ||  ||||      || | | | | | | ||     ||  ||||  ||  |||       | | |||  || |     |  ||   |   |  | || |  |   |
|     ||| ||  || |||   |    |  |   || |  \\-++--++++------++-+-+-+-+-+-+-++-----++--++++--++--+++-------+-+-+++--++-+-----+--+/   |   |  | || |  |   |
|     ||| ||  || \\++---+----+--+---++-+----++--++++------++-+-+-+-+-+-+-/|     ||  ||||  ||  |||       | | |||  || |     |  |    |   |  | || |  |   |
|     ||| ||  ||  ||   |    |  |   || |    \\+--++++------++-+-+-+-+-+-+--+-----++--++++--++--+++-------+-+-+++--++-+-----+--+----+---+--+-/| |  |   |
|     ||| ||  ||  ||   |    |  \\---++-+-----+--++++------++-+-+-+-+-+-+--+-----++--++++--++--+++-------+-+-+++--++-+-----+--+----+---+--/  | |  |   |
\\-----+++-++--/|  ||   |    |      || \\-----+--++++------++-+-+-+-+-+-+--+-----++--++++--++--+++-------+-+-+++--++-+-----+--+----+---/     | |  |   |
      ||| ||   |  ||   |    |      ||       |  ||||      || \\-+-+-+-+-+--+-----++--++++--++--+++-------+-+-+++--++-+-----+--+----+---------+-/  |   |
      ||| \\+---+--++---+----+------++-------+--++++------++---+-+-+-+-+--+-----++--/|||  ||  |||       | | |||  || |     |  |    |         |    |   |
      |||  \\---+--++---+----+------++-------+--++++------+/   | | | | \\--+-----/|   |||  ||  |||   /---+-+-+++--++-+-----+--+----+----\\    |    |   |
      |||      |  ||   |    |      ||       |  ||||      |    | | | |    |      |   |||  ||  \\++---+---+-+-+++--++-+-----/  |    |    |    |    |   |
      |||      |  ||   |    |      ||       |  |||\\------+----+-+-+-+----/      |   |||  ||   ||   |   | | |||  || |        |    |    |    |    |   |
      |||      |  ||   |    |      ||       |  |||       |    \\-+-+-+-----------/   |||  |\\---++---+---/ | \\++--++-+--------+----+----+----+----/   |
      |||      |  ||   |    |      |\\-------+--+++-------+------+-+-+---------------+++--+----+/   |     |  ||  || |        |    |    |    |        |
      |||      |  ||   \\----+------+--------+--+++-------+------+-/ |               \\++--+----+----+-----+--++--++-+--------+----/    |    |        |
      |\\+------+--++--------+------+--------+--+++-------+------+---+----------------++--+----/    |     |  ||  |\\-+--------+---------+----/        |
      | \\------+--+/        \\------+--------+--+++-------+------+---/                |\\--+---------+-----+--++--+--+--------/         |             |
      \\--------+--+----------------+--------+--+++-------+------+--------------------+---+---------+-----+--++--/  |                  |             |
               |  |                |        |  |||       |      |                    |   |         \\-----+--++-----+----------<-------/             |
               \\--+----------------+--------+--+/|       |      |                    |   |               |  ||     |                                |
                  \\----------------+--------+--+-+-------+------+--------------------+---+---------------+--/|     |                                |
                                   |        |  | |       |      |                    |   |               |   |     |                                |
                                   |        |  \\-+-------/      |                    |   |               |   |     |                                |
                                   |        |    |              \\--------------------+---+---------------+---/     |                                |
                                   |        \\----+-----------------------------------/   |               \\---------+--------------------------------/
                                   |             \\---------------------------------------+-------------------------/
                                   \\-----------------------------------------------------/
"""
