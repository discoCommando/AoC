module E11 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Intcode
import Parser exposing (..)
import Set exposing (Set)


main =
    Helpers.makeMain [ Debug.toString result1, Debug.toString result2 ]


type Direction
    = Up
    | Down
    | Left
    | Right


type Turn
    = TLeft
    | TRight


turn : Turn -> Direction -> Direction
turn t direction =
    case t of
        TLeft ->
            case direction of
                Up ->
                    Left

                Down ->
                    Right

                Left ->
                    Down

                Right ->
                    Up

        TRight ->
            case direction of
                Up ->
                    Right

                Down ->
                    Left

                Left ->
                    Up

                Right ->
                    Down


intToTurn : Int -> Turn
intToTurn i =
    case i of
        0 ->
            TLeft

        1 ->
            TRight

        _ ->
            Debug.todo "intToTurn"


type Panel
    = White
    | Black


panelToInt : Panel -> Int
panelToInt p =
    case p of
        White ->
            1

        Black ->
            0


intToPanel : Int -> Panel
intToPanel i =
    case i of
        1 ->
            White

        0 ->
            Black

        _ ->
            Debug.todo "inttopanel"


type alias Position =
    ( Int, Int )


type alias Cell =
    { panel : Panel, paintedCount : Int }


type alias Board =
    Dict Position Cell


get : Position -> Board -> Cell
get pos board =
    board |> Dict.get pos |> Maybe.withDefault (Cell Black 0)


paint : Position -> Panel -> Board -> Board
paint pos panel board =
    board
        |> Dict.update pos
            (\mc ->
                Just <|
                    case mc of
                        Just c ->
                            { c | panel = panel, paintedCount = c.paintedCount + 1 }

                        Nothing ->
                            Cell panel 1
            )


type alias State =
    { direction : Direction, board : Board, intcode : Intcode.State, position : Position }


initialIntcode : Intcode.State
initialIntcode =
    { array = parsed
    , input = []
    , output = []
    , status = Intcode.Going 0
    , relativeBase = 0
    }


moveForward : Position -> Direction -> Position
moveForward ( x, y ) dir =
    case dir of
        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )

        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )


initialState : Panel -> State
initialState initialPanel =
    State Up (Dict.empty |> Dict.insert ( 0, 0 ) (Cell initialPanel 0)) initialIntcode ( 0, 0 )


walk : State -> State
walk state =
    let
        currentPanel =
            get state.position state.board |> .panel

        oldintcode =
            state.intcode

        newintcode =
            Intcode.walk { oldintcode | input = [ currentPanel |> panelToInt ], output = [] }

        newstate =
            case newintcode.output of
                [ t, l ] ->
                    let
                        panel =
                            intToPanel l

                        turn_ =
                            intToTurn t

                        newDirection =
                            turn turn_ state.direction
                    in
                    { state
                        | direction =
                            newDirection
                        , board = paint state.position panel state.board
                        , intcode = newintcode
                        , position = moveForward state.position newDirection
                    }

                _ ->
                    Debug.todo "walk"
    in
    case newintcode.status of
        Intcode.Halted ->
            newstate

        Intcode.Going _ ->
            walk newstate


parsed =
    String.split "," input |> List.map Helpers.toI |> List.indexedMap Tuple.pair |> Dict.fromList


paintBoard : Board -> Board
paintBoard board =
    let
        tol =
            board |> Dict.keys

        minx =
            tol |> List.sortBy Tuple.first |> Helpers.at 0 |> Tuple.first

        miny =
            tol |> List.sortBy Tuple.second |> Helpers.at 0 |> Tuple.second

        arr =
            Array.initialize 40
                (\x ->
                    Array.initialize 40
                        (\y ->
                            case Dict.get ( x + minx, y + miny ) board of
                                Just c ->
                                    case c.panel of
                                        Black ->
                                            'B'

                                        White ->
                                            'W'

                                Nothing ->
                                    'N'
                        )
                )
                --                |> Array.map (Array.toList >> String.fromList)
                --               |> Array.toList
                --              |> String.join "\n"
                |> Debug.log "bbbb"
    in
    board


result1 =
    walk (initialState Black) |> Debug.log "final" |> .board |> paintBoard |> Dict.toList |> List.length


result2 =
    walk (initialState White) |> Debug.log "final" |> .board |> paintBoard |> Dict.toList |> List.length


input =
    """3,8,1005,8,338,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,29,2,105,19,10,1006,0,52,1,1009,7,10,1006,0,6,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,1001,8,0,64,2,1002,19,10,1,8,13,10,1,1108,16,10,2,1003,1,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,103,1006,0,10,2,109,16,10,1,102,11,10,2,6,13,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,140,2,102,8,10,2,4,14,10,1,8,19,10,1006,0,24,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,177,1006,0,16,1,1007,17,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,205,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,228,1,1005,1,10,1,9,1,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,258,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,279,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,301,1,3,17,10,2,7,14,10,2,6,18,10,1,1001,17,10,101,1,9,9,1007,9,1088,10,1005,10,15,99,109,660,104,0,104,1,21102,1,48092525312,1,21101,355,0,0,1106,0,459,21102,665750184716,1,1,21102,366,1,0,1106,0,459,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,235324768296,1,21101,0,413,0,1105,1,459,21101,3263212736,0,1,21102,424,1,0,1106,0,459,3,10,104,0,104,0,3,10,104,0,104,0,21102,1,709496824676,1,21101,447,0,0,1105,1,459,21102,988220904204,1,1,21102,1,458,0,1106,0,459,99,109,2,21201,-1,0,1,21102,40,1,2,21102,490,1,3,21102,1,480,0,1105,1,523,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,485,486,501,4,0,1001,485,1,485,108,4,485,10,1006,10,517,1101,0,0,485,109,-2,2105,1,0,0,109,4,2101,0,-1,522,1207,-3,0,10,1006,10,540,21102,0,1,-3,22101,0,-3,1,22102,1,-2,2,21102,1,1,3,21101,559,0,0,1106,0,564,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,587,2207,-4,-2,10,1006,10,587,22102,1,-4,-4,1105,1,655,22101,0,-4,1,21201,-3,-1,2,21202,-2,2,3,21102,606,1,0,1105,1,564,21202,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,625,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,647,22101,0,-1,1,21101,647,0,0,105,1,522,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0"""
