module E13 exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Dict exposing (Dict)
import Helpers
import Html exposing (Html)
import Intcode
import Json.Decode as Decode
import Parser exposing (..)
import Set exposing (Set)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--    Helpers.makeMain [ Debug.toString result1, Debug.toString result2 ]


view : Model -> Html Msg
view model =
    let
        strings =
            model.board |> boardToString
    in
    Html.div []
        [ strings
            |> List.map (Html.text >> List.singleton >> Html.p [])
            |> Html.div []
        , Html.text ("Score: " ++ String.fromInt model.score)
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    let
        _ =
            Debug.log "key" string
    in
    case string of
        "a" ->
            Left

        "w" ->
            Right

        _ ->
            Stay


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress keyDecoder


parsed =
    String.split "," input |> List.map Helpers.toI |> List.indexedMap Tuple.pair |> Dict.fromList


type alias Board =
    Array (Array Tile)


type Direction
    = R
    | L


type alias Model =
    { state : Intcode.State, board : Board, score : Int, paddleX : Int }



--initialModel : IntcodejModel
--initialModel state =
--    Model state


type Msg
    = Stay
    | Left
    | Right


type Tile
    = Empty
    | Wall
    | Block
    | HPaddle
    | Ball


intToTile : Int -> Tile
intToTile i =
    case i of
        0 ->
            Empty

        1 ->
            Wall

        2 ->
            Block

        3 ->
            HPaddle

        4 ->
            Ball

        _ ->
            Debug.todo "intToTile"


type alias Position =
    { x : Int, y : Int, tile : Tile }


type Output
    = Pos Position
    | Score Int


getPositions : List Output -> List Position
getPositions outputs =
    outputs
        |> List.filterMap
            (\output ->
                case output of
                    Pos p ->
                        Just p

                    _ ->
                        Nothing
            )


parseOutputs : List Int -> List Output
parseOutputs l =
    case l of
        [] ->
            []

        x :: y :: t :: rest ->
            (if (x == -1) && (y == 0) then
                Score t
             else
                Pos <| Position x y (intToTile t)
            )
                :: parseOutputs rest

        _ ->
            Debug.todo "getPositions"


set : Position -> Board -> Board
set position board =
    let
        row =
            Helpers.uG position.y board
    in
    Array.set position.y (Array.set position.x position.tile row) board


updateOutputs : List Output -> Model -> Model
updateOutputs outputs model =
    case outputs of
        [] ->
            model

        output :: rest ->
            updateOutputs rest
                (case output of
                    Pos position ->
                        case position.tile of
                            HPaddle ->
                                { model | board = model.board |> set position, paddleX = position.x }

                            _ ->
                                { model | board = model.board |> set position }

                    Score t ->
                        { model | score = t }
                )


createBoard : List Position -> Board
createBoard positions =
    let
        maxx =
            positions |> List.map .x |> List.maximum |> Helpers.uM

        maxy =
            positions |> List.map .y |> List.maximum |> Helpers.uM

        emptyBoard =
            Array.initialize (maxy + 1)
                (\y ->
                    Array.initialize (maxx + 1)
                        (\x ->
                            Empty
                        )
                )
    in
    positions |> List.foldl set emptyBoard


boardToString : Board -> List String
boardToString board =
    board
        |> Array.toList
        |> List.map
            (Array.toList
                >> List.map
                    (\tile ->
                        case tile of
                            Empty ->
                                'E'

                            Wall ->
                                '#'

                            Block ->
                                'B'

                            HPaddle ->
                                'H'

                            Ball ->
                                'O'
                    )
                >> String.fromList
            )


msgToInt : Msg -> Int
msgToInt msg =
    case msg of
        Stay ->
            0

        Left ->
            -1

        Right ->
            1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --    let
    --        msgInt =
    --            msgToInt msg
    --
    --        intcodeState =
    --            model.state
    --
    --        outputs =
    --            intcodeState.output |> List.reverse |> parseOutputs
    --    in
    --    ( { model | state = { intcodeState | output = [], input = [ msgInt ] } |> Intcode.walk } |> updateOutputs outputs, Cmd.none )
    ( (cycle >> cycle >> cycle >> cycle >> cycle >> cycle) model, Cmd.none )


getBall : List Position -> Position
getBall =
    List.filter (.tile >> (==) Ball) >> Helpers.at 0


cycle : Model -> Model
cycle model =
    case model.state.status of
        Intcode.Halted ->
            model

        _ ->
            let
                state =
                    model.state

                _ =
                    Debug.log "score" model.score

                _ =
                    Debug.log "board" model.board

                previousBall =
                    state.output |> List.reverse |> parseOutputs |> getPositions |> Debug.log "c0" |> List.filter (.tile >> (==) Ball) |> List.head |> Maybe.withDefault { tile = Ball, x = 0, y = 0 }

                ball =
                    { state | input = [ 0 ], output = [] } |> Intcode.walk |> .output |> List.reverse |> parseOutputs |> getPositions |> Debug.log "c1" |> List.filter (.tile >> (==) Ball) |> List.head |> Maybe.withDefault previousBall

                dirInt =
                    if previousBall.y == 18 && ball.y == 17 && previousBall.x - model.paddleX == 0 then
                        0
                    else
                        ball.x - model.paddleX

                _ =
                    Debug.log "c2" ( dirInt, ball, model.paddleX )

                newState =
                    { state | input = [ dirInt ], output = [] } |> Intcode.walk

                _ =
                    Debug.log "c3" (newState.output |> List.reverse |> parseOutputs)

                newOutputs =
                    newState |> .output |> List.reverse |> parseOutputs
            in
            { model | paddleX = model.paddleX + dirInt, state = newState } |> updateOutputs newOutputs



--    let
--       dir = case model.ballDirection of
--            R -> 1
--            L -> -1
--       positionToCheck = model.ballPosition |> \(x, y) -> (x + dir, y)
--       tile = model.board |> Helpers.uG y |> Helpers.uG x
--       isBlocking = tile /= Empty
--       state = model.state
--       newDirection = if isBlocking then case model.ballDirection of
--            R -> L
--            L -> R
--
--    in
--        { model | ballDirection = newDirection
--        , state = { state | output = [], input = [ case newDirection of
--            R -> 1
--            L -> -1] } |> Intcode.walk }


initialState1 =
    Intcode.State parsed [] [] (Intcode.Going 0) 0


initialState2 =
    Intcode.State (parsed |> Dict.insert 0 2) [] [] (Intcode.Going 0) 0 |> Intcode.walk


init : {} -> ( Model, Cmd Msg )
init _ =
    Tuple.pair
        { state = { initialState2 | output = [] }
        , score = 0
        , board = createBoard (initialState2.output |> List.reverse |> parseOutputs |> Debug.log "aaa" |> getPositions)
        , paddleX = 21
        }
        Cmd.none


result1 =
    initialState1
        |> Intcode.walk
        |> Debug.log "state"
        |> .output
        |> List.reverse
        |> parseOutputs
        |> getPositions
        |> List.filter (.tile >> (==) Block)
        |> List.length


result2 =
    init {} |> Tuple.first |> cycle


input =
    """1,380,379,385,1008,2445,260599,381,1005,381,12,99,109,2446,1102,0,1,383,1101,0,0,382,20102,1,382,1,21001,383,0,2,21101,0,37,0,1106,0,578,4,382,4,383,204,1,1001,382,1,382,1007,382,43,381,1005,381,22,1001,383,1,383,1007,383,21,381,1005,381,18,1006,385,69,99,104,-1,104,0,4,386,3,384,1007,384,0,381,1005,381,94,107,0,384,381,1005,381,108,1106,0,161,107,1,392,381,1006,381,161,1102,1,-1,384,1106,0,119,1007,392,41,381,1006,381,161,1101,0,1,384,21001,392,0,1,21102,1,19,2,21101,0,0,3,21102,1,138,0,1106,0,549,1,392,384,392,20101,0,392,1,21101,0,19,2,21101,3,0,3,21102,1,161,0,1106,0,549,1101,0,0,384,20001,388,390,1,21002,389,1,2,21102,1,180,0,1106,0,578,1206,1,213,1208,1,2,381,1006,381,205,20001,388,390,1,21001,389,0,2,21102,1,205,0,1106,0,393,1002,390,-1,390,1101,1,0,384,21002,388,1,1,20001,389,391,2,21102,228,1,0,1106,0,578,1206,1,261,1208,1,2,381,1006,381,253,21001,388,0,1,20001,389,391,2,21102,1,253,0,1105,1,393,1002,391,-1,391,1102,1,1,384,1005,384,161,20001,388,390,1,20001,389,391,2,21101,279,0,0,1105,1,578,1206,1,316,1208,1,2,381,1006,381,304,20001,388,390,1,20001,389,391,2,21101,0,304,0,1106,0,393,1002,390,-1,390,1002,391,-1,391,1101,0,1,384,1005,384,161,20102,1,388,1,20102,1,389,2,21101,0,0,3,21101,338,0,0,1106,0,549,1,388,390,388,1,389,391,389,20102,1,388,1,20101,0,389,2,21102,1,4,3,21102,1,365,0,1105,1,549,1007,389,20,381,1005,381,75,104,-1,104,0,104,0,99,0,1,0,0,0,0,0,0,284,19,16,1,1,21,109,3,22101,0,-2,1,22102,1,-1,2,21102,0,1,3,21102,1,414,0,1106,0,549,22102,1,-2,1,22101,0,-1,2,21101,0,429,0,1106,0,601,1202,1,1,435,1,386,0,386,104,-1,104,0,4,386,1001,387,-1,387,1005,387,451,99,109,-3,2105,1,0,109,8,22202,-7,-6,-3,22201,-3,-5,-3,21202,-4,64,-2,2207,-3,-2,381,1005,381,492,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,481,21202,-4,8,-2,2207,-3,-2,381,1005,381,518,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,507,2207,-3,-4,381,1005,381,540,21202,-4,-1,-1,22201,-3,-1,-3,2207,-3,-4,381,1006,381,529,21201,-3,0,-7,109,-8,2106,0,0,109,4,1202,-2,43,566,201,-3,566,566,101,639,566,566,2101,0,-1,0,204,-3,204,-2,204,-1,109,-4,2105,1,0,109,3,1202,-1,43,593,201,-2,593,593,101,639,593,593,21001,0,0,-2,109,-3,2105,1,0,109,3,22102,21,-2,1,22201,1,-1,1,21102,1,457,2,21101,0,364,3,21101,903,0,4,21101,0,630,0,1105,1,456,21201,1,1542,-2,109,-3,2105,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,2,2,2,2,0,0,0,0,2,0,0,0,2,0,0,0,2,0,2,0,0,0,0,2,2,0,2,0,2,0,0,0,2,2,0,0,0,2,0,0,1,1,0,2,0,2,2,0,2,2,0,2,0,2,2,0,2,2,0,2,2,2,0,0,2,2,2,2,2,2,0,0,2,0,2,2,2,2,2,0,2,0,0,1,1,0,2,0,0,2,0,2,2,0,2,2,2,2,0,0,0,0,2,2,2,2,2,0,2,0,2,2,2,2,2,2,0,0,2,2,2,2,2,2,0,0,1,1,0,2,2,0,0,2,2,2,2,2,0,2,0,2,0,2,0,0,2,2,0,2,0,2,0,2,2,2,0,2,2,0,2,2,2,0,2,2,0,2,0,1,1,0,0,2,2,2,2,0,2,2,0,0,2,0,0,2,0,2,2,0,2,0,0,2,0,0,0,2,2,0,2,0,2,2,2,2,2,2,0,2,0,0,1,1,0,0,2,2,0,2,2,2,2,2,2,2,0,0,2,2,2,2,0,2,0,2,2,0,2,0,2,0,0,0,2,2,0,2,0,0,2,2,2,2,0,1,1,0,2,0,2,0,2,2,0,2,2,2,2,0,2,2,0,0,2,0,0,2,2,2,2,0,2,2,2,2,2,0,0,0,0,2,2,2,2,2,0,0,1,1,0,2,0,2,2,2,2,2,0,2,0,0,0,2,2,0,0,0,0,2,2,2,0,2,2,2,2,0,0,0,0,2,0,2,2,2,2,0,0,2,0,1,1,0,0,2,2,0,2,0,0,2,0,0,0,2,0,0,0,0,2,0,0,0,2,0,2,0,0,2,2,0,2,0,2,0,2,0,0,2,2,2,0,0,1,1,0,2,2,0,2,2,0,2,0,2,0,2,0,2,2,0,0,2,0,0,0,0,2,2,0,2,2,2,0,0,0,0,2,2,0,0,2,2,2,2,0,1,1,0,2,2,2,0,0,0,0,0,2,2,2,2,2,2,0,0,0,2,2,0,2,2,0,0,2,2,0,2,2,2,2,2,2,0,0,0,0,0,0,0,1,1,0,0,0,0,2,0,2,2,0,2,2,0,0,0,2,2,0,0,2,2,0,2,2,2,0,2,0,0,0,0,0,0,2,0,0,2,2,0,2,2,0,1,1,0,2,2,0,2,2,0,0,2,0,0,2,0,2,2,2,0,2,0,2,2,0,2,2,0,0,2,2,0,0,0,0,2,2,2,0,2,0,2,2,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,14,83,53,46,55,2,66,81,32,11,4,66,63,86,28,36,35,4,98,32,66,15,27,12,31,43,97,55,57,22,71,23,95,69,74,44,73,88,80,23,36,64,30,19,80,51,54,78,53,97,76,64,64,94,30,54,42,27,35,23,58,71,1,60,39,6,14,39,21,54,66,38,30,90,37,70,59,92,82,40,43,52,69,23,8,80,77,20,16,25,29,82,4,86,35,50,72,51,42,60,45,83,71,4,56,14,14,21,24,45,5,10,2,5,40,80,88,40,49,84,18,58,74,92,52,36,23,63,91,31,96,74,84,89,31,10,85,3,92,47,59,53,63,23,22,36,39,16,98,63,16,89,76,62,25,60,34,40,40,79,59,87,24,20,71,97,96,36,29,80,30,21,61,52,92,76,98,72,7,65,34,28,4,75,1,45,67,87,22,2,69,22,83,9,58,94,76,69,9,7,12,59,42,73,87,92,68,48,89,97,55,27,66,92,26,13,44,56,79,35,8,17,22,4,6,69,49,25,65,93,46,73,48,78,35,82,69,80,71,62,96,18,15,57,28,34,91,65,41,23,88,24,15,3,75,95,29,39,68,67,87,59,66,19,67,77,42,65,34,2,62,20,4,38,47,52,78,47,61,94,30,43,61,21,40,82,16,32,18,46,75,57,63,85,54,15,55,44,56,73,21,60,30,77,76,87,13,7,3,87,77,15,51,81,15,1,45,79,72,71,96,62,8,53,78,45,94,39,45,8,22,21,39,20,60,2,20,12,48,54,21,30,19,95,86,12,52,15,59,29,43,46,19,9,13,3,68,80,60,70,67,90,54,36,20,65,10,75,51,27,86,37,92,5,69,54,94,50,67,24,72,2,25,58,56,83,5,82,88,98,82,2,1,15,21,34,61,86,97,71,69,65,6,70,69,91,67,50,8,70,71,15,40,17,33,55,5,97,60,5,23,49,59,38,40,86,21,23,54,41,75,15,86,84,57,24,53,58,47,92,66,71,29,83,85,25,37,66,1,78,87,61,69,25,91,9,3,3,9,27,76,36,17,37,62,76,98,84,88,24,78,61,72,41,33,20,54,14,72,2,81,95,81,53,8,69,47,53,82,52,54,59,12,87,15,42,58,33,94,79,3,38,5,30,5,23,96,33,10,30,41,58,80,25,31,50,31,44,65,63,54,9,54,14,20,52,9,23,62,67,26,35,44,9,57,95,11,18,65,92,2,58,12,88,53,34,69,37,87,46,34,9,69,57,15,30,26,75,72,55,42,29,7,79,82,91,81,59,51,25,81,9,85,82,46,17,37,76,71,78,40,65,30,57,33,97,71,97,95,36,36,70,7,65,67,53,20,18,30,93,26,62,49,71,86,84,70,85,14,55,36,67,97,64,12,58,14,38,51,55,89,85,23,30,97,41,51,7,75,1,78,61,39,44,7,41,88,20,5,92,30,59,26,44,2,87,16,32,24,42,33,90,46,47,60,75,87,44,21,9,52,20,93,7,54,15,90,50,25,20,4,90,68,41,72,46,81,98,30,49,29,21,44,45,22,12,57,51,53,41,37,94,2,85,59,88,19,19,76,67,45,28,7,40,61,49,1,35,98,19,94,66,73,25,20,91,15,71,86,1,30,25,46,70,83,38,42,78,6,87,77,86,98,76,87,51,69,48,54,41,90,92,95,27,44,77,47,13,70,49,62,18,34,14,51,24,48,52,51,93,37,7,54,69,4,84,23,29,37,9,4,35,44,51,41,32,26,62,90,94,7,42,46,83,77,26,30,75,81,11,88,91,8,68,64,84,52,25,70,95,98,15,49,73,14,15,7,56,52,84,13,72,30,64,49,26,66,11,11,24,43,38,59,37,85,19,74,64,95,56,27,8,52,64,22,70,51,4,48,55,80,78,64,20,73,52,59,29,51,55,98,58,78,32,25,69,30,49,69,36,95,54,18,90,1,94,98,10,36,95,17,49,9,45,11,75,33,30,52,76,68,76,2,95,34,21,83,87,47,15,89,28,23,73,57,64,89,29,69,68,81,80,60,260599"""
