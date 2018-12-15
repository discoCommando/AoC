module E15 exposing (..)

import Array exposing (Array)
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


type alias DiscState =
    { size : Int
    , position : Int
    }


type BallPosition
    = Waiting Int
    | InDisc Int
    | Finished


type alias State =
    { discs : Array DiscState
    , time : Int
    , ballPosition : BallPosition
    }


lineParser : Parser DiscState
lineParser =
    succeed DiscState
        |. keyword "Disc #"
        |. H.pInt
        |. keyword " has "
        |= H.pInt
        |. keyword " positions; at time=0, it is at position "
        |= H.pInt


parseInput : Int -> String -> State
parseInput waiting input =
    { discs =
        input
            |> String.lines
            |> List.map (Parser.run lineParser >> H.uR)
            |> Array.fromList
    , time = 0
    , ballPosition = Waiting waiting
    }


moveAllDiscs : Array DiscState -> Array DiscState
moveAllDiscs discs =
    discs |> Array.map (\discState -> { discState | position = (discState.position + 1) % (discState.size - 1) })



-- step : State -> State
-- step state =
--     let
--         newDiscs = state.discs |> moveAllDiscs
--         newBallPosition =
--             case state.ballPosition of
--                 Waiting _ ->
--                     case H.uG state.discs 0 |> .position of
--                         0 ->
--                             { stateWithNewTime | ballPosition = InDisc 0 }
--
--                         _ ->
--                             stateWithNewTime
--
--                 InDisc position ->
--                     case H.uG state.discs position |> .position of
--                         0 ->
--                             if position == Array.length state.discs then
--                                 Finished
--                             else
--
--
--                 Finished ->
--                     state
--     in
--     { state | }
--


checkDiscs : Int -> Array DiscState -> Bool
checkDiscs i discs =
    case Array.get i discs of
        Nothing ->
            True

        Just state ->
            case (state.position + i) % Array.length discs of
                0 ->
                    checkDiscs (i + 1) discs

                _ ->
                    False


findTime : Int -> Array DiscState -> Int
findTime i discs =
    let
        movedDiscs =
            moveAllDiscs discs
    in
    case checkDiscs 0 movedDiscs of
        True ->
            let
                _ =
                    Debug.log "finishing discs" movedDiscs
            in
            i

        False ->
            findTime (i + 1) movedDiscs


test1 =
    inpute |> parseInput 0 |> .discs |> Debug.log "discs" |> findTime 1 |> Debug.log "time"


input =
    """Disc #1 has 13 positions; at time=0, it is at position 10.
Disc #2 has 17 positions; at time=0, it is at position 15.
Disc #3 has 19 positions; at time=0, it is at position 17.
Disc #4 has 7 positions; at time=0, it is at position 1.
Disc #5 has 5 positions; at time=0, it is at position 0.
Disc #6 has 3 positions; at time=0, it is at position 1."""


inpute =
    """Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1."""



-- ball d1 d2
-- 0    4  1
-- 1    0  0
-- 2    1  1
-- 3    2  0
-- 4    3  1
-- 5    4  0
-- 6    0  1
-- 7    1  0
--
-- x + 4 % 5  == 0
-- 2x + 8 % 10 == 0
-- 5x + 5 % 10  == 0
-- 7x + 8 % 10 == 0
-- 0 8 -> 8
-- 1 15 -> 5
-- 2 22 -> 2
-- 3 29 -> 9
-- 4 36 -> 6
-- 5 43 -> 3
-- 6 50 -> 0
--
-- -- 7x + 13 % 10 == 0
--
-- 0 13 -> 3
-- 1 20 -> 0
--
--
-- -- x + 3 % 5 == 0
-- -- x + 1 % 2 == 0
-- --
-- -- 2x + 6
-- -- 5x + 5
-- -- 7x + 11 % 10
-- --
-- -- 0 11 -> 1
-- -- 1 18 -> 8
-- -- 2 25 -> 5 ...
-- --
-- -- 3x - 1
-- -- 0 -1 -> 9
-- -- 1 2 -> 2
-- -- 2 5 -> 5
-- -- 3 8 -> 8
-- -- 4 11 -> 1
-- -- 5 14 -> 4
-- -- 6 17 -> 7
-- -- 7 20 -> 0
--
-- -- x + 4 % 5 == x % 2
-- -- 3x - 8 % 5 == 0
--
-- x === 4 (mod 5)
-- x === 1 (mod 2) -> (5t + 4) === 1 -> 5t === -3 -> 5t === 1 (mod 2) -> t == -3 -> t === 1
