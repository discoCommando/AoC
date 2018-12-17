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
    { number : Int
    , size : Int
    , initialPosition : Int
    }


type BallPosition
    = Waiting Int
    | InDisc Int
    | Finished


type alias State =
    { discs : List DiscState
    , turn : Int
    }


lineParser : Parser DiscState
lineParser =
    succeed DiscState
        |. keyword "Disc #"
        |= H.pInt
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
            |> List.sortBy .size
            |> List.reverse
    , turn = 0
    }



-- moveAllDiscs : Array DiscState -> Array DiscState
-- moveAllDiscs discs =
--     discs |> Array.map (\discState -> { discState | position = (discState.position + 1) % (discState.size - 1) })
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


getZero : Int -> DiscState -> Int
getZero turn discState =
    ((discState.initialPosition + discState.number) % discState.size) + discState.size * turn


checkDiscAt : Int -> DiscState -> Bool
checkDiscAt time discState =
    ((discState.initialPosition + discState.number + time) % discState.size) == 0


checkDiscsAt : Int -> List DiscState -> Bool
checkDiscsAt time discs =
    case discs of
        [] ->
            True

        discState :: rest ->
            if checkDiscAt time discState then
                checkDiscsAt time rest
            else
                False


findTime : State -> State
findTime state =
    let
        _ =
            if state.turn % 100000 == 0 then
                Debug.log (toString state.turn) ()
            else
                ()
    in
    if checkDiscsAt (state.discs |> H.at 0 |> getZero state.turn) state.discs then
        state
    else
        findTime { state | turn = state.turn + 1 }


test1 =
    input
        |> parseInput 0
        |> Debug.log "discs"
        |> findTime
        |> Debug.log "finalState"
        |> (\state -> getZero state.turn (H.at 0 state.discs))
        |> Debug.log "finalTime"


input =
    """Disc #1 has 13 positions; at time=0, it is at position 10.
Disc #2 has 17 positions; at time=0, it is at position 15.
Disc #3 has 19 positions; at time=0, it is at position 17.
Disc #4 has 7 positions; at time=0, it is at position 1.
Disc #5 has 5 positions; at time=0, it is at position 0.
Disc #6 has 3 positions; at time=0, it is at position 1."""



-- x + 10 + 1 % 13 -> x = 13t - 11
-- x + 15 + 2 % 17 -> x = 17s


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
--
-- x + 5 % 5 -> x % 5 -> x = 5t
-- x + 3 % 2 -> x + 1 % 2 -> 5t + 1 % 2 -> t - 3 % 2 -> t + 1 % 2 -> t = 2s - 1
-- x = 10s - 5
--
