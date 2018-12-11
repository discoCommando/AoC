module E11 exposing (..)

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


computePowerLevel : ( Int, Int ) -> Int
computePowerLevel ( x, y ) =
    let
        rackId =
            x + 10
    in
    (((rackId * y) + gridSerialNumber) * rackId) |> getHundreth |> (\v -> v - 5)


getHundreth : Int -> Int
getHundreth int =
    (int // 100)
        |> toString
        |> String.toList
        |> List.reverse
        |> H.at 0
        |> List.singleton
        |> String.fromList
        |> H.toI


gridSerialNumber : Int
gridSerialNumber =
    1718


gridSize =
    3


getGrid : ( Int, Int ) -> Int -> List ( Int, Int )
getGrid ( x, y ) gridSize =
    List.range 0 (gridSize - 1)
        |> List.map (\v -> x + v)
        |> List.map (\newX -> List.range 0 (gridSize - 1) |> List.map (\v -> ( newX, y + v )))
        |> List.concat
        |> List.filter (\( x, y ) -> x <= gridAllSize && y <= gridAllSize)


totalPowerE : ( Int, Int ) -> Int -> Int
totalPowerE point gridSize =
    getGrid point gridSize |> List.map computePowerLevel |> List.sum


gridAllSize =
    300


res1 =
    List.range 1 gridAllSize
        |> List.map (\x -> List.range 1 gridAllSize |> List.map (\y -> ( x, y, totalPowerE ( x, y ) 3 )))
        |> List.concat
        |> List.foldl
            (\( x, y, v ) ( x_, y_, v2 ) ->
                if v > v2 then
                    ( x, y, v )
                else
                    ( x_, y_, v2 )
            )
            ( -1, -1, -10000000 )
        |> Debug.log "result"


type alias State =
    Dict Int (Dict ( Int, Int ) Int)


getV : ( Int, Int ) -> Int -> State -> ( State, Maybe Int )
getV ( x, y ) quantity state =
    let
        ug point q =
            Dict.get q state |> H.uM |> Dict.get point |> H.uM

        ins q v =
            Dict.update q (Maybe.withDefault Dict.empty >> Dict.insert ( x, y ) v >> Just) state
    in
    if x + quantity - 1 <= gridAllSize && y + quantity - 1 <= gridAllSize then
        if quantity <= 2 then
            let
                value =
                    totalPowerE ( x, y ) quantity
            in
            ( ins quantity value, Just value )
        else
            let
                value =
                    ug ( x, y ) (quantity - 1)
                        + ug ( x + 1, y + 1 ) (quantity - 1)
                        - ug ( x + 1, y + 1 ) (quantity - 2)
                        + computePowerLevel ( x + quantity - 1, y )
                        + computePowerLevel ( x, y + quantity - 1 )
            in
            ( ins quantity value, Just value )
    else
        ( state, Nothing )


res22 =
    List.range 1 gridAllSize
        |> List.foldl
            (\quantity ( old, state, v ) ->
                let
                    _ =
                        Debug.log "q" quantity
                in
                List.range 1 (gridAllSize - quantity + 1)
                    |> List.map
                        (\x ->
                            List.range 1 (gridAllSize - quantity + 1)
                                |> List.map
                                    (\y ->
                                        ( x, y )
                                    )
                        )
                    |> List.concat
                    |> List.foldl
                        (\point ( old, state, v ) ->
                            case getV point quantity state of
                                ( newState, Just v2 ) ->
                                    if v2 > v then
                                        ( ( point, quantity ), newState, v2 )
                                    else
                                        ( old, newState, v )

                                _ ->
                                    ( old, state, v )
                        )
                        ( old, state, v )
                    |> (\( n, s, v ) ->
                            if quantity <= 2 then
                                ( n, s, v )
                            else
                                ( n, s |> Dict.remove (quantity - 2), v )
                       )
            )
            ( ( ( -1, -1 ), -1 ), Dict.empty, -10000000 )
        |> Debug.log "res3"


test1 =
    computePowerLevel ( 217, 196 ) |> Debug.log ""
