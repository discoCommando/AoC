module E13 exposing (..)

import Bitwise
import Char
import Dict exposing (Dict)
import Helpers as H
import List
import List.Extra
import Parser exposing ((|.), (|=), Parser, keep, keyword, succeed, symbol)
import Regex
import Set exposing (Set)
import String


designerFavouriteNumber : Int
designerFavouriteNumber =
    1364


ones : Int -> Int
ones i =
    case i of
        0 ->
            0

        _ ->
            if i % 2 == 0 then
                ones (Bitwise.shiftRightBy 1 i)
            else
                1 + ones (Bitwise.shiftRightBy 1 i)


isWall : Int -> Bool
isWall i =
    ones i % 2 == 1


test1 =
    ones 3 |> Debug.log "test1"


type Tile
    = Wall
    | OpenSpace


calculation : Int -> Int -> Int
calculation x y =
    x * x + 3 * x + 2 * x * y + y + y * y


tile : ( Int, Int ) -> Tile
tile ( x, y ) =
    if (calculation x y |> (+) designerFavouriteNumber |> ones) % 2 == 0 then
        OpenSpace
    else
        Wall


pointToReach =
    ( 31, 39 )


startingPoint =
    ( 1, 1 )


possiblePoints : ( Int, Int ) -> List ( Int, Int )
possiblePoints ( x, y ) =
    [ ( x + 1, y )
    , ( abs <| x - 1, y )
    , ( x, y + 1 )
    , ( x, abs <| y - 1 )
    ]
        |> List.Extra.unique
        |> List.filter (tile >> (==) OpenSpace)


type alias State =
    Dict ( Int, Int ) Int


shortestPathTo : ( Int, Int ) -> State -> ( State, Int )
shortestPathTo ( x, y ) state =
    possiblePoints ( x, y )
        |> H.foldState shortestPathTo state
        |> (\( newState, values ) ->
                let
                    minimum =
                        List.foldl min 100000 values
                in
                ( newState |> Dict.insert ( x, y ) (minimum + 1), minimum + 1 )
           )


decorate : List ( ( Int, Int ), Int ) -> State -> State
decorate points state =
    case points of
        [] ->
            state

        ( point, length ) :: rest ->
            case state |> Dict.get point of
                Nothing ->
                    if point == pointToReach then
                        state |> Dict.insert point length
                    else
                        decorate (rest ++ (point |> possiblePoints |> List.map (\p -> ( p, length + 1 )))) (state |> Dict.insert point length)

                Just _ ->
                    decorate rest state


try1 =
    decorate [ ( startingPoint, 0 ) ] Dict.empty
        |> Debug.log "state"
        |> Dict.get pointToReach
        |> H.uM
        |> Debug.log "answer"


try2 =
    decorate [ ( startingPoint, 0 ) ] Dict.empty
        |> Dict.filter (\_ len -> len <= 50)
        |> Dict.size
        |> Debug.log "answer2"
