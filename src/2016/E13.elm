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


tile : Int -> Int -> Tile
tile x y =
    if (calculation x y |> (+) designerFavouriteNumber |> ones) % 2 == 0 then
        OpenSpace
    else
        Wall


pointToReach =
    ( 31, 39 )
