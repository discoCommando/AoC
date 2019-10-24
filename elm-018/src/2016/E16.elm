module E16 exposing (..)

import Array exposing (Array, isEmpty)
import Bitwise
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


input : Int
input =
    "11100010111110100" |> toInt


toInt : String -> Int
toInt string =
    string
        |> String.toList
        |> List.foldl
            (\c n ->
                case c of
                    '1' ->
                        n |> Bitwise.shiftLeftBy 1 |> (+) 1

                    _ ->
                        n |> Bitwise.shiftLeftBy 1
            )
            0


part1Length : Int
part1Length =
    272


length : Int -> Int -> Int
length acc i =
    if i <= 1 then
        acc + 1
    else
        length (acc + 1) (i |> Bitwise.shiftRightBy 1)


test1 =
    input |> length 0 |> Debug.log "length"


step : Int -> Int
step i =
    (i |> Bitwise.shiftLeftBy (length 0 i + 1)) + (i |> Bitwise.complement)


stepUntilLength : Int -> Int
stepUntilLength int =
    let
        l =
            length 0 int

        _ =
            Debug.log (toString int ++ "") l
    in
    if l >= part1Length then
        int |> Bitwise.shiftRightBy (l - part1Length)
    else
        stepUntilLength (step int)


getNewChecksum : Int -> Int -> Int -> Int
getNewChecksum times acc int =
    if times <= 0 then
        acc
    else if int % 2 == 1 then
        getNewChecksum (times - 1) (acc + (1 |> Bitwise.shiftLeftBy (times - 1))) (int |> Bitwise.shiftRightBy 2)
    else
        getNewChecksum (times - 1) acc (int |> Bitwise.shiftRightBy 2)


computeChecksum : Int -> Int
computeChecksum int =
    let
        l =
            length 0 int
    in
    if l % 2 == 1 then
        int
    else
        computeChecksum (Bitwise.xor int (int |> Bitwise.shiftRightBy 1) |> Bitwise.complement |> getNewChecksum (l // 2) 0)


writeBinary : List Char -> Int -> String
writeBinary ls int =
    if int <= 0 then
        ('0' :: ls) |> String.fromList
    else if int == 1 then
        ('1' :: ls) |> String.fromList
    else if int % 2 == 1 then
        writeBinary ('1' :: ls) (int |> Bitwise.shiftRightBy 1)
    else
        writeBinary ('0' :: ls) (int |> Bitwise.shiftRightBy 1)


debugBinary : Int -> Int
debugBinary int =
    let
        _ =
            Debug.log (writeBinary [] int) int
    in
    int


part1 =
    input
        |> debugBinary
        |> stepUntilLength
        |> Debug.log ""



-- |> computeChecksum
-- |> debugBinary
-- |> Debug.log "final "
