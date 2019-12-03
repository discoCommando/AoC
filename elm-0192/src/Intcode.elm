module Intcode exposing (..)

import Helpers

import Array exposing (Array)

type Action
    = Add
    | Mult
    | Halt


next : Action -> List Action
next a =
    a
        :: (case a of
                Add ->
                    next Mult

                Mult ->
                    next Halt

                Halt ->
                    []
           )


all : List Action
all =
    next Add


code : Action -> Int
code a =
    case a of
        Add ->
            1

        Mult ->
            2

        Halt ->
            99


getAction : Int -> Action
getAction i =
    all
        |> List.map (\x -> ( x, code x ))
        |> List.filter (\( x, c ) -> c == i)
        |> List.head
        |> Helpers.uM
        |> Tuple.first


operation : Action -> Maybe (Int -> Int -> Int)
operation a =
    case a of
        Add ->
            Just (+)

        Mult ->
            Just (*)

        Halt ->
            Nothing


