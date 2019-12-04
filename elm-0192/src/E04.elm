module E04 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Intcode
import Parser exposing (..)
import Set exposing (Set)


main =
    Helpers.makeAppWithMeasurements
        [ Helpers.Computation "part one" (\() -> Debug.toString <| result1 ())
        , Helpers.Computation "part two" (\() -> Debug.toString <| result2 ())
        ]


result1 _ =
    generate correctPartOne |> List.length


result2 _ =
    generate correctPartTwo |> List.length


type alias Number =
    List Int


toIntHelper : Number -> Int
toIntHelper nm =
    case nm of
        [] ->
            0

        i :: rest ->
            toIntHelper rest * 10 + i


toInt : Number -> Int
toInt n =
    n
        |> List.reverse
        |> toIntHelper


codeLength =
    6


notDecreasing : Number -> Bool
notDecreasing n =
    n
        |> List.sort
        |> (==) n


sameDigit : Number -> Bool
sameDigit n =
    let
        set =
            n |> List.foldl Set.insert Set.empty
    in
    List.length n /= Set.size set


notZeroFirst : Number -> Bool
notZeroFirst n =
    n
        |> List.head
        |> Helpers.uM
        |> (/=) 0


correctPartOne : Number -> Bool
correctPartOne num =
    [ notDecreasing, sameDigit, notZeroFirst ]
        |> List.all ((|>) num)


notPartOfBiggerGroup : Number -> Bool
notPartOfBiggerGroup n =
    n
        |> List.foldl (\i -> Dict.update i (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)) Dict.empty
        |> Dict.filter (\i count -> count == 2)
        |> Dict.size
        |> (<=) 1


correctPartTwo : Number -> Bool
correctPartTwo num =
    [ notDecreasing, notPartOfBiggerGroup, notZeroFirst ]
        |> List.all ((|>) num)


digits : List Int
digits =
    [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]


generateHelper : Int -> Int -> List Number
generateHelper index from =
    if index <= 0 then
        [ [] ]

    else
        digits
            |> List.filter (\i -> i >= from)
            |> List.map (\i -> generateHelper (index - 1) i |> List.map ((::) i))
            |> List.concat


generate : (Number -> Bool) -> List Int
generate predicate =
    generateHelper codeLength 0
        |> List.filter predicate
        |> List.map toInt
        |> List.filter (\i -> i >= lowerBoundary && i <= upperBoundary)


lowerBoundary : Int
lowerBoundary =
    --    172930
    0


upperBoundary : Int
upperBoundary =
    --    683082
    999999999999999
