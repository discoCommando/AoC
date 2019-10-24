module E14 exposing (..)

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


inputtest : String
inputtest =
    """"""


type alias State =
    { currents : List Int
    , recipes : Array.Array Int
    }


uG : Int -> Array a -> a
uG i a =
    Array.get i a |> H.uM


newRecipes : List Int -> Array Int
newRecipes recipes =
    List.sum recipes |> toString |> String.toList |> List.map (List.singleton >> String.fromList >> H.toI) |> Array.fromList


initialState : State
initialState =
    { currents = [ 0, 1 ]
    , recipes = [ 3, 7 ] |> Array.fromList
    }


step : State -> State
step state =
    let
        recipesForCurrents =
            state.currents |> List.map (\i -> uG i state.recipes)

        additionalRecipes =
            recipesForCurrents |> newRecipes

        newRecipes_ =
            additionalRecipes |> Array.foldl Array.push state.recipes

        newRecipesLength =
            newRecipes_ |> Array.length

        newIndexes =
            recipesForCurrents |> List.Extra.zip state.currents |> List.map (\( current, recipe ) -> (current + recipe + 1) % newRecipesLength)
    in
    State newIndexes newRecipes_


getTenLasts : Int -> State -> String
getTenLasts i state =
    List.range 0 9 |> List.map (\v -> state.recipes |> uG (v + i % Array.length state.recipes) |> toString) |> String.join ""


stepXTimes : Int -> State -> State
stepXTimes i state =
    case i of
        0 ->
            state

        _ ->
            stepXTimes (i - 1) (step state)


stepUntilLength : Int -> State -> State
stepUntilLength i state =
    if Array.length state.recipes >= i then
        state
    else
        stepUntilLength i (step state)


findSequenceHelper : Int -> List Int -> Array Int -> Bool
findSequenceHelper i ls arr =
    case ls of
        [] ->
            True

        l :: rest ->
            case Array.get i arr of
                Just v ->
                    if v == l then
                        findSequenceHelper (i + 1) rest arr
                    else
                        False

                Nothing ->
                    False


findSequenceHelper1 : Int -> List Int -> Array Int -> Maybe Int
findSequenceHelper1 i ls arr =
    if i + List.length ls - 1 >= Array.length arr then
        Nothing
    else if findSequenceHelper i ls arr then
        Just i
    else
        findSequenceHelper1 (i + 1) ls arr


findSequence : List Int -> State -> Maybe Int
findSequence value state =
    findSequenceHelper1 (Array.length state.recipes - List.length value - 2) value state.recipes


stepUntilSequence : List Int -> State -> Int
stepUntilSequence seq state =
    case findSequence seq state of
        Just i ->
            i

        Nothing ->
            stepUntilSequence seq (step state)


test1 =
    initialState
        |> stepUntilLength (input + 10)
        |> getTenLasts input
        |> Debug.log "answer1"


test2 =
    initialState
        |> (\state -> stepUntilSequence (input |> toString |> String.toList |> List.map (List.singleton >> String.fromList >> H.toI)) state)
        |> Debug.log "answer2"


input : Int
input =
    894501
