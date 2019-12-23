module E15 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Parser exposing (..)
import Set exposing (Set)


main =
    Helpers.makeMain [ Debug.toString result1, Debug.toString result2 ]


result1 =
    testInput2 |> parseInput |> slice 4 |> repeat 4 4


testInput2 =
    "12345678"


basePattern =
    [ 0, 1, 0, -1 ] |> Array.fromList



-- starts from 1


startingIndex =
    1


type alias Pattern =
    { length : Int }


buildPattern : Int -> Pattern
buildPattern i =
    Pattern i


patternLength : Pattern -> Int
patternLength p =
    p.length * 4


getValue : Int -> Pattern -> Int
getValue index pattern =
    let
        newIndex =
            index |> modBy (patternLength pattern)

        pIndex =
            newIndex // pattern.length
    in
    basePattern |> Helpers.uG pIndex


getNextIndex : Int -> Pattern -> Int
getNextIndex index pattern =
    let
        newIndex =
            index |> modBy (patternLength pattern)

        pIndex =
            newIndex // pattern.length
    in
    index - newIndex + (pIndex + 1) * pattern.length - 1


type alias Input =
    Array Int


parseInput : String -> Input
parseInput s =
    s |> String.toList |> List.map (Char.toCode >> (-) (Char.toCode '0') >> (*) -1) |> Array.fromList


getLastDigit : Int -> Int
getLastDigit i =
    i |> abs |> modBy 10


getSum : Bool -> Pattern -> Int -> Int -> Int -> Input -> Int
getSum first pattern offset index sum input =
    if index >= Array.length input then
        sum

    else
        let
            patternValue =
                getValue (index + 1 + offset) pattern

            _ =
                if patternValue == -1 then
                    Debug.log "-1" -1

                else
                    -1
        in
        if patternValue == 0 then
            if first then
                let
                    nextIndex =
                        getNextIndex (index + offset) pattern
                in
                getSum False pattern offset (nextIndex - offset) sum input

            else
                getSum False pattern offset (index + pattern.length) sum input

        else
            getSum False
                pattern
                offset
                (index + pattern.length)
                (sum + (getSum2 index 0 pattern.length 0 input * patternValue))
                input


getSum2 : Int -> Int -> Int -> Int -> Input -> Int
getSum2 from counter to sum input =
    if (from + counter) >= Array.length input then
        sum

    else if counter == to then
        sum

    else
        getSum2 from (counter + 1) to ((sum + Helpers.uG (from + counter) input) |> modBy 10) input


getSumArray : Int -> Input -> Input
getSumArray i input =
    if i == -1 then
        input

    else
        getSumArray (i - 1) (Array.set i (Helpers.uG i input + Helpers.uG (i + 1) input) input)


step : Int -> Int -> Input -> Int
step offset index input =
    let
        _ =
            if modBy 1000 index == 0 then
                Debug.log "step" index

            else
                index

        pattern =
            buildPattern (index + 1 + offset)
    in
    input |> getSum True pattern offset 0 0 |> getLastDigit


phase : Int -> Input -> Input
phase offset input =
    phaseHelper offset 0 input


phaseHelper : Int -> Int -> Input -> Input
phaseHelper offset index input =
    if index >= Array.length input then
        input

    else
        phaseHelper offset (index + 1) (Array.set index (step offset index input) input)


repeat : Int -> Int -> Input -> Input
repeat i offset input =
    let
        _ =
            Debug.log "input" i
    in
    if i == 0 then
        input

    else
        repeat (i - 1) offset (phase offset input)


repeat2 : Int -> Input -> Input
repeat2 i input =
    if i == 0 then
        input

    else
        repeat2 (i - 1) (input |> getSumArray (Array.length input - 2) |> Array.map getLastDigit)


slice : Int -> Input -> Input
slice offset input =
    Array.slice offset (Array.length input) input


inputToInt : Input -> Int
inputToInt input =
    let
        helper : List Int -> Int
        helper l =
            case l of
                [] ->
                    0

                f :: rest ->
                    f + helper rest * 10
    in
    input |> Array.toList |> List.reverse |> helper


get8Digits : Input -> List Int
get8Digits input =
    let
        offset =
            Array.slice 0 7 input |> inputToInt
    in
    input |> Array.toList |> List.drop offset |> List.take 8


testInput =
    "80871224585914546619083218645595"


testInput10 =
    "03036732577212944063491565474664"


dependsOnHelper : Pattern -> Int -> List Int -> Input -> List Int
dependsOnHelper pattern index ll input =
    if index >= Array.length input then
        ll

    else
        let
            patternValue =
                getValue (index + 1) pattern
        in
        if patternValue == 0 then
            if index == 0 then
                dependsOnHelper pattern (index + pattern.length - 1) ll input

            else
                dependsOnHelper pattern (index + pattern.length) ll input

        else
            dependsOnHelper pattern (index + 1) (index :: ll) input


type alias DependsOnTree =
    Dict Int (List Int)


dependsOn : Input -> Int -> DependsOnTree -> DependsOnTree
dependsOn input i dot =
    case dot |> Dict.get i of
        Just l ->
            dot

        Nothing ->
            let
                _ =
                    Debug.log "dependsOn" i

                pattern =
                    buildPattern (i + 1)

                newAcc =
                    dependsOnHelper pattern 0 [] input

                newDot =
                    dot |> Dict.insert i newAcc
            in
            newAcc |> List.foldl (dependsOn input) newDot


legitValues : Input -> Int -> Set Int
legitValues input index =
    dependsOn input index Dict.empty |> Dict.foldl (\k v set -> v |> List.foldl Set.insert set) Set.empty


result2 =
    let
        input =
            newInput_ |> parseInput

        offset =
            getOffset input

        newInput =
            input |> slice offset
    in
    repeat2 100 newInput


newInput_ =
    input_ |> List.repeat 10000 |> String.join ""


getOffset input =
    input |> Array.slice 0 7 |> inputToInt |> Debug.log "inputToInt"



--    legitValues (parseInput "12345678") 0


input_ =
    "59723517898690342336085619027921111260000667417052529433894092649779685557557996383085708903241535436786723718804155370155263736632861535632645335233170435646844328735934063129720822438983948765830873108060969395372667944081201020154126736565212455403582565814037568332106043336657972906297306993727714730061029321153984390658949013821918352341503629705587666779681013358053312990709423156110291835794179056432958537796855287734217125615700199928915524410743382078079059706420865085147514027374485354815106354367548002650415494525590292210440827027951624280115914909910917047084328588833201558964370296841789611989343040407348115608623432403085634084"
