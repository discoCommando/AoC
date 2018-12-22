module E22 exposing (..)

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


type alias YoloState =
    { erosionLevels : Array (Array Int)
    , lengths : Dict ( ( Int, Int ), Int ) Int
    }


xmult =
    16807


ymult =
    48271


geoModulo =
    20183



-- buildXs : Int -> Int -> Array Int -> Array Int
-- buildXs last i xs =
--     let
--         new = (last * i * xmult)
--     in
--     if i - 1 > Tuple.first target then
--         xs
--     else
--         buildXs new (i + 1) (Array.insert (i - 1) new xs)
-- buildYs : Int -> Int -> Array Int -> Array Int
-- buildYs last i ys =
--     let
--         new = (last * i * ymult)
--     in
--     if i - 1 > Tuple.second target then
--         ys
--     else
--         buildYs new (i + 1) (Array.insert (i - 1) new ys)


getAt : ( Int, Int ) -> Array (Array Int) -> Int
getAt ( x, y ) array =
    array |> Array.get y |> H.uM |> Array.get x |> H.uM


builderosionLevels : Int -> Int -> Array (Array Int) -> Array (Array Int)
builderosionLevels x y erosionLevels =
    if y > Tuple.second target then
        erosionLevels
    else if x > Tuple.first target then
        builderosionLevels 1 (y + 1) erosionLevels
    else if x == Tuple.first target1 && y == Tuple.second target1 then
        let
            xs =
                Array.get y erosionLevels |> H.uM
        in
        builderosionLevels (x + 1) y (erosionLevels |> Array.set y (xs |> Array.set x 0))
    else
        let
            xs =
                Array.get y erosionLevels |> H.uM

            value =
                ((getAt ( x - 1, y ) erosionLevels * getAt ( x, y - 1 ) erosionLevels) |> (+) depth) % geoModulo
        in
        builderosionLevels (x + 1) y (erosionLevels |> Array.set y (xs |> Array.set x value))


buildState : YoloState
buildState =
    { erosionLevels =
        Array.initialize (Tuple.second target + 1)
            (\y ->
                Array.initialize (Tuple.first target + 1)
                    (\x ->
                        if x == 0 then
                            if y == 0 then
                                0
                            else
                                (y * ymult + depth) % geoModulo
                        else if y == 0 then
                            (x * xmult + depth) % geoModulo
                        else
                            0
                    )
            )
            |> builderosionLevels 1 1
    , lengths = Dict.empty
    }


depth : Int
depth =
    510


target1 : ( Int, Int )
target1 =
    ( 10, 10 )


target =
    ( Tuple.first target1 + 1000, Tuple.second target1 + 1000 )


type Type
    = Rocky
    | Narrow
    | Wet


getType : ( Int, Int ) -> YoloState -> Type
getType ( x, y ) state =
    let
        mod =
            (state.erosionLevels |> getAt ( x, y )) % 3
    in
    if mod == 0 then
        Rocky
    else if mod == 1 then
        Wet
    else
        Narrow


answer1 : ( Int, Int ) -> Int -> YoloState -> Int
answer1 ( x, y ) sum state =
    if x > Tuple.first target1 then
        answer1 ( 0, y + 1 ) sum state
    else if y > Tuple.second target1 then
        sum
    else
        let
            newSum =
                case getType ( x, y ) state of
                    Rocky ->
                        sum

                    Wet ->
                        sum + 1

                    Narrow ->
                        sum + 2
        in
        answer1 ( x + 1, y ) newSum state


test1 =
    buildState |> answer1 ( 0, 0 ) 0 |> Debug.log "answer1"


type Tool
    = Neither
    | Torch
    | ClimbingGear


type alias PriorityQueueRow =
    { point : ( Int, Int )
    , minutes : Int
    , tool : Tool
    , terrain : Type
    , lengthFromTarget : Int
    }


type alias PriorityQueue =
    List PriorityQueueRow


rollback : PriorityQueue -> PriorityQueue -> PriorityQueue
rollback acc pq =
    case acc of
        [] ->
            pq

        p :: rest ->
            rollback rest (p :: pq)


insert : PriorityQueueRow -> PriorityQueue -> PriorityQueue -> PriorityQueue
insert priorityQueueRow acc priorityQueue =
    case priorityQueue of
        [] ->
            rollback acc [ priorityQueueRow ]

        p1 :: rest ->
            if p1.minutes + p1.lengthFromTarget < priorityQueueRow.minutes + priorityQueueRow.lengthFromTarget then
                insert priorityQueueRow (p1 :: acc) rest
            else
                rollback acc (priorityQueueRow :: p1 :: rest)


getAdjacents : ( Int, Int ) -> Int -> Tool -> Type -> YoloState -> List PriorityQueueRow
getAdjacents ( x, y ) currentMinutes currentTool currentTerrain state =
    [ ( x - 1, y ), ( x, y - 1 ), ( x + 1, y ), ( x, y + 1 ) ]
        |> List.filter (\p -> Tuple.first p >= 0 && Tuple.second p >= 0)
        |> List.map
            (\p ->
                let
                    entry =
                        { point = p
                        , minutes = currentMinutes + 1
                        , tool = currentTool
                        , terrain = newTerrain
                        , lengthFromTarget = abs (Tuple.first p - Tuple.first target1) + abs (Tuple.second p - Tuple.second target1)
                        }

                    newTerrain =
                        getType p state
                in
                case newTerrain of
                    Rocky ->
                        case currentTool of
                            ClimbingGear ->
                                [ entry ]

                            Torch ->
                                [ entry ]

                            Neither ->
                                case currentTerrain of
                                    Wet ->
                                        [ { entry | minutes = currentMinutes + 8, tool = ClimbingGear } ]

                                    Narrow ->
                                        [ { entry | minutes = currentMinutes + 8, tool = Torch } ]

                                    _ ->
                                        Debug.crash "rocky"

                    Wet ->
                        case currentTool of
                            ClimbingGear ->
                                [ entry ]

                            Neither ->
                                [ entry ]

                            Torch ->
                                case currentTerrain of
                                    Rocky ->
                                        [ { entry | minutes = currentMinutes + 8, tool = ClimbingGear } ]

                                    Narrow ->
                                        [ { entry | minutes = currentMinutes + 8, tool = Neither } ]

                                    _ ->
                                        Debug.crash "wet"

                    Narrow ->
                        case currentTool of
                            Neither ->
                                [ entry ]

                            Torch ->
                                [ entry ]

                            ClimbingGear ->
                                case currentTerrain of
                                    Rocky ->
                                        [ { entry | minutes = currentMinutes + 8, tool = Torch } ]

                                    Wet ->
                                        [ { entry | minutes = currentMinutes + 8, tool = Neither } ]

                                    _ ->
                                        Debug.crash "narrow"
            )
        |> List.concat


toolToInt : Tool -> Int
toolToInt tool =
    case tool of
        Neither ->
            0

        Torch ->
            1

        ClimbingGear ->
            2


bfs : PriorityQueue -> YoloState -> ( Int, YoloState )
bfs priorityQueue state =
    case priorityQueue of
        [] ->
            ( -1, state )

        pr :: rest ->
            if pr.point == target1 then
                case pr.tool of
                    Torch ->
                        let
                            _ =
                                Debug.log "pq" priorityQueue
                        in
                        ( pr.minutes, state )

                    _ ->
                        bfs (rest |> insert { pr | tool = Torch, minutes = pr.minutes + 7 } []) state
            else
                case Dict.get ( pr.point, pr.tool |> toolToInt ) state.lengths of
                    Just m ->
                        if m <= pr.minutes then
                            bfs rest state
                        else
                            let
                                adjacents =
                                    getAdjacents pr.point pr.minutes pr.tool pr.terrain state
                            in
                            bfs (adjacents |> List.foldl (\p -> insert p []) rest) { state | lengths = state.lengths |> Dict.insert ( pr.point, pr.tool |> toolToInt ) pr.minutes }

                    Nothing ->
                        let
                            adjacents =
                                getAdjacents pr.point pr.minutes pr.tool pr.terrain state
                        in
                        bfs (adjacents |> List.foldl (\p -> insert p []) rest) { state | lengths = state.lengths |> Dict.insert ( pr.point, pr.tool |> toolToInt ) pr.minutes }


answer2 =
    buildState
        |> bfs
            [ { minutes = 0
              , tool = Torch
              , point = ( 0, 0 )
              , terrain = buildState |> getType ( 0, 0 )
              , lengthFromTarget = Tuple.first target1 + Tuple.second target1
              }
            ]
        |> Debug.log "state2"



{-
      ....
      ....
      ....
      ...x


      ....
      ....
      ...1
      ..1x

      ....
      ...1
      ..2.
      .1.x

      ...1
      ..3.
      .3..
      1..x


      .631
      6...
      3...
      1..x


      ....
      ....
      ...x
      ....

      ....
      ...1
      ..1x
      ....

      ...1
      ..2.
      .1.x
      ....


      .321
      3...
      1..x
      ....

   mod 5

   4  3  6  7
   7  3  0
   8  0 48
   9  0
-}
