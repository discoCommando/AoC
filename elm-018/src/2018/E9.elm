module E8 exposing (..)

import Char
import Dict exposing (Dict)
import Helpers as H
import List
import List.Extra
import Parser exposing ((|.), (|=), Parser, andThen, keep, keyword, succeed, symbol)
import Regex
import Set exposing (Set)
import String


main =
    H.makeMain []


type alias State =
    { marbles : List Int
    , players : List Int
    , turn : Int
    , currentMarbleIndex : Int
    }


marbleIndex : Int -> List Int -> Int
marbleIndex index marbles =
    index % List.length marbles


insertMarble : State -> State
insertMarble { marbles, players, turn, currentMarbleIndex } =
    case turn % 23 == 0 && turn > 0 of
        False ->
            let
                currentMarble =
                    marbles |> H.at currentMarbleIndex

                ( a, b ) =
                    divideAt2 (marbleIndex (currentMarbleIndex + 1) marbles + 1) marbles []

                newMarbles =
                    a
                        ++ [ turn ]
                        ++ b
            in
            State newMarbles players (turn + 1) (marbleIndex (currentMarbleIndex + 1) marbles + 1)

        True ->
            let
                removedMarble =
                    marbles |> H.at (marbleIndex (currentMarbleIndex - 7) marbles)

                newMarbles =
                    (marbles |> List.take (marbleIndex (currentMarbleIndex - 7) marbles))
                        ++ (marbles |> List.drop (marbleIndex (currentMarbleIndex - 6) marbles))

                currentPlayerIndex =
                    (turn - 1) % List.length players

                newPlayers =
                    players |> H.replaceAt currentPlayerIndex ((+) (removedMarble + turn))
            in
            State newMarbles newPlayers (turn + 1) (marbleIndex (currentMarbleIndex - 7) marbles)



-- stopWhenLastMarble : Int -> State -> State
-- stopWhenLastMarble i state =
--     case (state.marbles |> H.at state.currentMarbleIndex) == i of
--         True ->
--             state
--         False ->
--             stopWhenLastMarble i (insertMarble state)


insertXTimes : Int -> State -> State
insertXTimes i state =
    List.repeat i () |> List.foldl (\_ -> insertMarble) state


highestScore : State -> Int
highestScore state =
    List.foldl max 0 state.players



-- initialState =
--     State [0, 2, 1] (List.repeat 411 0) 3 1
-- res1 = initialState |> insertXTimes 72059 |> highestScore |> Debug.log "answer1"


aa =
    1618 % 23 |> Debug.log "aa"



-- ress = initialState |> insertXTimes 24 |> Debug.log "23"


divideAt : Int -> List a -> ( List a, List a )
divideAt i l =
    case l of
        [] ->
            ( [], [] )

        l_ :: ls ->
            case i of
                0 ->
                    ( [], l )

                _ ->
                    let
                        ( ll, rl ) =
                            divideAt (i - 1) ls
                    in
                    ( l_ :: ll, rl )


divideAt2 : Int -> List a -> List a -> ( List a, List a )
divideAt2 i l ll =
    case l of
        [] ->
            ( List.reverse ll, [] )

        l_ :: rest ->
            case i of
                0 ->
                    ( ll |> List.reverse, l )

                _ ->
                    divideAt2 (i - 1) rest (l_ :: ll)


test1 =
    Debug.log "asd" (divideAt 0 [ 0 ] == divideAt2 0 [ 0 ] [])


test2 =
    Debug.log "asd" (divideAt 1 [ 0, 1 ])


test4 =
    Debug.log "asd" (divideAt 3 [ 0, 2, 1 ] == divideAt2 3 [ 0, 2, 1 ] [])


test3 =
    Debug.log "asd" (divideAt 1 [ 0, 2, 1, 3 ] == divideAt2 1 [ 0, 2, 1, 3 ] [])


type alias Circle =
    { before : List Int
    , current : Int
    , after : List Int
    }


moveClockWise : Int -> Circle -> Circle
moveClockWise i c =
    case i of
        0 ->
            c

        _ ->
            case c.after of
                [] ->
                    case c.before of
                        [] ->
                            c

                        _ ->
                            let
                                revBefore =
                                    List.reverse c.before
                            in
                            { before = [ c.current ]
                            , current = revBefore |> H.at 0
                            , after = revBefore |> List.drop 1
                            }

                a :: rest ->
                    moveClockWise (i - 1)
                        { c
                            | before = c.current :: c.before
                            , current = a
                            , after = rest
                        }


moveAntiClockWise : Int -> Circle -> Circle
moveAntiClockWise i c =
    case i of
        0 ->
            c

        _ ->
            case c.before of
                [] ->
                    case c.after of
                        [] ->
                            c

                        _ ->
                            let
                                revAfter =
                                    List.reverse c.after
                            in
                            moveAntiClockWise (i - 1)
                                { after = [ c.current ]
                                , current = revAfter |> H.at 0
                                , before = revAfter |> List.drop 1
                                }

                a :: rest ->
                    moveAntiClockWise (i - 1)
                        { c
                            | after = c.current :: c.after
                            , current = a
                            , before = rest
                        }


type alias State2 =
    { circle : Circle
    , players : List Int
    , turn : Int
    }


addToCircle :
    Int
    -> Circle
    -> Circle -- adds on current, moves current to before
addToCircle value circle =
    { circle | before = circle.current :: circle.before, current = value }


removeCurrentFromCircle : Circle -> ( Int, Circle )
removeCurrentFromCircle circle =
    case circle.after of
        [] ->
            removeCurrentFromCircle { circle | after = List.reverse circle.before, before = [] }

        x :: xs ->
            ( circle.current, { circle | after = xs, current = x } )


insertMarble2 : State2 -> State2
insertMarble2 state =
    case state.turn % 23 == 0 && state.turn > 0 of
        False ->
            let
                newCircle =
                    state.circle
                        |> moveClockWise 1
                        |> addToCircle state.turn
            in
            { state | circle = newCircle, turn = state.turn + 1 }

        True ->
            let
                ( removedMarble, newCircle ) =
                    state.circle
                        -- |> Debug.log "state before"
                        |> moveAntiClockWise 7
                        |> removeCurrentFromCircle

                -- |> Debug.log "state after"
                currentPlayerIndex =
                    (state.turn - 1) % List.length state.players

                newPlayers =
                    state.players |> H.replaceAt currentPlayerIndex ((+) (removedMarble + state.turn))
            in
            { state | players = newPlayers, circle = newCircle, turn = state.turn + 1 }


initialCircle =
    Circle [] 0 []


insertXTimes2 i state =
    case i of
        0 ->
            state

        _ ->
            insertXTimes2 (i - 1) (insertMarble2 state)


noOfPlayers =
    411


lastMarble =
    72059 * 100


initialState =
    { circle = initialCircle
    , players = List.repeat noOfPlayers 0
    , turn = 1
    }


res1 =
    initialState |> insertXTimes2 lastMarble |> .players |> List.foldl max 0 |> Debug.log "answerbetter"



{-
   [-] (0) index 0
   [1]  0 (1) index 1 -- next 0 next1 1 (take next + 1) (drop next1)
   [2]  0 (2) 1 index 1  -- next 2 next1 1 (take )
   [3]  0  2  1 (3) index 3 == 3 % 4
   [4]  0 (4) 2  1  3   index 1 == 5 % 5
   [5]  0  4  2 (5) 1  3  index 3
   [6]  0  4  2  5  1 (6) 3 index 5
   [7]  0  4  2  5  1  6  3 (7)
   [8]  0 (8) 4  2  5  1  6  3  7
   [9]  0  8  4 (9) 2  5  1  6  3  7
   [1]  0  8  4  9  2(10) 5  1  6  3  7
   [2]  0  8  4  9  2 10  5(11) 1  6  3  7
   [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
   [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
   [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
   [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
   [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
   [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
   [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
   [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
   [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
   [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
   [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
   [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
   [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
   [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

-}
