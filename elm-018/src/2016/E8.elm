module E8 exposing (..)

import Char
import Dict
import Helpers as H
import List
import List.Extra
import Parser exposing ((|.), (|=), keep, keyword, succeed, symbol)
import String


type Cm
    = R Int Int
    | RR Int Int
    | RC Int Int


main =
    H.makeMain [ res1 |> toString, [ 1, 2, 3 ] |> H.replaceAt 1 (always 10) |> toString, rotate 4 [ 1, 2, 3 ] |> toString ]


p1 =
    (Parser.oneOf
        [ succeed R
            |. keyword "rect "
            |= H.pInt
            |. keyword "x"
            |= H.pInt
        , succeed RR
            |. keyword "rotate row y="
            |= H.pInt
            |. keyword " by "
            |= H.pInt
        , succeed RC
            |. keyword "rotate column x="
            |= H.pInt
            |. keyword " by "
            |= H.pInt
        ]
        |> Parser.run
    )
        >> H.uR


inpute =
    """rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4"""


createRectH len l =
    List.repeat len True ++ List.drop len l


createRect h len ls =
    case h of
        0 ->
            ls

        _ ->
            createRect (h - 1) len (H.replaceAt (h - 1) (createRectH len) ls)


action cm ls =
    case cm of
        R w h ->
            createRect h w ls

        RR y i ->
            H.replaceAt y (rotate i) ls

        RC x i ->
            H.replaceAt x (rotate i) (List.Extra.transpose ls) |> List.Extra.transpose


res1 =
    input |> String.lines |> List.map p1 |> List.foldl action (List.repeat 6 (List.repeat 50 False)) |> List.map (List.filter identity >> List.length) |> List.sum


res2 =
    input
        |> String.lines
        |> List.map p1
        |> List.foldl action (List.repeat 6 (List.repeat 50 False))
        |> List.reverse
        |> List.map
            (List.indexedMap
                (\i x ->
                    [ case i % 5 of
                        0 ->
                            "|"

                        _ ->
                            ""
                    , case x of
                        True ->
                            "#"

                        False ->
                            " "
                    ]
                        |> String.join ""
                )
                >> String.join ""
                >> Debug.log "a"
            )


rotate i l =
    let
        i1 =
            i % ll

        ll =
            List.length l
    in
    List.drop (ll - i1) l ++ List.take (ll - i1) l



{-
     0 1 2 3
   0 F F F F
   1 F F F F  ->
   2 F F F F

-}


input =
    """rect 1x1
rotate row y=0 by 7
rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 3
rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 3
rect 1x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 3
rect 2x1
rotate row y=0 by 7
rect 6x1
rotate row y=0 by 3
rect 2x1
rotate row y=0 by 2
rect 1x2
rotate row y=1 by 10
rotate row y=0 by 3
rotate column x=0 by 1
rect 2x1
rotate column x=20 by 1
rotate column x=15 by 1
rotate column x=5 by 1
rotate row y=1 by 5
rotate row y=0 by 2
rect 1x2
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate row y=2 by 15
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate row y=2 by 5
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate row y=2 by 10
rotate row y=0 by 10
rotate column x=8 by 1
rotate column x=5 by 1
rotate column x=0 by 1
rect 9x1
rotate column x=27 by 1
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate column x=42 by 1
rotate column x=40 by 1
rotate column x=22 by 1
rotate column x=17 by 1
rotate column x=12 by 1
rotate column x=7 by 1
rotate column x=2 by 1
rotate row y=3 by 10
rotate row y=2 by 5
rotate row y=1 by 3
rotate row y=0 by 10
rect 1x4
rotate column x=37 by 2
rotate row y=3 by 18
rotate row y=2 by 30
rotate row y=1 by 7
rotate row y=0 by 2
rotate column x=13 by 3
rotate column x=12 by 1
rotate column x=10 by 1
rotate column x=7 by 1
rotate column x=6 by 3
rotate column x=5 by 1
rotate column x=3 by 3
rotate column x=2 by 1
rotate column x=0 by 1
rect 14x1
rotate column x=38 by 3
rotate row y=3 by 12
rotate row y=2 by 10
rotate row y=0 by 10
rotate column x=7 by 1
rotate column x=5 by 1
rotate column x=2 by 1
rotate column x=0 by 1
rect 9x1
rotate row y=4 by 20
rotate row y=3 by 25
rotate row y=2 by 10
rotate row y=0 by 15
rotate column x=12 by 1
rotate column x=10 by 1
rotate column x=8 by 3
rotate column x=7 by 1
rotate column x=5 by 1
rotate column x=3 by 3
rotate column x=2 by 1
rotate column x=0 by 1
rect 14x1
rotate column x=34 by 1
rotate row y=1 by 45
rotate column x=47 by 1
rotate column x=42 by 1
rotate column x=19 by 1
rotate column x=9 by 2
rotate row y=4 by 7
rotate row y=3 by 20
rotate row y=0 by 7
rotate column x=5 by 1
rotate column x=3 by 1
rotate column x=2 by 1
rotate column x=0 by 1
rect 6x1
rotate row y=4 by 8
rotate row y=3 by 5
rotate row y=1 by 5
rotate column x=5 by 1
rotate column x=4 by 1
rotate column x=3 by 2
rotate column x=2 by 1
rotate column x=1 by 3
rotate column x=0 by 1
rect 6x1
rotate column x=36 by 3
rotate column x=25 by 3
rotate column x=18 by 3
rotate column x=11 by 3
rotate column x=3 by 4
rotate row y=4 by 5
rotate row y=3 by 5
rotate row y=2 by 8
rotate row y=1 by 8
rotate row y=0 by 3
rotate column x=3 by 4
rotate column x=0 by 4
rect 4x4
rotate row y=4 by 10
rotate row y=3 by 20
rotate row y=1 by 10
rotate row y=0 by 10
rotate column x=8 by 1
rotate column x=7 by 1
rotate column x=6 by 1
rotate column x=5 by 1
rotate column x=3 by 1
rotate column x=2 by 1
rotate column x=1 by 1
rotate column x=0 by 1
rect 9x1
rotate row y=0 by 40
rotate column x=44 by 1
rotate column x=35 by 5
rotate column x=18 by 5
rotate column x=15 by 3
rotate column x=10 by 5
rotate row y=5 by 15
rotate row y=4 by 10
rotate row y=3 by 40
rotate row y=2 by 20
rotate row y=1 by 45
rotate row y=0 by 35
rotate column x=48 by 1
rotate column x=47 by 5
rotate column x=46 by 5
rotate column x=45 by 1
rotate column x=43 by 1
rotate column x=40 by 1
rotate column x=38 by 2
rotate column x=37 by 3
rotate column x=36 by 2
rotate column x=32 by 2
rotate column x=31 by 2
rotate column x=28 by 1
rotate column x=23 by 3
rotate column x=22 by 3
rotate column x=21 by 5
rotate column x=20 by 1
rotate column x=18 by 1
rotate column x=17 by 3
rotate column x=13 by 1
rotate column x=10 by 1
rotate column x=8 by 1
rotate column x=7 by 5
rotate column x=6 by 5
rotate column x=5 by 1
rotate column x=3 by 5
rotate column x=2 by 5
rotate column x=1 by 5"""
