module E6 exposing (..)

import Char
import Dict
import Helpers as H
import List
import List.Extra
import Parser exposing ((|.), (|=), keep, keyword, succeed, symbol)
import Regex
import String


main =
    H.makeMain []


type X
    = X Int Int


pI1 =
    Parser.succeed X
        |= H.pInt
        |. symbol ", "
        |= H.pInt
        |> Parser.run


type CC
    = N
    | R Int Int
    | RS (List Int) Int


createBoard x y =
    List.repeat (y + 1) (List.repeat (x + 1) N)


pI =
    input
        |> String.lines
        |> List.map (pI1 >> H.uR)
        |> (\ls ->
                let
                    ( minx, maxx, miny, maxy ) =
                        List.foldl getMaxes ( 1000, 0, 1000, 0 ) ls

                    -- maxx = maxx_ - minx
                    -- maxy = maxy_ - miny
                    board =
                        createBoard maxx maxy

                    lsN =
                        ls
                in
                board
                    |> List.indexedMap
                        (\y xs ->
                            xs
                                |> List.indexedMap
                                    (\x v ->
                                        setL x y lsN
                                    )
                        )
                    |> (\boardN ->
                            let
                                infx =
                                    getInfx (Debug.log "s" boardN) |> Debug.log "asd"
                            in
                            List.foldl (\ys d -> List.foldl (gatherMaxes infx) d ys) Dict.empty boardN
                                |> Dict.toList
                                |> List.sortBy H.ts
                                |> List.reverse
                       )
           )
        |> Debug.log "asd"


pi2 =
    input
        |> String.lines
        |> List.map (pI1 >> H.uR)
        |> (\ls ->
                let
                    ( minx, maxx, miny, maxy ) =
                        List.foldl getMaxes ( 1000, 0, 1000, 0 ) ls

                    -- maxx = maxx_ - minx
                    -- maxy = maxy_ - miny
                    board =
                        createBoard maxx maxy

                    lsN =
                        ls
                in
                board
                    |> List.indexedMap
                        (\y xs ->
                            xs
                                |> List.indexedMap
                                    (\x v ->
                                        setL2 x y lsN
                                    )
                        )
                    |> (\boardN ->
                            boardN |> List.concat |> List.filter (\x -> x < 10000) |> List.length
                       )
           )
        |> Debug.log "asd"


id : List (List CC) -> List (List CC)
id =
    identity


getMaxes (X x y) ( a, b, c, d ) =
    ( min a x, max b x, min y c, max y d )


len x y x1 y1 =
    abs (x - x1) + abs (y - y1)


setLH x y ( i, X i1 i2 ) cc =
    let
        newL =
            len x y i1 i2
    in
    case cc of
        N ->
            R i newL

        R idx l ->
            case l < newL of
                True ->
                    R idx l

                False ->
                    case l == newL of
                        True ->
                            RS (idx :: i :: []) l

                        False ->
                            R i newL

        RS idxx l ->
            case l < newL of
                True ->
                    RS idxx l

                False ->
                    case l == newL of
                        True ->
                            RS (i :: idxx) l

                        False ->
                            R i newL


setLH2 x y ( i, X i1 i2 ) cc =
    let
        newL =
            len x y i1 i2
    in
    cc + newL


getInfx b =
    [ b |> H.at 0
    , b |> List.map (H.at 0)
    , b |> List.map (\x -> H.at (List.length x - 1) x)
    , b |> H.at (List.length b - 1)
    ]
        |> List.concat
        |> List.filterMap
            (\x ->
                case x of
                    R i _ ->
                        Just [ ( i, () ) ]

                    RS is _ ->
                        Just <| List.map (\i -> ( i, () )) is

                    -- Nothing
                    _ ->
                        Nothing
            )
        |> List.concat
        |> Dict.fromList


setL : Int -> Int -> List X -> CC
setL x y ls =
    List.foldl (\v ( le, cc ) -> ( le + 1, setLH x y ( le, v ) cc )) ( 0, N ) ls |> H.ts


setL2 : Int -> Int -> List X -> Int
setL2 x y ls =
    List.foldl (\v ( le, cc ) -> ( le + 1, setLH2 x y ( le, v ) cc )) ( 0, 0 ) ls |> H.ts


gatherMaxes : Dict.Dict Int () -> CC -> Dict.Dict Int Int -> Dict.Dict Int Int
gatherMaxes infx cc d =
    case cc of
        R i v ->
            case Dict.get i infx of
                Just _ ->
                    d

                Nothing ->
                    case v of
                        0 ->
                            d |> H.countApp i

                        _ ->
                            d |> H.countApp i

        _ ->
            d


inpute =
    """1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"""


input =
    """227, 133
140, 168
99, 112
318, 95
219, 266
134, 144
306, 301
189, 188
58, 334
337, 117
255, 73
245, 144
102, 257
255, 353
303, 216
141, 167
40, 321
201, 50
60, 188
132, 74
125, 199
176, 307
204, 218
338, 323
276, 278
292, 229
109, 228
85, 305
86, 343
97, 254
182, 151
110, 292
285, 124
43, 223
153, 188
285, 136
334, 203
84, 243
92, 185
330, 223
259, 275
106, 199
183, 205
188, 212
231, 150
158, 95
174, 212
279, 97
172, 131
247, 320"""
