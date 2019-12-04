module E4 exposing (..)

import Char
import Dict
import Helpers as H
import List
import List.Extra
import Regex
import String


main =
    H.makeMain [ res1 |> toString, res2 |> toString ]


type Action
    = FA
    | WA
    | GBS Int


type alias E =
    { mon : Int
    , day : Int
    , hou : Int
    , min : Int
    , act : Action
    }


parseAction : String -> Action
parseAction s =
    case String.trim s of
        "falls asleep" ->
            FA

        "wakes up" ->
            WA

        _ ->
            GBS (s |> String.trim |> String.split " " |> H.at 1 |> String.toList |> List.drop 1 |> String.fromList |> H.toI)


parseInput : String -> E
parseInput s =
    s
        |> Regex.find Regex.All (Regex.regex "\\[1518-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\] ([a-zA-Z#0-9 ]+)")
        |> H.at 0
        |> (.submatches >> List.map H.uM)
        |> (\r ->
                E (r |> H.at 0 |> H.toI) (r |> H.at 1 |> H.toI) (r |> H.at 2 |> H.toI) (r |> H.at 3 |> H.toI) (r |> H.at 4 |> parseAction)
           )


fillFrom l i x =
    case i of
        0 ->
            List.repeat (List.length l) x

        _ ->
            case l of
                [] ->
                    []

                a :: ass ->
                    a :: fillFrom ass (i - 1) x


addDay mon day =
    (case mon of
        1 ->
            case day of
                31 ->
                    Just ( 2, 1 )

                _ ->
                    Nothing

        2 ->
            case day of
                28 ->
                    Just ( 3, 1 )

                _ ->
                    Nothing

        3 ->
            case day of
                31 ->
                    Just ( 4, 1 )

                _ ->
                    Nothing

        4 ->
            case day of
                30 ->
                    Just ( 5, 1 )

                _ ->
                    Nothing

        5 ->
            case day of
                31 ->
                    Just ( 6, 1 )

                _ ->
                    Nothing

        6 ->
            case day of
                30 ->
                    Just ( 7, 1 )

                _ ->
                    Nothing

        7 ->
            case day of
                31 ->
                    Just ( 8, 1 )

                _ ->
                    Nothing

        8 ->
            case day of
                31 ->
                    Just ( 8, 1 )

                _ ->
                    Nothing

        9 ->
            case day of
                30 ->
                    Just ( 10, 1 )

                _ ->
                    Nothing

        10 ->
            case day of
                31 ->
                    Just ( 11, 1 )

                _ ->
                    Nothing

        11 ->
            case day of
                30 ->
                    Just ( 12, 1 )

                _ ->
                    Nothing

        12 ->
            case day of
                31 ->
                    Just ( 1, 1 )

                _ ->
                    Nothing

        _ ->
            Nothing
    )
        |> Maybe.withDefault ( mon, day + 1 )


fld1 : E -> ( Maybe Int, Dict.Dict ( Int, Int ) { g : Int, minutes : List Bool } ) -> ( Maybe Int, Dict.Dict ( Int, Int ) { g : Int, minutes : List Bool } )
fld1 e ( mi, d ) =
    let
        monDay =
            case e.hou of
                23 ->
                    addDay e.mon e.day

                _ ->
                    ( e.mon, e.day )

        mmin =
            case e.hou of
                23 ->
                    0

                _ ->
                    e.min

        newE =
            case mi of
                Just i ->
                    Just { g = i, minutes = List.repeat 56 False }

                Nothing ->
                    Nothing
    in
    case e.act of
        GBS i ->
            ( Just i, d |> Dict.insert monDay { g = i, minutes = List.repeat 56 False } )

        WA ->
            ( mi
            , d
                |> Dict.update monDay
                    (\x ->
                        case x of
                            Nothing ->
                                case newE of
                                    Nothing ->
                                        Nothing

                                    Just ee ->
                                        ee |> Just

                            Just s ->
                                { s | minutes = fillFrom s.minutes mmin False } |> Just
                    )
            )

        FA ->
            ( mi
            , d
                |> Dict.update monDay
                    (\x ->
                        case x of
                            Nothing ->
                                case newE of
                                    Nothing ->
                                        Nothing

                                    Just ee ->
                                        { ee | minutes = fillFrom ee.minutes mmin True } |> Just

                            Just s ->
                                { s | minutes = fillFrom s.minutes mmin True } |> Just
                    )
            )


fld2 : ( ( Int, Int ), { g : Int, minutes : List Bool } ) -> Dict.Dict Int (List Int) -> Dict.Dict Int (List Int)
fld2 ( ( mon, day ), { g, minutes } ) =
    Dict.update g
        (\e ->
            case e of
                Nothing ->
                    minutes
                        |> List.map
                            (\x ->
                                case x of
                                    True ->
                                        1

                                    False ->
                                        0
                            )
                        |> Just

                Just asleeps ->
                    minutes
                        |> List.map
                            (\x ->
                                case x of
                                    True ->
                                        1

                                    False ->
                                        0
                            )
                        |> List.Extra.zip asleeps
                        |> List.map (\( i1, i2 ) -> i1 + i2)
                        |> Just
        )


getMin v ( ci, mi, mv ) =
    case v > mv of
        True ->
            ( ci + 1, ci, v )

        False ->
            ( ci + 1, mi, mv )


max ls =
    case ls of
        [] ->
            0

        l :: lls ->
            let
                m =
                    max lls
            in
            case l > m of
                True ->
                    l

                False ->
                    m


res1 =
    input
        |> String.lines
        |> List.map parseInput
        |> List.sortBy
            (\e ->
                [ e.mon * 1000000
                , e.day * 1000
                , e.hou * 10
                , e.min
                ]
                    |> List.sum
            )
        |> List.foldl fld1 ( Nothing, Dict.empty )
        |> H.ts
        |> Dict.toList
        |> List.foldl fld2 Dict.empty
        |> Dict.toList
        |> List.sortBy (\( _, x ) -> x |> List.sum)
        |> List.reverse
        |> H.at 0
        |> (\( id, mins ) -> mins |> List.foldl getMin ( 0, 0, -1 ) |> (\( _, mi, _ ) -> mi * id))


res2 =
    input
        |> String.lines
        |> List.map parseInput
        |> List.sortBy
            (\e ->
                [ e.mon * 1000000
                , e.day * 1000
                , e.hou * 10
                , e.min
                ]
                    |> List.sum
            )
        |> List.foldl fld1 ( Nothing, Dict.empty )
        |> H.ts
        |> Dict.toList
        |> List.foldl fld2 Dict.empty
        |> Dict.toList
        |> List.sortBy (\( _, x ) -> x |> max)
        |> List.reverse
        |> H.at 0
        |> (\( id, mins ) -> mins |> List.foldl getMin ( 0, 0, -1 ) |> (\( _, mi, _ ) -> mi * id))


inpute =
    """[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"""


input =
    """[1518-09-20 00:43] falls asleep
[1518-09-20 00:56] wakes up
[1518-04-06 00:34] falls asleep
[1518-11-03 00:33] wakes up
[1518-03-16 00:08] falls asleep
[1518-09-12 00:46] wakes up
[1518-05-11 00:48] wakes up
[1518-04-22 00:59] wakes up
[1518-08-05 00:59] wakes up
[1518-07-25 00:24] falls asleep
[1518-04-11 00:15] wakes up
[1518-05-24 00:59] wakes up
[1518-10-08 00:16] falls asleep
[1518-07-20 00:36] wakes up
[1518-09-23 00:39] wakes up
[1518-07-17 00:55] wakes up
[1518-08-19 00:52] wakes up
[1518-10-29 00:09] falls asleep
[1518-05-14 23:58] Guard #1559 begins shift
[1518-04-13 00:39] falls asleep
[1518-05-19 23:57] Guard #2749 begins shift
[1518-02-14 00:45] wakes up
[1518-08-05 23:51] Guard #3323 begins shift
[1518-06-26 00:04] Guard #1093 begins shift
[1518-02-22 00:33] wakes up
[1518-07-20 00:03] Guard #811 begins shift
[1518-10-07 00:15] wakes up
[1518-07-31 00:23] falls asleep
[1518-09-22 00:17] falls asleep
[1518-09-03 00:43] wakes up
[1518-05-28 00:47] falls asleep
[1518-07-08 00:03] Guard #1093 begins shift
[1518-05-02 00:02] falls asleep
[1518-04-17 00:54] wakes up
[1518-09-18 23:48] Guard #1327 begins shift
[1518-07-31 00:58] wakes up
[1518-09-21 00:00] Guard #3491 begins shift
[1518-10-08 00:29] wakes up
[1518-05-16 23:57] Guard #3491 begins shift
[1518-04-23 00:56] wakes up
[1518-08-29 23:57] Guard #2221 begins shift
[1518-05-03 00:45] falls asleep
[1518-03-12 00:03] Guard #3137 begins shift
[1518-10-28 00:03] Guard #1637 begins shift
[1518-07-14 00:51] falls asleep
[1518-04-16 23:56] Guard #1637 begins shift
[1518-03-04 00:33] wakes up
[1518-06-22 00:54] falls asleep
[1518-09-27 00:40] falls asleep
[1518-07-12 00:00] Guard #1637 begins shift
[1518-05-01 00:00] Guard #2399 begins shift
[1518-02-10 00:52] wakes up
[1518-09-16 00:04] Guard #3137 begins shift
[1518-08-02 00:00] Guard #3547 begins shift
[1518-02-15 00:15] wakes up
[1518-07-28 00:03] falls asleep
[1518-06-20 00:56] wakes up
[1518-06-17 00:40] wakes up
[1518-07-25 00:53] wakes up
[1518-02-28 00:19] falls asleep
[1518-03-19 23:51] Guard #2161 begins shift
[1518-03-26 00:52] wakes up
[1518-09-07 00:04] Guard #113 begins shift
[1518-10-30 00:20] wakes up
[1518-05-09 00:02] Guard #3137 begins shift
[1518-11-19 00:22] wakes up
[1518-10-30 00:40] wakes up
[1518-03-14 00:04] falls asleep
[1518-03-10 23:59] Guard #1993 begins shift
[1518-11-05 00:58] wakes up
[1518-07-27 00:37] falls asleep
[1518-05-03 23:57] Guard #1709 begins shift
[1518-08-17 00:48] wakes up
[1518-04-09 00:16] falls asleep
[1518-07-11 00:48] wakes up
[1518-05-06 00:08] falls asleep
[1518-04-21 00:56] falls asleep
[1518-03-23 00:08] falls asleep
[1518-03-28 00:10] falls asleep
[1518-06-30 00:50] wakes up
[1518-07-11 00:38] falls asleep
[1518-02-20 00:59] wakes up
[1518-02-12 00:53] falls asleep
[1518-08-11 00:30] falls asleep
[1518-10-12 00:49] wakes up
[1518-06-22 00:32] wakes up
[1518-04-14 00:01] Guard #229 begins shift
[1518-05-06 00:31] wakes up
[1518-03-05 00:47] falls asleep
[1518-05-19 00:37] wakes up
[1518-10-14 00:36] falls asleep
[1518-06-18 00:46] falls asleep
[1518-03-24 00:18] falls asleep
[1518-10-14 00:49] wakes up
[1518-05-16 00:01] falls asleep
[1518-06-05 00:40] wakes up
[1518-11-19 00:58] wakes up
[1518-03-01 00:30] falls asleep
[1518-03-03 00:26] falls asleep
[1518-06-22 00:21] falls asleep
[1518-10-21 00:00] Guard #2713 begins shift
[1518-05-07 00:04] Guard #3323 begins shift
[1518-08-15 00:29] wakes up
[1518-03-31 23:58] Guard #2749 begins shift
[1518-06-16 23:50] Guard #3323 begins shift
[1518-05-09 00:25] falls asleep
[1518-04-11 00:56] wakes up
[1518-07-05 00:50] falls asleep
[1518-03-29 00:09] falls asleep
[1518-03-18 00:35] wakes up
[1518-04-25 00:21] falls asleep
[1518-06-27 00:46] falls asleep
[1518-06-07 23:51] Guard #311 begins shift
[1518-11-12 00:04] Guard #3137 begins shift
[1518-03-17 00:55] wakes up
[1518-05-10 00:03] falls asleep
[1518-07-27 00:47] wakes up
[1518-06-28 00:41] wakes up
[1518-05-27 00:59] wakes up
[1518-03-25 00:29] falls asleep
[1518-10-01 00:00] Guard #1709 begins shift
[1518-02-24 00:04] falls asleep
[1518-04-29 00:08] falls asleep
[1518-04-22 00:02] Guard #2713 begins shift
[1518-11-15 23:59] Guard #3137 begins shift
[1518-07-26 00:04] Guard #2221 begins shift
[1518-10-08 00:01] Guard #2749 begins shift
[1518-03-15 00:56] wakes up
[1518-05-16 00:41] wakes up
[1518-08-14 00:43] wakes up
[1518-07-15 00:51] wakes up
[1518-09-13 00:09] falls asleep
[1518-07-08 00:12] falls asleep
[1518-05-27 00:18] falls asleep
[1518-04-16 00:04] Guard #1993 begins shift
[1518-03-27 00:55] wakes up
[1518-05-30 00:18] wakes up
[1518-10-25 00:06] falls asleep
[1518-10-10 23:56] Guard #113 begins shift
[1518-04-28 00:02] Guard #1637 begins shift
[1518-04-25 00:04] Guard #3547 begins shift
[1518-02-23 00:12] falls asleep
[1518-07-07 00:15] wakes up
[1518-03-28 00:40] wakes up
[1518-11-09 00:48] wakes up
[1518-11-23 00:03] Guard #2399 begins shift
[1518-10-17 00:38] falls asleep
[1518-09-15 00:44] wakes up
[1518-02-20 00:40] wakes up
[1518-11-19 23:46] Guard #113 begins shift
[1518-03-14 00:23] wakes up
[1518-06-23 23:52] Guard #2399 begins shift
[1518-03-07 00:58] wakes up
[1518-04-29 23:49] Guard #311 begins shift
[1518-06-25 00:43] wakes up
[1518-05-21 00:00] Guard #1613 begins shift
[1518-11-20 00:01] falls asleep
[1518-10-28 00:21] falls asleep
[1518-10-04 00:34] wakes up
[1518-07-01 00:45] wakes up
[1518-06-01 00:54] wakes up
[1518-02-27 00:41] wakes up
[1518-07-10 00:54] wakes up
[1518-05-18 00:37] falls asleep
[1518-10-14 00:18] wakes up
[1518-03-04 00:26] wakes up
[1518-06-09 00:44] falls asleep
[1518-08-22 00:28] falls asleep
[1518-07-21 00:01] Guard #2749 begins shift
[1518-09-17 00:54] wakes up
[1518-03-04 00:59] wakes up
[1518-04-11 00:02] falls asleep
[1518-04-17 23:58] Guard #1327 begins shift
[1518-06-11 00:31] wakes up
[1518-05-24 00:34] wakes up
[1518-04-02 23:46] Guard #2221 begins shift
[1518-08-25 23:59] Guard #229 begins shift
[1518-09-10 00:50] falls asleep
[1518-09-11 00:44] falls asleep
[1518-11-13 00:50] wakes up
[1518-10-31 00:42] falls asleep
[1518-03-31 00:24] falls asleep
[1518-03-19 00:53] falls asleep
[1518-04-04 00:27] falls asleep
[1518-09-28 00:16] wakes up
[1518-07-01 00:51] falls asleep
[1518-11-05 00:18] falls asleep
[1518-03-13 00:59] wakes up
[1518-08-21 00:56] falls asleep
[1518-11-09 00:51] falls asleep
[1518-02-20 00:56] falls asleep
[1518-06-26 23:56] Guard #2399 begins shift
[1518-10-29 00:00] Guard #1093 begins shift
[1518-03-03 00:54] wakes up
[1518-06-21 00:00] Guard #3323 begins shift
[1518-08-28 00:47] falls asleep
[1518-11-01 23:58] Guard #1993 begins shift
[1518-08-19 23:58] Guard #1993 begins shift
[1518-08-04 00:39] falls asleep
[1518-03-05 00:50] wakes up
[1518-04-11 00:21] falls asleep
[1518-05-11 23:52] Guard #2749 begins shift
[1518-03-21 23:56] Guard #1327 begins shift
[1518-02-11 00:55] wakes up
[1518-03-29 23:47] Guard #1993 begins shift
[1518-10-18 00:51] falls asleep
[1518-02-11 00:42] falls asleep
[1518-08-09 00:49] wakes up
[1518-02-23 00:48] wakes up
[1518-11-09 00:56] wakes up
[1518-04-15 00:28] wakes up
[1518-08-25 00:50] falls asleep
[1518-10-09 00:03] Guard #2713 begins shift
[1518-11-20 00:46] wakes up
[1518-10-26 00:53] wakes up
[1518-04-24 00:44] wakes up
[1518-07-02 00:33] wakes up
[1518-09-29 00:13] falls asleep
[1518-02-24 00:41] wakes up
[1518-07-05 00:55] wakes up
[1518-09-19 00:52] falls asleep
[1518-09-01 00:03] Guard #311 begins shift
[1518-08-11 00:57] falls asleep
[1518-06-12 00:10] falls asleep
[1518-07-13 00:35] falls asleep
[1518-03-06 00:02] Guard #3323 begins shift
[1518-05-28 00:55] wakes up
[1518-06-04 00:59] wakes up
[1518-06-11 00:00] Guard #2399 begins shift
[1518-08-30 00:27] wakes up
[1518-08-29 00:31] falls asleep
[1518-08-21 00:57] wakes up
[1518-06-12 00:46] falls asleep
[1518-11-20 00:38] falls asleep
[1518-09-21 00:35] falls asleep
[1518-02-25 00:37] wakes up
[1518-03-15 00:24] falls asleep
[1518-08-18 00:54] falls asleep
[1518-06-12 23:58] Guard #1093 begins shift
[1518-02-22 00:30] falls asleep
[1518-02-13 00:32] falls asleep
[1518-03-16 00:54] wakes up
[1518-08-24 00:29] falls asleep
[1518-03-31 00:46] wakes up
[1518-10-25 00:56] falls asleep
[1518-03-01 00:37] wakes up
[1518-04-14 00:41] falls asleep
[1518-07-13 23:58] Guard #1093 begins shift
[1518-04-29 00:53] wakes up
[1518-04-07 23:56] Guard #2749 begins shift
[1518-04-21 00:27] wakes up
[1518-05-04 00:26] falls asleep
[1518-07-25 00:37] wakes up
[1518-05-29 00:00] Guard #1093 begins shift
[1518-07-30 00:44] falls asleep
[1518-04-27 00:42] wakes up
[1518-09-09 00:16] falls asleep
[1518-09-15 00:02] falls asleep
[1518-09-19 00:57] wakes up
[1518-07-06 00:04] Guard #3491 begins shift
[1518-04-28 00:58] wakes up
[1518-08-05 00:10] wakes up
[1518-06-13 00:59] wakes up
[1518-08-07 00:22] wakes up
[1518-09-30 00:44] falls asleep
[1518-03-03 00:59] wakes up
[1518-04-22 23:59] Guard #3137 begins shift
[1518-02-11 00:25] falls asleep
[1518-09-14 00:41] wakes up
[1518-11-09 00:37] wakes up
[1518-10-03 00:19] falls asleep
[1518-06-20 00:00] Guard #1709 begins shift
[1518-11-13 00:31] falls asleep
[1518-07-14 00:43] wakes up
[1518-03-01 00:47] falls asleep
[1518-07-15 00:57] falls asleep
[1518-08-19 00:00] Guard #2749 begins shift
[1518-03-03 00:50] falls asleep
[1518-03-30 00:04] falls asleep
[1518-11-08 00:57] falls asleep
[1518-03-31 00:39] falls asleep
[1518-11-06 00:53] wakes up
[1518-02-17 00:40] wakes up
[1518-05-31 00:05] falls asleep
[1518-10-31 00:48] wakes up
[1518-10-02 00:50] falls asleep
[1518-02-10 00:23] wakes up
[1518-07-18 00:55] wakes up
[1518-06-22 00:29] falls asleep
[1518-03-29 00:43] wakes up
[1518-04-09 00:59] wakes up
[1518-11-12 23:51] Guard #113 begins shift
[1518-07-02 00:54] wakes up
[1518-06-09 00:04] Guard #2221 begins shift
[1518-06-21 00:34] wakes up
[1518-03-07 00:52] falls asleep
[1518-11-08 00:53] wakes up
[1518-07-10 00:48] falls asleep
[1518-07-09 00:28] falls asleep
[1518-09-25 00:10] falls asleep
[1518-04-26 00:04] Guard #2383 begins shift
[1518-03-25 00:50] wakes up
[1518-07-15 00:19] wakes up
[1518-10-05 00:02] Guard #1709 begins shift
[1518-08-12 23:59] Guard #2713 begins shift
[1518-10-31 23:57] Guard #2713 begins shift
[1518-06-03 23:57] Guard #2713 begins shift
[1518-09-06 00:03] falls asleep
[1518-11-23 00:42] falls asleep
[1518-03-13 00:53] falls asleep
[1518-05-24 23:59] Guard #2293 begins shift
[1518-03-03 00:45] wakes up
[1518-11-09 00:29] falls asleep
[1518-08-12 00:54] wakes up
[1518-11-13 00:57] falls asleep
[1518-08-16 00:04] falls asleep
[1518-10-02 00:56] wakes up
[1518-10-10 00:36] wakes up
[1518-05-03 00:29] wakes up
[1518-03-17 00:42] wakes up
[1518-05-18 00:41] wakes up
[1518-06-06 23:56] Guard #229 begins shift
[1518-09-28 00:15] falls asleep
[1518-05-07 00:58] wakes up
[1518-06-09 00:12] falls asleep
[1518-08-13 00:51] wakes up
[1518-06-04 00:57] falls asleep
[1518-02-11 23:50] Guard #113 begins shift
[1518-08-01 00:00] Guard #3491 begins shift
[1518-10-17 00:45] wakes up
[1518-05-07 23:49] Guard #113 begins shift
[1518-11-16 00:45] falls asleep
[1518-06-03 00:55] wakes up
[1518-05-05 00:49] falls asleep
[1518-11-17 23:50] Guard #811 begins shift
[1518-07-09 23:56] Guard #1613 begins shift
[1518-07-06 23:58] Guard #1093 begins shift
[1518-05-13 00:18] falls asleep
[1518-07-21 00:12] falls asleep
[1518-10-04 00:02] Guard #3323 begins shift
[1518-04-04 00:04] Guard #229 begins shift
[1518-07-26 00:59] wakes up
[1518-04-05 00:56] wakes up
[1518-04-19 00:59] wakes up
[1518-10-09 23:47] Guard #3323 begins shift
[1518-03-20 00:49] falls asleep
[1518-11-12 00:55] falls asleep
[1518-08-12 00:00] Guard #3137 begins shift
[1518-03-11 00:22] falls asleep
[1518-09-18 00:35] falls asleep
[1518-03-26 00:02] Guard #811 begins shift
[1518-07-04 23:57] Guard #3323 begins shift
[1518-08-03 00:36] wakes up
[1518-07-10 00:39] wakes up
[1518-03-22 00:51] wakes up
[1518-11-12 00:45] wakes up
[1518-10-02 00:46] wakes up
[1518-02-10 00:19] falls asleep
[1518-04-14 00:55] wakes up
[1518-06-11 00:16] falls asleep
[1518-11-21 00:12] wakes up
[1518-08-06 00:14] wakes up
[1518-07-03 23:59] Guard #3547 begins shift
[1518-08-29 00:47] wakes up
[1518-06-25 00:47] falls asleep
[1518-10-15 00:34] falls asleep
[1518-09-08 00:04] Guard #2383 begins shift
[1518-09-28 00:29] wakes up
[1518-02-10 00:01] Guard #1637 begins shift
[1518-08-26 00:47] wakes up
[1518-06-19 00:00] Guard #2293 begins shift
[1518-05-30 00:45] wakes up
[1518-02-13 23:48] Guard #3491 begins shift
[1518-04-18 00:53] falls asleep
[1518-06-12 00:01] Guard #1993 begins shift
[1518-08-05 00:04] falls asleep
[1518-07-27 23:53] Guard #3323 begins shift
[1518-03-26 00:14] falls asleep
[1518-06-05 00:49] falls asleep
[1518-10-25 00:28] wakes up
[1518-10-16 00:04] Guard #3491 begins shift
[1518-04-21 00:03] Guard #1327 begins shift
[1518-08-04 00:54] falls asleep
[1518-09-06 00:36] falls asleep
[1518-11-06 00:02] Guard #2293 begins shift
[1518-05-30 23:52] Guard #2399 begins shift
[1518-06-05 00:02] falls asleep
[1518-10-20 00:45] falls asleep
[1518-05-13 00:20] wakes up
[1518-06-12 00:58] wakes up
[1518-10-31 00:54] falls asleep
[1518-06-17 00:02] falls asleep
[1518-10-01 00:12] falls asleep
[1518-04-08 00:08] falls asleep
[1518-03-31 00:30] wakes up
[1518-09-28 00:25] falls asleep
[1518-07-07 00:11] falls asleep
[1518-02-27 00:02] Guard #2399 begins shift
[1518-09-17 00:02] Guard #1637 begins shift
[1518-11-19 00:13] falls asleep
[1518-07-09 00:03] Guard #2713 begins shift
[1518-02-14 00:37] wakes up
[1518-08-04 00:26] wakes up
[1518-02-17 00:30] falls asleep
[1518-08-30 00:37] falls asleep
[1518-04-22 00:37] falls asleep
[1518-03-21 00:16] falls asleep
[1518-06-30 00:46] falls asleep
[1518-07-30 00:01] falls asleep
[1518-07-09 00:42] wakes up
[1518-07-08 00:42] wakes up
[1518-08-19 00:31] wakes up
[1518-08-13 00:35] falls asleep
[1518-09-04 00:00] falls asleep
[1518-06-22 00:26] wakes up
[1518-04-21 00:57] wakes up
[1518-08-30 00:24] falls asleep
[1518-09-22 00:00] Guard #3323 begins shift
[1518-05-18 00:56] wakes up
[1518-09-20 00:40] wakes up
[1518-08-06 00:19] falls asleep
[1518-08-14 00:03] Guard #2749 begins shift
[1518-03-17 00:23] falls asleep
[1518-09-05 00:51] falls asleep
[1518-07-26 00:54] falls asleep
[1518-03-16 23:47] Guard #2161 begins shift
[1518-11-13 00:03] falls asleep
[1518-02-12 00:00] falls asleep
[1518-11-19 00:52] falls asleep
[1518-10-17 00:00] Guard #1093 begins shift
[1518-09-11 00:59] wakes up
[1518-03-09 00:06] falls asleep
[1518-11-17 00:25] falls asleep
[1518-10-03 00:03] wakes up
[1518-11-13 23:59] Guard #1559 begins shift
[1518-05-31 00:47] wakes up
[1518-06-08 00:05] falls asleep
[1518-10-19 00:18] falls asleep
[1518-10-22 00:29] falls asleep
[1518-07-30 00:36] wakes up
[1518-07-15 00:00] Guard #1709 begins shift
[1518-03-08 00:46] falls asleep
[1518-02-21 00:01] falls asleep
[1518-10-28 00:50] falls asleep
[1518-10-06 00:58] wakes up
[1518-05-16 00:50] wakes up
[1518-11-08 00:59] wakes up
[1518-04-12 00:29] falls asleep
[1518-11-23 00:57] falls asleep
[1518-08-15 00:39] falls asleep
[1518-06-21 00:58] wakes up
[1518-02-14 00:00] falls asleep
[1518-07-12 00:54] falls asleep
[1518-02-16 00:46] wakes up
[1518-08-24 23:51] Guard #3491 begins shift
[1518-10-01 00:23] wakes up
[1518-04-12 00:56] wakes up
[1518-07-03 00:38] falls asleep
[1518-08-16 00:33] wakes up
[1518-07-13 00:58] wakes up
[1518-02-22 00:42] falls asleep
[1518-11-01 00:42] falls asleep
[1518-08-15 00:00] Guard #1327 begins shift
[1518-09-13 00:22] falls asleep
[1518-06-28 00:12] falls asleep
[1518-05-29 00:10] falls asleep
[1518-03-11 00:28] wakes up
[1518-04-09 00:02] Guard #3491 begins shift
[1518-09-22 00:37] wakes up
[1518-04-29 00:03] Guard #1637 begins shift
[1518-11-08 00:00] Guard #2713 begins shift
[1518-03-30 00:29] wakes up
[1518-07-27 00:03] Guard #229 begins shift
[1518-04-10 00:02] Guard #1613 begins shift
[1518-05-15 23:53] Guard #2749 begins shift
[1518-05-24 00:00] Guard #2293 begins shift
[1518-02-20 00:32] falls asleep
[1518-02-11 00:37] wakes up
[1518-02-19 00:35] falls asleep
[1518-03-30 00:55] falls asleep
[1518-05-18 00:49] falls asleep
[1518-04-15 00:10] falls asleep
[1518-09-03 23:52] Guard #3491 begins shift
[1518-03-12 00:06] falls asleep
[1518-06-21 00:15] wakes up
[1518-04-18 00:57] wakes up
[1518-06-15 00:16] falls asleep
[1518-05-10 00:40] falls asleep
[1518-09-11 00:48] wakes up
[1518-06-10 00:20] falls asleep
[1518-07-29 23:53] Guard #113 begins shift
[1518-03-23 00:22] wakes up
[1518-09-29 00:52] falls asleep
[1518-03-16 00:00] Guard #1993 begins shift
[1518-06-19 00:31] falls asleep
[1518-08-24 00:41] wakes up
[1518-11-17 00:13] falls asleep
[1518-07-15 00:46] falls asleep
[1518-06-28 00:50] wakes up
[1518-09-16 00:49] wakes up
[1518-06-25 00:01] Guard #1093 begins shift
[1518-09-01 00:24] falls asleep
[1518-08-28 00:58] wakes up
[1518-09-06 00:20] wakes up
[1518-04-03 00:13] wakes up
[1518-05-03 00:24] falls asleep
[1518-09-23 00:07] wakes up
[1518-07-17 00:16] falls asleep
[1518-09-12 00:31] falls asleep
[1518-05-10 00:58] wakes up
[1518-06-10 00:55] wakes up
[1518-04-27 00:05] falls asleep
[1518-07-28 00:57] wakes up
[1518-02-15 00:01] Guard #3547 begins shift
[1518-08-25 00:56] wakes up
[1518-10-26 00:18] falls asleep
[1518-09-23 00:35] falls asleep
[1518-05-10 00:20] falls asleep
[1518-07-05 00:43] wakes up
[1518-03-18 23:57] Guard #2749 begins shift
[1518-08-04 23:54] Guard #1637 begins shift
[1518-06-09 00:41] wakes up
[1518-03-02 00:23] falls asleep
[1518-03-09 00:59] wakes up
[1518-02-14 00:42] falls asleep
[1518-09-18 00:48] wakes up
[1518-03-26 23:49] Guard #113 begins shift
[1518-05-22 00:49] falls asleep
[1518-05-09 00:49] wakes up
[1518-06-19 00:27] wakes up
[1518-07-02 00:59] wakes up
[1518-08-06 00:01] falls asleep
[1518-06-21 23:56] Guard #1327 begins shift
[1518-05-14 00:02] Guard #2161 begins shift
[1518-10-05 00:40] falls asleep
[1518-06-17 23:56] Guard #2399 begins shift
[1518-05-20 00:58] wakes up
[1518-06-07 00:43] wakes up
[1518-11-15 00:04] Guard #1483 begins shift
[1518-02-23 00:04] Guard #3491 begins shift
[1518-11-21 00:43] falls asleep
[1518-08-21 00:51] wakes up
[1518-08-27 00:58] wakes up
[1518-06-14 00:41] falls asleep
[1518-02-19 23:58] Guard #3137 begins shift
[1518-10-21 00:38] wakes up
[1518-10-24 00:04] Guard #2399 begins shift
[1518-07-02 00:00] Guard #1327 begins shift
[1518-08-31 00:39] falls asleep
[1518-07-23 23:57] Guard #2713 begins shift
[1518-06-23 00:43] wakes up
[1518-03-30 00:42] falls asleep
[1518-10-10 00:40] falls asleep
[1518-04-15 00:00] Guard #1093 begins shift
[1518-07-04 00:15] falls asleep
[1518-07-22 00:07] falls asleep
[1518-06-02 00:02] Guard #2399 begins shift
[1518-06-30 00:23] falls asleep
[1518-11-17 00:47] wakes up
[1518-10-27 00:01] Guard #2161 begins shift
[1518-10-17 23:57] Guard #3323 begins shift
[1518-06-09 00:55] wakes up
[1518-03-30 00:51] wakes up
[1518-10-28 00:53] wakes up
[1518-05-08 00:05] falls asleep
[1518-04-17 00:27] falls asleep
[1518-11-18 00:57] wakes up
[1518-10-13 00:59] wakes up
[1518-07-15 00:14] falls asleep
[1518-06-28 00:14] wakes up
[1518-08-15 00:52] wakes up
[1518-05-01 00:53] wakes up
[1518-09-23 00:03] falls asleep
[1518-05-28 00:35] falls asleep
[1518-10-22 00:54] wakes up
[1518-02-22 00:55] wakes up
[1518-09-19 23:58] Guard #113 begins shift
[1518-05-11 00:00] Guard #2399 begins shift
[1518-10-19 00:51] wakes up
[1518-03-29 00:00] Guard #1709 begins shift
[1518-09-11 00:52] falls asleep
[1518-08-08 00:23] falls asleep
[1518-09-13 00:18] wakes up
[1518-10-07 00:08] falls asleep
[1518-09-29 00:46] wakes up
[1518-06-28 00:46] falls asleep
[1518-11-20 00:52] wakes up
[1518-02-18 00:29] wakes up
[1518-06-12 00:25] wakes up
[1518-09-04 00:53] wakes up
[1518-10-30 00:25] falls asleep
[1518-06-20 00:11] falls asleep
[1518-08-18 00:57] wakes up
[1518-10-25 00:59] wakes up
[1518-08-02 00:58] wakes up
[1518-11-13 00:12] wakes up
[1518-06-02 23:57] Guard #1327 begins shift
[1518-03-06 00:59] wakes up
[1518-07-06 00:24] wakes up
[1518-11-10 23:48] Guard #2749 begins shift
[1518-05-07 00:30] falls asleep
[1518-09-26 00:02] Guard #1993 begins shift
[1518-10-09 00:54] falls asleep
[1518-10-27 00:28] falls asleep
[1518-11-23 00:52] wakes up
[1518-04-16 00:38] falls asleep
[1518-09-05 23:46] Guard #2749 begins shift
[1518-02-25 00:47] falls asleep
[1518-06-06 00:18] falls asleep
[1518-11-07 00:37] falls asleep
[1518-03-30 23:58] Guard #3137 begins shift
[1518-03-08 00:58] wakes up
[1518-08-01 00:58] wakes up
[1518-05-20 00:50] falls asleep
[1518-11-04 00:00] Guard #3323 begins shift
[1518-04-24 00:23] wakes up
[1518-07-30 23:49] Guard #2293 begins shift
[1518-05-04 00:45] wakes up
[1518-07-07 00:49] wakes up
[1518-05-30 00:00] Guard #2749 begins shift
[1518-07-23 00:45] wakes up
[1518-06-27 00:15] falls asleep
[1518-03-24 00:27] wakes up
[1518-04-12 23:58] Guard #2221 begins shift
[1518-04-10 00:57] wakes up
[1518-11-03 00:17] falls asleep
[1518-03-04 00:44] falls asleep
[1518-08-14 00:15] falls asleep
[1518-05-12 00:35] wakes up
[1518-05-24 00:40] falls asleep
[1518-05-05 00:54] wakes up
[1518-07-20 00:51] wakes up
[1518-06-15 00:01] Guard #1327 begins shift
[1518-03-17 00:12] wakes up
[1518-09-11 00:14] falls asleep
[1518-04-10 00:45] wakes up
[1518-11-02 00:48] falls asleep
[1518-04-06 00:04] Guard #3137 begins shift
[1518-08-30 00:43] wakes up
[1518-05-27 23:56] Guard #1613 begins shift
[1518-07-06 00:59] wakes up
[1518-09-30 00:55] wakes up
[1518-07-26 00:40] wakes up
[1518-03-16 00:38] wakes up
[1518-09-22 23:48] Guard #1327 begins shift
[1518-09-27 23:58] Guard #2713 begins shift
[1518-04-30 00:38] falls asleep
[1518-08-10 00:39] falls asleep
[1518-08-04 00:00] Guard #1709 begins shift
[1518-02-19 00:04] falls asleep
[1518-05-19 00:42] falls asleep
[1518-09-26 00:14] falls asleep
[1518-05-29 00:11] wakes up
[1518-11-22 00:19] falls asleep
[1518-04-28 00:40] wakes up
[1518-07-22 23:47] Guard #1709 begins shift
[1518-03-28 00:13] wakes up
[1518-08-09 23:56] Guard #1327 begins shift
[1518-09-07 00:27] falls asleep
[1518-02-19 00:58] wakes up
[1518-09-27 00:47] wakes up
[1518-11-09 00:43] falls asleep
[1518-03-27 00:05] falls asleep
[1518-03-17 00:54] falls asleep
[1518-11-08 00:46] falls asleep
[1518-11-05 00:51] falls asleep
[1518-06-26 00:57] falls asleep
[1518-03-25 00:04] Guard #3547 begins shift
[1518-05-10 00:13] wakes up
[1518-04-26 23:50] Guard #1327 begins shift
[1518-11-11 00:43] wakes up
[1518-06-19 00:55] wakes up
[1518-07-18 23:58] Guard #1613 begins shift
[1518-07-10 00:08] falls asleep
[1518-11-21 23:56] Guard #3137 begins shift
[1518-11-20 00:19] wakes up
[1518-02-23 00:22] wakes up
[1518-09-21 00:46] wakes up
[1518-02-17 23:47] Guard #1093 begins shift
[1518-06-16 00:49] wakes up
[1518-04-25 00:57] wakes up
[1518-04-02 00:53] falls asleep
[1518-03-10 00:00] Guard #2399 begins shift
[1518-05-06 00:45] wakes up
[1518-07-14 00:14] falls asleep
[1518-04-08 00:26] wakes up
[1518-11-21 00:47] wakes up
[1518-09-30 00:00] Guard #1709 begins shift
[1518-05-10 00:37] wakes up
[1518-07-15 00:29] falls asleep
[1518-05-25 23:50] Guard #229 begins shift
[1518-09-19 00:03] falls asleep
[1518-08-23 23:56] Guard #1709 begins shift
[1518-11-10 00:01] Guard #811 begins shift
[1518-11-12 00:34] falls asleep
[1518-09-10 23:58] Guard #3323 begins shift
[1518-05-05 00:04] falls asleep
[1518-07-14 00:52] wakes up
[1518-10-10 00:02] falls asleep
[1518-07-10 23:56] Guard #2713 begins shift
[1518-05-31 23:46] Guard #3323 begins shift
[1518-07-24 00:37] wakes up
[1518-05-22 00:00] Guard #1327 begins shift
[1518-06-08 00:41] wakes up
[1518-08-31 00:44] wakes up
[1518-10-14 23:58] Guard #3547 begins shift
[1518-09-13 23:59] Guard #311 begins shift
[1518-08-03 00:05] falls asleep
[1518-07-12 00:56] wakes up
[1518-02-15 00:13] falls asleep
[1518-02-20 23:54] Guard #3547 begins shift
[1518-05-08 00:53] wakes up
[1518-06-18 00:07] falls asleep
[1518-04-18 23:57] Guard #1613 begins shift
[1518-03-04 00:25] falls asleep
[1518-09-29 00:00] Guard #3491 begins shift
[1518-08-06 00:41] wakes up
[1518-10-12 00:18] falls asleep
[1518-08-07 00:20] falls asleep
[1518-04-05 00:10] falls asleep
[1518-05-30 00:26] falls asleep
[1518-03-13 23:50] Guard #2399 begins shift
[1518-05-12 00:00] falls asleep
[1518-07-26 00:37] falls asleep
[1518-07-01 00:14] falls asleep
[1518-05-09 23:54] Guard #2399 begins shift
[1518-05-19 00:49] falls asleep
[1518-04-20 00:50] falls asleep
[1518-05-14 00:49] wakes up
[1518-09-30 00:34] wakes up
[1518-08-21 23:56] Guard #811 begins shift
[1518-08-27 00:18] falls asleep
[1518-10-11 00:54] wakes up
[1518-11-09 00:00] Guard #2293 begins shift
[1518-11-18 00:00] falls asleep
[1518-06-02 00:53] falls asleep
[1518-08-02 00:22] falls asleep
[1518-02-19 00:22] wakes up
[1518-03-20 23:58] Guard #3547 begins shift
[1518-11-17 00:03] Guard #811 begins shift
[1518-02-12 00:56] wakes up
[1518-10-12 23:53] Guard #311 begins shift
[1518-02-21 00:25] wakes up
[1518-03-21 00:47] wakes up
[1518-04-05 00:02] Guard #3137 begins shift
[1518-09-12 23:57] Guard #1093 begins shift
[1518-09-26 00:46] wakes up
[1518-05-27 00:01] Guard #3491 begins shift
[1518-07-07 00:46] falls asleep
[1518-08-31 00:21] wakes up
[1518-10-31 00:58] wakes up
[1518-04-14 00:13] falls asleep
[1518-04-13 00:55] wakes up
[1518-09-02 23:54] Guard #1613 begins shift
[1518-08-15 00:08] falls asleep
[1518-04-23 23:50] Guard #2221 begins shift
[1518-08-05 00:23] falls asleep
[1518-06-13 00:46] wakes up
[1518-11-20 00:51] falls asleep
[1518-10-27 00:54] wakes up
[1518-07-01 00:53] wakes up
[1518-07-25 00:03] Guard #1993 begins shift
[1518-04-11 23:57] Guard #3323 begins shift
[1518-07-31 00:05] falls asleep
[1518-10-16 00:36] wakes up
[1518-03-15 00:02] Guard #2399 begins shift
[1518-09-19 00:27] wakes up
[1518-08-10 23:56] Guard #2399 begins shift
[1518-08-01 00:09] falls asleep
[1518-04-23 00:11] falls asleep
[1518-10-13 00:29] falls asleep
[1518-10-11 23:56] Guard #113 begins shift
[1518-03-16 00:49] falls asleep
[1518-06-27 00:49] wakes up
[1518-06-27 00:52] falls asleep
[1518-03-19 00:50] wakes up
[1518-10-11 00:32] falls asleep
[1518-10-21 23:57] Guard #2293 begins shift
[1518-03-02 00:00] Guard #1993 begins shift
[1518-11-22 00:45] wakes up
[1518-11-12 00:56] wakes up
[1518-07-20 00:35] falls asleep
[1518-11-16 00:36] falls asleep
[1518-09-28 00:39] falls asleep
[1518-08-16 23:59] Guard #3323 begins shift
[1518-03-30 00:08] wakes up
[1518-02-10 00:47] falls asleep
[1518-10-24 23:56] Guard #1637 begins shift
[1518-11-03 00:44] wakes up
[1518-08-20 00:52] wakes up
[1518-10-12 00:56] wakes up
[1518-10-25 00:34] falls asleep
[1518-09-12 00:00] Guard #3323 begins shift
[1518-10-02 00:15] falls asleep
[1518-05-11 00:12] falls asleep
[1518-03-20 00:53] wakes up
[1518-08-11 00:59] wakes up
[1518-09-06 00:57] wakes up
[1518-04-06 00:45] wakes up
[1518-04-03 00:38] wakes up
[1518-04-29 00:50] falls asleep
[1518-06-15 00:58] wakes up
[1518-02-25 00:04] Guard #811 begins shift
[1518-11-04 23:59] Guard #2161 begins shift
[1518-10-23 00:50] wakes up
[1518-08-11 00:53] wakes up
[1518-02-13 00:53] wakes up
[1518-07-03 00:47] wakes up
[1518-10-24 00:46] wakes up
[1518-05-12 00:52] wakes up
[1518-03-27 23:57] Guard #1327 begins shift
[1518-07-19 00:33] wakes up
[1518-10-14 00:01] falls asleep
[1518-10-15 00:54] wakes up
[1518-05-19 00:50] wakes up
[1518-04-10 23:53] Guard #1327 begins shift
[1518-10-23 00:48] falls asleep
[1518-06-27 00:27] wakes up
[1518-03-28 00:19] falls asleep
[1518-10-02 23:48] Guard #811 begins shift
[1518-03-04 00:29] falls asleep
[1518-10-04 00:32] falls asleep
[1518-04-10 00:43] falls asleep
[1518-07-22 00:38] wakes up
[1518-04-30 00:47] wakes up
[1518-10-02 00:00] Guard #2161 begins shift
[1518-02-18 23:53] Guard #2749 begins shift
[1518-09-24 00:43] wakes up
[1518-02-14 00:19] falls asleep
[1518-05-26 00:51] wakes up
[1518-06-29 00:23] wakes up
[1518-06-29 00:28] falls asleep
[1518-04-21 00:22] falls asleep
[1518-08-10 00:23] falls asleep
[1518-03-22 23:59] Guard #3137 begins shift
[1518-06-02 00:58] wakes up
[1518-06-21 00:45] falls asleep
[1518-06-19 00:16] falls asleep
[1518-09-29 00:55] wakes up
[1518-04-29 00:46] wakes up
[1518-07-05 00:42] falls asleep
[1518-09-05 00:57] wakes up
[1518-04-30 00:29] wakes up
[1518-07-25 00:45] falls asleep
[1518-02-23 00:41] falls asleep
[1518-07-02 00:24] falls asleep
[1518-08-17 23:56] Guard #1093 begins shift
[1518-06-23 00:19] falls asleep
[1518-10-18 23:59] Guard #229 begins shift
[1518-08-25 00:05] falls asleep
[1518-10-21 00:22] falls asleep
[1518-02-27 23:57] Guard #113 begins shift
[1518-08-02 23:53] Guard #3323 begins shift
[1518-07-15 00:42] wakes up
[1518-08-09 00:17] falls asleep
[1518-04-04 00:59] wakes up
[1518-09-10 00:56] wakes up
[1518-08-08 00:42] wakes up
[1518-06-10 00:03] Guard #1327 begins shift
[1518-10-12 00:53] falls asleep
[1518-11-04 00:34] falls asleep
[1518-03-24 00:04] Guard #229 begins shift
[1518-10-13 23:47] Guard #1993 begins shift
[1518-10-31 00:01] Guard #2713 begins shift
[1518-02-13 00:02] Guard #1613 begins shift
[1518-05-21 00:11] falls asleep
[1518-11-11 00:01] falls asleep
[1518-04-24 00:33] falls asleep
[1518-05-12 00:43] falls asleep
[1518-03-03 00:00] Guard #1709 begins shift
[1518-04-20 00:01] Guard #2713 begins shift
[1518-03-07 00:01] Guard #2293 begins shift
[1518-10-09 00:44] wakes up
[1518-05-22 00:17] falls asleep
[1518-06-06 00:57] wakes up
[1518-04-29 00:30] wakes up
[1518-05-25 00:17] falls asleep
[1518-06-03 00:12] falls asleep
[1518-09-03 00:01] falls asleep
[1518-02-12 00:46] wakes up
[1518-02-27 00:22] falls asleep
[1518-04-24 00:04] falls asleep
[1518-06-21 00:08] falls asleep
[1518-06-13 23:58] Guard #2399 begins shift
[1518-06-07 00:25] falls asleep
[1518-08-19 00:20] falls asleep
[1518-04-02 00:03] Guard #2161 begins shift
[1518-07-30 00:51] wakes up
[1518-05-08 00:34] falls asleep
[1518-06-13 00:10] falls asleep
[1518-08-08 00:01] Guard #311 begins shift
[1518-03-18 00:15] falls asleep
[1518-05-19 00:21] falls asleep
[1518-09-14 23:49] Guard #811 begins shift
[1518-10-29 00:55] wakes up
[1518-04-03 00:04] falls asleep
[1518-11-02 23:56] Guard #811 begins shift
[1518-08-30 23:59] Guard #3491 begins shift
[1518-02-14 00:16] wakes up
[1518-03-06 00:50] falls asleep
[1518-08-28 23:57] Guard #3137 begins shift
[1518-10-20 00:01] Guard #1709 begins shift
[1518-11-03 00:36] falls asleep
[1518-11-02 00:49] wakes up
[1518-07-31 00:18] wakes up
[1518-11-10 00:58] wakes up
[1518-07-20 00:47] falls asleep
[1518-09-25 00:22] wakes up
[1518-09-14 00:55] wakes up
[1518-03-01 00:55] wakes up
[1518-07-22 00:02] Guard #1093 begins shift
[1518-11-02 00:45] wakes up
[1518-09-14 00:22] falls asleep
[1518-06-24 00:04] falls asleep
[1518-08-27 23:59] Guard #2713 begins shift
[1518-03-10 00:46] wakes up
[1518-09-09 00:36] wakes up
[1518-06-25 00:58] wakes up
[1518-07-02 00:57] falls asleep
[1518-03-18 00:03] Guard #1637 begins shift
[1518-07-23 00:04] falls asleep
[1518-10-28 00:33] wakes up
[1518-02-18 00:04] falls asleep
[1518-05-24 00:07] falls asleep
[1518-05-13 00:01] Guard #2713 begins shift
[1518-02-16 00:07] falls asleep
[1518-05-01 00:38] falls asleep
[1518-06-22 00:59] wakes up
[1518-06-18 00:27] wakes up
[1518-07-18 00:49] falls asleep
[1518-06-16 00:23] falls asleep
[1518-07-06 00:31] falls asleep
[1518-08-12 00:53] falls asleep
[1518-02-23 23:50] Guard #3137 begins shift
[1518-03-07 23:57] Guard #1613 begins shift
[1518-04-06 23:58] Guard #1483 begins shift
[1518-11-17 00:16] wakes up
[1518-06-28 00:22] falls asleep
[1518-08-23 00:07] falls asleep
[1518-04-01 00:54] wakes up
[1518-03-22 00:07] falls asleep
[1518-07-21 00:47] wakes up
[1518-08-23 00:58] wakes up
[1518-06-30 00:03] Guard #2713 begins shift
[1518-02-26 00:00] Guard #2383 begins shift
[1518-04-03 00:23] falls asleep
[1518-08-15 23:54] Guard #1613 begins shift
[1518-03-17 00:00] falls asleep
[1518-09-13 00:28] wakes up
[1518-07-24 00:13] falls asleep
[1518-02-28 00:58] wakes up
[1518-06-14 00:54] wakes up
[1518-08-17 00:20] falls asleep
[1518-10-03 00:01] falls asleep
[1518-10-23 00:02] Guard #3547 begins shift
[1518-08-26 00:17] falls asleep
[1518-04-30 00:04] falls asleep
[1518-03-30 00:12] falls asleep
[1518-09-02 00:57] wakes up
[1518-08-09 00:03] Guard #1637 begins shift
[1518-08-26 23:56] Guard #3137 begins shift
[1518-04-18 00:23] falls asleep
[1518-07-06 00:22] falls asleep
[1518-05-19 00:00] Guard #3491 begins shift
[1518-05-30 00:11] falls asleep
[1518-04-16 00:59] wakes up
[1518-04-02 00:38] falls asleep
[1518-05-21 00:40] wakes up
[1518-06-29 00:11] falls asleep
[1518-10-07 00:51] wakes up
[1518-11-02 00:12] falls asleep
[1518-06-23 00:02] Guard #3137 begins shift
[1518-06-29 00:51] wakes up
[1518-08-10 00:31] wakes up
[1518-06-13 00:50] falls asleep
[1518-08-22 00:53] wakes up
[1518-06-29 00:00] Guard #811 begins shift
[1518-02-15 23:56] Guard #1993 begins shift
[1518-04-20 00:59] wakes up
[1518-05-03 00:59] wakes up
[1518-09-02 00:02] Guard #311 begins shift
[1518-05-02 00:57] wakes up
[1518-07-16 00:01] Guard #1709 begins shift
[1518-06-01 00:03] falls asleep
[1518-06-10 00:25] wakes up
[1518-09-01 00:46] wakes up
[1518-04-19 00:37] falls asleep
[1518-11-20 23:47] Guard #1993 begins shift
[1518-07-19 00:25] falls asleep
[1518-09-05 00:00] Guard #2399 begins shift
[1518-02-17 00:04] Guard #3323 begins shift
[1518-04-17 00:14] falls asleep
[1518-07-29 00:09] falls asleep
[1518-11-19 00:04] Guard #229 begins shift
[1518-10-07 00:00] Guard #1993 begins shift
[1518-09-24 00:04] Guard #3491 begins shift
[1518-11-07 00:48] wakes up
[1518-07-29 00:49] wakes up
[1518-04-18 00:28] wakes up
[1518-08-04 00:20] falls asleep
[1518-08-04 00:47] wakes up
[1518-08-31 00:09] falls asleep
[1518-05-26 00:03] falls asleep
[1518-03-03 00:57] falls asleep
[1518-04-27 00:47] falls asleep
[1518-05-28 00:40] wakes up
[1518-05-23 00:00] Guard #1559 begins shift
[1518-07-15 00:58] wakes up
[1518-06-10 00:47] falls asleep
[1518-09-14 00:44] falls asleep
[1518-11-10 00:35] falls asleep
[1518-04-02 00:59] wakes up
[1518-10-09 00:59] wakes up
[1518-09-26 23:57] Guard #113 begins shift
[1518-05-05 00:36] wakes up
[1518-09-20 00:26] falls asleep
[1518-08-19 00:40] falls asleep
[1518-10-24 00:07] falls asleep
[1518-07-28 23:58] Guard #1613 begins shift
[1518-06-16 00:00] Guard #113 begins shift
[1518-11-23 00:59] wakes up
[1518-08-20 00:36] falls asleep
[1518-11-16 00:38] wakes up
[1518-06-27 00:56] wakes up
[1518-10-25 00:46] wakes up
[1518-07-18 00:00] Guard #2749 begins shift
[1518-07-12 00:50] wakes up
[1518-10-30 00:10] falls asleep
[1518-08-07 00:44] wakes up
[1518-07-02 00:38] falls asleep
[1518-10-20 00:57] wakes up
[1518-03-12 00:34] wakes up
[1518-03-05 00:04] Guard #2749 begins shift
[1518-11-06 00:23] falls asleep
[1518-06-25 00:29] falls asleep
[1518-06-26 00:58] wakes up
[1518-07-14 00:59] wakes up
[1518-07-16 00:44] wakes up
[1518-11-13 00:58] wakes up
[1518-09-10 00:00] Guard #3323 begins shift
[1518-03-20 00:46] wakes up
[1518-06-05 23:56] Guard #3491 begins shift
[1518-06-30 00:43] wakes up
[1518-06-30 23:57] Guard #2399 begins shift
[1518-07-03 00:00] Guard #1709 begins shift
[1518-06-04 23:47] Guard #2749 begins shift
[1518-07-14 00:56] falls asleep
[1518-08-21 00:43] falls asleep
[1518-04-29 00:38] falls asleep
[1518-10-25 23:59] Guard #3491 begins shift
[1518-09-24 00:14] falls asleep
[1518-03-10 00:22] falls asleep
[1518-10-10 00:44] wakes up
[1518-05-17 00:58] wakes up
[1518-04-02 00:42] wakes up
[1518-02-28 23:58] Guard #229 begins shift
[1518-08-04 00:57] wakes up
[1518-08-25 00:17] wakes up
[1518-04-28 00:34] falls asleep
[1518-05-06 00:37] falls asleep
[1518-08-22 23:56] Guard #811 begins shift
[1518-08-10 00:49] wakes up
[1518-05-01 23:48] Guard #2221 begins shift
[1518-04-10 00:55] falls asleep
[1518-10-06 00:01] falls asleep
[1518-11-01 00:52] wakes up
[1518-09-16 00:34] falls asleep
[1518-11-16 00:58] wakes up
[1518-11-07 00:02] Guard #113 begins shift
[1518-03-02 00:43] wakes up
[1518-05-18 00:02] Guard #2161 begins shift
[1518-02-25 00:17] falls asleep
[1518-03-12 23:57] Guard #1613 begins shift
[1518-09-07 00:42] wakes up
[1518-04-27 00:58] wakes up
[1518-05-16 00:46] falls asleep
[1518-05-22 00:59] wakes up
[1518-10-13 00:01] falls asleep
[1518-08-20 23:57] Guard #2221 begins shift
[1518-05-14 00:44] falls asleep
[1518-10-13 00:25] wakes up
[1518-09-02 00:52] falls asleep
[1518-03-20 00:00] falls asleep
[1518-07-04 00:19] wakes up
[1518-04-14 00:33] wakes up
[1518-07-12 00:07] falls asleep
[1518-10-07 00:37] falls asleep
[1518-10-18 00:58] wakes up
[1518-05-17 00:14] falls asleep
[1518-10-05 23:49] Guard #2221 begins shift
[1518-04-28 00:56] falls asleep
[1518-09-09 00:00] Guard #1327 begins shift
[1518-06-21 00:18] falls asleep
[1518-06-28 00:00] Guard #2749 begins shift
[1518-10-30 00:00] Guard #1613 begins shift
[1518-06-05 00:52] wakes up
[1518-09-28 00:55] wakes up
[1518-05-06 00:03] Guard #2713 begins shift
[1518-03-19 00:31] falls asleep
[1518-06-24 00:57] wakes up
[1518-09-11 00:40] wakes up
[1518-02-21 23:58] Guard #2293 begins shift
[1518-08-07 00:36] falls asleep
[1518-03-09 00:04] Guard #229 begins shift
[1518-09-30 00:22] falls asleep
[1518-05-19 00:43] wakes up
[1518-07-13 00:03] Guard #1709 begins shift
[1518-02-10 23:58] Guard #1637 begins shift
[1518-08-07 00:02] Guard #229 begins shift
[1518-10-05 00:54] wakes up
[1518-07-16 00:39] falls asleep
[1518-07-16 23:58] Guard #2713 begins shift
[1518-05-08 00:10] wakes up
[1518-09-18 00:02] Guard #2713 begins shift
[1518-05-02 23:58] Guard #1327 begins shift
[1518-06-18 00:53] wakes up
[1518-03-04 00:01] Guard #311 begins shift
[1518-11-04 00:42] wakes up
[1518-10-09 00:39] falls asleep
[1518-10-16 00:24] falls asleep
[1518-09-17 00:16] falls asleep
[1518-05-25 00:51] wakes up
[1518-03-30 00:59] wakes up
[1518-11-21 00:00] falls asleep
[1518-05-22 00:30] wakes up
[1518-04-01 00:43] falls asleep
[1518-10-03 00:44] wakes up
[1518-02-25 00:58] wakes up
[1518-11-05 00:25] wakes up
[1518-09-24 23:59] Guard #2221 begins shift
[1518-04-17 00:18] wakes up
[1518-03-19 00:55] wakes up
[1518-05-04 23:50] Guard #3547 begins shift"""