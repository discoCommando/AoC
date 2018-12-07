module E7 exposing (..)

import Char
import Dict
import Helpers as H exposing (iFF)
import List
import List.Extra
import Parser exposing ((|.), (|=), keep, keyword, succeed, symbol)
import Regex
import String


main =
    H.makeMain []


type X
    = X String String


pI =
    Parser.succeed X
        |. symbol "Step "
        |= keep (Parser.Exactly 1) Char.isUpper
        |. symbol " must be finished before step "
        |= keep (Parser.Exactly 1) Char.isUpper
        |> Parser.run


cmp a b =
    let
        f1 =
            a |> String.toList |> H.at 0 |> Char.toCode

        f2 =
            b |> String.toList |> H.at 0 |> Char.toCode
    in
    iFF (f1 < f2) LT (iFF (f1 == f2) EQ GT)


makeF (X a b) f =
    \a_ b_ ->
        iFF (a == a_ && b == b_) LT (f a b)



-- remEx (X a b) (r, d) =
--     case Dict.get a d of
--         Nothing ->
--             case Dict.get
--             (String.join a r "", d |> Dict.insert a ())
--         Just _ ->


gather (X a b) d =
    H.updD a [ b ] ((::) b >> List.sort) d


gather2 (X a b) d =
    H.updD b [ a ] ((::) a >> List.sort) d


isAv c dba =
    case dba |> Dict.get c of
        Nothing ->
            True

        Just [] ->
            True

        _ ->
            False


dfs a dab dba r =
    let
        proceed =
            case dab |> Dict.get a of
                Nothing ->
                    ( r ++ a, dab, dba )

                Just ls ->
                    case ls of
                        [] ->
                            ( r ++ a, dab, dba )

                        xs ->
                            List.foldl
                                (\x ( r_, dab_, dba_ ) ->
                                    dfs x
                                        dab_
                                        (dba_
                                            |> Dict.update x
                                                (\cc ->
                                                    case cc of
                                                        Nothing ->
                                                            Nothing

                                                        Just ccc ->
                                                            Just (ccc |> List.filter ((/=) a))
                                                )
                                        )
                                        r_
                                )
                                ( r ++ a, dab |> Dict.remove a, dba )
                                xs
    in
    case dba |> Dict.get a of
        Nothing ->
            proceed

        Just [] ->
            proceed

        Just xs ->
            ( r, dab, dba )


go2 a dab dba r =
    case dab |> Dict.get a of
        Nothing ->
            ( r ++ a, dab, dba |> Dict.remove a )

        Just ls ->
            case ls of
                [] ->
                    ( r ++ a, dab |> Dict.remove a, dba |> Dict.remove a )

                xs ->
                    ( r ++ a
                    , dab |> Dict.remove a
                    , List.foldl
                        (\x d ->
                            d
                                |> Dict.update x
                                    (\cc ->
                                        case cc of
                                            Nothing ->
                                                Nothing

                                            Just aaa ->
                                                aaa |> List.filter ((/=) a) |> Just
                                    )
                        )
                        (dba |> Dict.remove a)
                        xs
                    )


go3 lst =
    lst |> List.map (\( c, l ) -> ( c, l - 1 )) |> List.partition (H.ts >> (/=) 0)


getInt d1 d2 =
    List.foldl (\x l -> List.filter ((/=) x) l) (d1 |> Dict.keys) (d2 |> Dict.toList |> List.filter (H.ts >> List.isEmpty >> not) |> List.map H.tf)
        ++ (Dict.toList d2 |> List.filter (H.ts >> List.isEmpty) |> List.map H.tf)


dd d1 d2 r =
    let
        int =
            getInt d1 d2

        _ =
            Debug.log "d1" d1

        _ =
            Debug.log "d1" d2
    in
    case int of
        [] ->
            ( d1, d2, r )

        x :: xs ->
            let
                ( r_, d1_, d2_ ) =
                    go2 x d1 d2 r
            in
            dd d1_ d2_ r_


sec a =
    (a |> String.toList |> H.at 0 |> Char.toCode) - ('A' |> Char.toCode) + 61


dd2 d1 d2 workers secs =
    let
        _ =
            Debug.log "workers" ( workers, secs )

        proceed =
            case go3 workers of
                ( notFree, finished ) ->
                    case finished of
                        [] ->
                            case notFree of
                                [] ->
                                    secs

                                _ ->
                                    dd2 d1 d2 notFree (secs + 1)

                        xs ->
                            let
                                ( r_, d1_, d2_ ) =
                                    List.foldl (\x ( r_, d1_, d2_ ) -> go2 x d1_ d2_ r_) ( "", d1, d2 ) (finished |> List.map H.tf) |> Debug.log "newfinished"

                                int_ =
                                    getInt d1_ d2_ |> List.filter (\x -> notFree |> List.map H.tf |> List.member x |> not)

                                newWorkers =
                                    List.take (5 - List.length notFree) int_ |> List.map (\x -> ( x, sec x )) |> (++) notFree
                            in
                            dd2 d1_ d2_ newWorkers (secs + 1)
    in
    proceed


r1 =
    input
        |> String.lines
        |> List.map (pI >> H.uR)
        |> (\vs ->
                let
                    newF =
                        List.foldl makeF cmp vs
                in
                List.sortWith (\(X s1 _) (X s2 _) -> newF s1 s2) vs |> Debug.log "asd"
           )
        |> (\vs -> ( List.foldl gather Dict.empty vs, List.foldl gather2 Dict.empty vs ))
        |> H.log
        |> (\( d1, d2 ) ->
                dd d1 d2 ""
           )
        |> H.log
        |> (\( _, _, a ) -> a)
        |> String.toList
        -- |> List.reverse
        |> List.Extra.unique
        -- |> List.reverse
        |> String.fromList
        |> H.log


r2 =
    input
        |> String.lines
        |> List.map (pI >> H.uR)
        |> (\vs ->
                let
                    newF =
                        List.foldl makeF cmp vs
                in
                List.sortWith (\(X s1 _) (X s2 _) -> newF s1 s2) vs |> Debug.log "asd"
           )
        |> (\vs -> ( List.foldl gather Dict.empty vs, List.foldl gather2 Dict.empty vs ))
        |> H.log
        |> (\( d1, d2 ) ->
                let
                    int =
                        List.foldl (\x l -> List.filter ((/=) x) l) (d1 |> Dict.keys) (d2 |> Dict.toList |> List.filter (H.ts >> List.isEmpty >> not) |> List.map H.tf) |> H.log
                in
                dd2 d1 d2 (List.take 5 int |> List.map (\x -> ( x, sec x ))) 0
           )
        |> H.log


inpute =
    """Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin."""


input =
    """Step J must be finished before step K can begin.
Step N must be finished before step X can begin.
Step S must be finished before step G can begin.
Step T must be finished before step R can begin.
Step H must be finished before step L can begin.
Step V must be finished before step W can begin.
Step G must be finished before step U can begin.
Step K must be finished before step A can begin.
Step D must be finished before step Z can begin.
Step C must be finished before step E can begin.
Step X must be finished before step P can begin.
Step Y must be finished before step U can begin.
Step R must be finished before step O can begin.
Step W must be finished before step U can begin.
Step O must be finished before step Q can begin.
Step A must be finished before step P can begin.
Step B must be finished before step E can begin.
Step F must be finished before step E can begin.
Step Q must be finished before step U can begin.
Step M must be finished before step E can begin.
Step P must be finished before step U can begin.
Step L must be finished before step Z can begin.
Step Z must be finished before step U can begin.
Step U must be finished before step E can begin.
Step I must be finished before step E can begin.
Step H must be finished before step G can begin.
Step X must be finished before step I can begin.
Step K must be finished before step X can begin.
Step Z must be finished before step I can begin.
Step S must be finished before step M can begin.
Step L must be finished before step U can begin.
Step A must be finished before step M can begin.
Step W must be finished before step A can begin.
Step N must be finished before step A can begin.
Step S must be finished before step E can begin.
Step W must be finished before step Q can begin.
Step J must be finished before step L can begin.
Step Q must be finished before step L can begin.
Step M must be finished before step U can begin.
Step H must be finished before step E can begin.
Step D must be finished before step E can begin.
Step V must be finished before step P can begin.
Step Q must be finished before step M can begin.
Step X must be finished before step W can begin.
Step K must be finished before step I can begin.
Step T must be finished before step H can begin.
Step Y must be finished before step L can begin.
Step G must be finished before step O can begin.
Step M must be finished before step Z can begin.
Step F must be finished before step Z can begin.
Step Q must be finished before step E can begin.
Step H must be finished before step C can begin.
Step Q must be finished before step P can begin.
Step D must be finished before step U can begin.
Step Z must be finished before step E can begin.
Step O must be finished before step M can begin.
Step L must be finished before step I can begin.
Step J must be finished before step A can begin.
Step Q must be finished before step Z can begin.
Step P must be finished before step I can begin.
Step K must be finished before step O can begin.
Step R must be finished before step E can begin.
Step W must be finished before step F can begin.
Step D must be finished before step Q can begin.
Step R must be finished before step U can begin.
Step W must be finished before step P can begin.
Step S must be finished before step Z can begin.
Step T must be finished before step P can begin.
Step B must be finished before step Q can begin.
Step S must be finished before step T can begin.
Step R must be finished before step A can begin.
Step K must be finished before step R can begin.
Step N must be finished before step G can begin.
Step C must be finished before step W can begin.
Step T must be finished before step A can begin.
Step B must be finished before step Z can begin.
Step C must be finished before step P can begin.
Step D must be finished before step P can begin.
Step B must be finished before step P can begin.
Step F must be finished before step U can begin.
Step V must be finished before step X can begin.
Step K must be finished before step W can begin.
Step Y must be finished before step I can begin.
Step C must be finished before step B can begin.
Step X must be finished before step L can begin.
Step X must be finished before step M can begin.
Step H must be finished before step P can begin.
Step S must be finished before step F can begin.
Step J must be finished before step Y can begin.
Step Y must be finished before step Z can begin.
Step B must be finished before step I can begin.
Step S must be finished before step C can begin.
Step K must be finished before step E can begin.
Step N must be finished before step Q can begin.
Step A must be finished before step Z can begin.
Step J must be finished before step I can begin.
Step Y must be finished before step O can begin.
Step Y must be finished before step F can begin.
Step S must be finished before step U can begin.
Step D must be finished before step W can begin.
Step V must be finished before step D can begin."""
