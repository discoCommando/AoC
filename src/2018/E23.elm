module E23 exposing (..)

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


type alias Nanobot =
    { inRadiusOf : Int, x : Int, y : Int, z : Int, radius : Int }


parseNanobot : Parser Nanobot
parseNanobot =
    Parser.succeed (Nanobot 0)
        |. keyword "pos=<"
        |= H.pInt
        |. keyword ","
        |= H.pInt
        |. keyword ","
        |= H.pInt
        |. keyword ">, r="
        |= H.pInt


type alias BruteForce =
    { nanobots : List Nanobot }


isInRadius : Nanobot -> Nanobot -> Bool
isInRadius nanobot nanobot2 =
    abs (nanobot.x - nanobot2.x)
        + abs (nanobot.y - nanobot2.y)
        + abs (nanobot.z - nanobot2.z)
        <= nanobot.radius



-- minus nanobot.radius
--     ([ abs (nanobot.x - nanobot2.x)
--      , abs (nanobot.y - nanobot2.y)
--      , abs (nanobot.z - nanobot2.z)
--      ]
--
--     )
--     >= 0


minus : Int -> List Int -> Int
minus i li =
    case li of
        [] ->
            i

        a :: r ->
            minus (i - a) r


bruteforce : BruteForce -> BruteForce
bruteforce bruteForce =
    let
        initial =
            bruteForce.nanobots
    in
    initial
        |> List.map
            (\n1 ->
                initial
                    |> List.foldl
                        (\n2 sum ->
                            if isInRadius n2 n1 then
                                sum + 1
                            else
                                sum
                        )
                        0
                    |> (\v -> { n1 | inRadiusOf = v })
            )
        |> BruteForce


answer1 =
    input
        |> String.lines
        |> List.map (Parser.run parseNanobot >> H.uR)
        |> List.sortBy (\v -> -v.radius)
        |> (\list ->
                let
                    strongest =
                        H.at 0 list
                in
                List.filter (\n -> isInRadius strongest n) list |> List.length |> Debug.log "res"
           )


allPointsInRadiusHelper : ( Int, Int, Int ) -> Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
allPointsInRadiusHelper ( int, int2, int3 ) int4 intIntIntList =
    if int4 == 0 then
        ( int, int2, int3 ) :: intIntIntList
    else
        allPointsInRadiusHelper ( int, int2, int3 )
            (int4 - 1)
            (([ ( int - 1, int2, int3 )
              , ( int + 1, int2, int3 )
              , ( int, int2 - 1, int3 )
              , ( int, int2 + 1, int3 )
              , ( int, int2, int3 - 1 )
              , ( int, int2, int3 + 1 )
              ]
                ++ intIntIntList
             )
                |> List.Extra.unique
            )



-- [ ( int - 1, int2, int3 )
--       , ( int + 1, int2, int3 )
--       , ( int, int2 - 1, int3 )
--       , ( int, int2 + 1, int3 )
--       , ( int, int2, int3 - 1 )
--       , ( int, int2, int3 + 1 )
--       ]
-- |> List.foldl (\p all -> )


allPointsInRadius : Nanobot -> List ( Int, Int, Int )
allPointsInRadius nanobot =
    allPointsInRadiusHelper ( nanobot.x, nanobot.y, nanobot.z ) nanobot.radius []


check : Nanobot -> List Nanobot -> Int
check nanobot nanobots =
    allPointsInRadius nanobot
        |> List.map
            (\( x, y, z ) ->
                let
                    _ =
                        Debug.log "xyz" ( x, y, z )
                in
                nanobots
                    |> List.foldl
                        (\n sum ->
                            if
                                abs (n.x - x)
                                    + abs (n.y - y)
                                    + abs (n.z - z)
                                    <= n.radius
                            then
                                sum + 1
                            else
                                sum
                        )
                        0
            )
        |> List.sort
        |> List.reverse
        |> Debug.log "s"
        |> H.at 0



-- pointsInRange : Nanobot -> Nanobot -> List ( Int, Int, Int )
-- pointsInRange n1 n2 =
--     -- let
--     --     (r1, r2) = (n1.radius - (abs (n1.x - n2.x)
--     --         + abs (n1.y - n2.y)
--     --         + abs (n1.z - n2.z)
--     --         ),  n2.radius - (abs (n1.x - n2.x)
--     --             + abs (n1.y - n2.y)
--     --             + abs (n1.z - n2.z))
--     -- in
--     allPointsInRadius n1
--         |> List.filter
--             (\( x, y, z ) ->
--                 abs (n2.x - x)
--                     + abs (n2.y - y)
--                     + abs (n2.z - z)
--                     <= n2.radius
--             )


distance : ( Int, Int, Int ) -> ( Int, Int, Int ) -> Int
distance ( x1, y1, z1 ) ( x2, y2, z2 ) =
    abs (x1 - x2)
        + abs (y1 - y2)
        + abs (z1 - z2)


traverse : Int -> ( Int, Int, Int ) -> List ( Int, Int, Int ) -> List ( Int, Int, Int ) -> List Nanobot -> Dict ( Int, Int, Int ) Int -> Dict ( Int, Int, Int ) Int
traverse length mainPoint todo todorev nanobotList dict =
    case todo of
        [] ->
            case todorev of
                [] ->
                    dict

                _ ->
                    traverse length mainPoint (List.reverse todorev) [] nanobotList dict

        p :: rest ->
            if distance p mainPoint > length then
                traverse length mainPoint rest todorev nanobotList dict
            else if Dict.member p dict then
                traverse length mainPoint rest todorev nanobotList dict
            else
                let
                    v =
                        nanobotList
                            |> List.foldl
                                (\n v ->
                                    if distance p ( n.x, n.y, n.z ) <= n.radius then
                                        v + 1
                                    else
                                        v
                                )
                                0
                in
                let
                    ( int, int2, int3 ) =
                        p

                    newTodorev =
                        [ ( int - 1, int2, int3 )
                        , ( int + 1, int2, int3 )
                        , ( int, int2 - 1, int3 )
                        , ( int, int2 + 1, int3 )
                        , ( int, int2, int3 - 1 )
                        , ( int, int2, int3 + 1 )
                        ]
                            |> List.foldl (::) todorev
                in
                traverse length mainPoint rest newTodorev nanobotList (dict |> Dict.insert p v)


addToEndOf : List a -> List a -> List a
addToEndOf aList aList2 =
    let
        rev =
            List.reverse aList2
    in
    aList |> List.foldl (::) rev |> List.reverse



-- sUPERBRUTEFORCE : List Nanobot -> Dict ( Int, Int, Int ) Int
-- sUPERBRUTEFORCE nanobots =
--     nanobots
--         |> List.foldl
--             (\n1 dict ->
--                 allPointsInRadius n1
--                     |> List.foldl
--                         (\p dict -> Dict.update p (Maybe.withDefault 0 >> (+) 1 >> Just) dict)
--                         dict
--             )
--             Dict.empty
-- answer2 =
--     input
--         |> String.lines
--         |> List.map (Parser.run parseNanobot >> H.uR)
--         |> BruteForce
--         |> bruteforce
--         |> .nanobots
--         |> debug
--         |> findMod
--         |> Debug.log "a"
-- |> Debug.log "nanobots"
-- |> sUPERBRUTEFORCE
-- |> Debug.log "superbruteforce"
-- |> (\ns ->
--         let
--             n1 =
--                 H.at 0 ns |> Debug.log "asd"
--         in
--         traverse 1 ( n1.x, n1.y, n1.z ) [ ( n1.x, n1.y, n1.z ) ] [] ns Dict.empty
--    )
-- |> Dict.toList
-- |> List.sortBy (Tuple.second >> (-) 0)
-- |> H.at 0
-- |> Debug.log "result"
-- |> List.sortBy (\v -> -v.inRadiusOf)
-- |> (\nanobots ->
--         let
--             a =
--                 H.at 0 nanobots
--         in
--         check a nanobots |> Debug.log "a"
--    )


modminus : Int -> Int -> Int
modminus i1 i2 =
    if i1 > 0 then
        i1 % i2
    else
        -(abs i1 % i2)


findMod : List Nanobot -> Int
findMod ls =
    let
        f i =
            let
                nls =
                    List.map (\n -> ( n.z // i, n.y // i, n.z // i )) ls |> Set.fromList
            in
            if Set.size nls == List.length ls then
                i
            else
                f (i - 33333)
    in
    f 1000001


draw : List Nanobot -> String
draw ls =
    let
        nls =
            ls |> List.map (\n -> ( ( n.x // 10000000, n.y // 10000000 ), n.z // 10000000 ))

        set =
            nls |> Dict.fromList

        set2 =
            ls |> List.map (\n -> ( n.x // 133343, n.y // 133343, n.z // 133343 )) |> Set.fromList

        _ =
            Debug.log "size" <| Set.size set2

        _ =
            Debug.log "origsize" <| List.length ls

        _ =
            ls |> List.sortBy .radius |> H.at 0 |> Debug.log "smallest"

        ( minx, maxx, miny, maxy ) =
            nls
                |> List.foldl
                    (\( ( x, y ), z ) ( minx, maxx, miny, maxy ) ->
                        ( min minx x, max maxx x, min miny y, max maxy y )
                    )
                    ( 1000, -1000, 1000, -1000 )
                |> Debug.log "min"
    in
    List.range miny maxy
        |> List.map
            (\y ->
                List.range minx maxx
                    |> List.map
                        (\x ->
                            case Dict.get ( x, y ) set of
                                Just a ->
                                    if a < 10 && a >= 0 then
                                        "  " ++ (a |> toString)
                                    else if a > 10 then
                                        " " ++ (a |> toString)
                                    else if a > -10 then
                                        " " ++ toString a
                                    else
                                        toString a

                                _ ->
                                    "..."
                        )
                    |> String.join ""
            )
        |> String.join "\n"


debug : List Nanobot -> List Nanobot
debug ls =
    let
        _ =
            Debug.log (draw ls) ""
    in
    ls


mods =
    38


shitson : List Nanobot -> Dict ( Int, Int, Int ) Int
shitson nanobots =
    let
        newNanobots =
            nanobots |> List.map (\n -> { n | x = n.x % mods, y = n.y % mods, z = n.z % mods, radius = n.radius % mods })
    in
    List.range 0 (mods - 1)
        |> List.foldl
            (\x d ->
                List.range 0 (mods - 1)
                    |> List.foldl
                        (\y d ->
                            List.range 0 (mods - 1)
                                |> List.foldl
                                    (\z d ->
                                        let
                                            v =
                                                newNanobots
                                                    |> List.foldl
                                                        (\n v ->
                                                            if distance ( x, y, z ) ( n.x, n.y, n.z ) <= n.radius then
                                                                v + 1
                                                            else
                                                                v
                                                        )
                                                        0
                                        in
                                        Dict.insert ( x, y, z ) v d
                                    )
                                    d
                        )
                        d
            )
            Dict.empty


type Dimension
    = X
    | Y
    | Z


type KDTree a
    = Node Dimension Nanobot (KDTree a) (KDTree a) a
    | Leaf


getA : KDTree a -> Maybe a
getA aKDTree =
    case aKDTree of
        Node dimension nanobot kDTree kDTree2 a ->
            Just a

        Leaf ->
            Nothing


next : Dimension -> Dimension
next dimension =
    case dimension of
        X ->
            Y

        Y ->
            Z

        Z ->
            X


getDimension : Dimension -> Nanobot -> Int
getDimension dimension nanobot =
    case dimension of
        X ->
            nanobot.x

        Y ->
            nanobot.y

        Z ->
            nanobot.z


getMedian : List a -> ( List a, a, List a )
getMedian l =
    let
        i =
            List.length l // 2
    in
    ( List.take i l, H.at i l, List.drop (i + 1) l )


makeTree : List Nanobot -> Dimension -> KDTree ()
makeTree nanobots dimension =
    case nanobots of
        [] ->
            Leaf

        _ ->
            let
                sorted =
                    nanobots |> List.sortBy (getDimension dimension)

                ( l, m, r ) =
                    getMedian sorted

                nextDimension =
                    next dimension
            in
            Node dimension m (makeTree l nextDimension) (makeTree r nextDimension) ()


type alias State =
    { minx : Int, maxx : Int, miny : Int, maxy : Int, minz : Int, maxz : Int }


merge : State -> State -> State
merge state1 state2 =
    { minx = min state1.minx state2.minx
    , maxx = max state1.maxx state2.maxx
    , miny = min state1.miny state2.miny
    , maxy = max state1.maxy state2.maxy
    , minz = min state1.minz state2.minz
    , maxz = max state1.maxz state2.maxz
    }


fromNanobot : Nanobot -> State
fromNanobot nanobot =
    { minx = nanobot.x, maxx = nanobot.x, miny = nanobot.y, maxy = nanobot.y, minz = nanobot.z, maxz = nanobot.z }


defaultState : State
defaultState =
    State 100000000 -100000000 100000000 -100000000 100000000 -100000000


decorateTree : KDTree () -> KDTree State
decorateTree kDTree =
    case kDTree of
        Node dimension nanobot kDTree kDTree2 () ->
            let
                l =
                    decorateTree kDTree

                r =
                    decorateTree kDTree2

                newa =
                    fromNanobot nanobot
                        |> merge (l |> getA |> Maybe.withDefault defaultState)
                        |> merge (r |> getA |> Maybe.withDefault defaultState)
            in
            Node dimension nanobot l r newa

        Leaf ->
            Leaf


test1 =
    inpute2
        |> String.lines
        |> List.map (Parser.run parseNanobot >> H.uR)
        |> (\l -> makeTree l X)
        |> decorateTree
        |> Debug.log "tree"


getCornerPoints : Int -> ( Int, Int, Int ) -> List ( Int, Int, Int )
getCornerPoints reach ( int, int2, int3 ) =
    [ ( int, int2, int3 + reach )
    , ( int, int2, int3 - reach )
    , ( int, int2 + reach, int3 )
    , ( int, int2 - reach, int3 )
    , ( int + reach, int2, int3 )
    , ( int - reach, int2, int3 )
    ]


findBestCorners : List Nanobot -> List ( ( Int, Int, Int ), Int )
findBestCorners nanobots =
    nanobots
        |> List.map (\n -> getCornerPoints n.radius ( n.x, n.y, n.z ))
        |> List.concat
        |> List.map (\p -> ( p, nanobots |> List.filter (\n -> distance p ( n.x, n.y, n.z ) <= n.radius) |> List.length ))
        |> List.sortBy (Tuple.second >> (*) -1)



-- |> Debug.log "bestCorners"


test3 =
    input
        |> String.lines
        |> List.map (Parser.run parseNanobot >> H.uR)
        |> findBestCorners


getSearchPoints : ( Int, Int, Int ) -> List ( Int, Int, Int )
getSearchPoints p =
    getCornerPoints 1 p
        ++ getCornerPoints 5 p
        ++ getCornerPoints 10 p
        ++ getCornerPoints 30 p
        ++ getCornerPoints 100 p


explore : List Nanobot -> Int -> List ( Int, Int, Int ) -> Dict ( Int, Int, Int ) Int -> Dict ( Int, Int, Int ) Int
explore nanobots times ps dict =
    if times == 0 then
        dict
    else
        case ps of
            [] ->
                dict

            p :: rest ->
                let
                    newPoints =
                        getCornerPoints 1 p |> List.filter (\p -> Dict.member p dict |> not)

                    score =
                        nanobots |> List.filter (\n -> distance p ( n.x, n.y, n.z ) <= n.radius) |> List.length
                in
                explore nanobots (times - 1) (newPoints |> addToEndOf rest) (dict |> Dict.insert p score)


shittyfind : List ( ( Int, Int, Int ), Int ) -> List Nanobot -> List ( ( Int, Int, Int ), Int )
shittyfind points nanobots =
    let
        newPoints =
            points
                |> (\ps -> List.take 10 ps ++ (List.reverse ps |> List.take 10))
                |> List.map (Tuple.first >> getSearchPoints)
                |> List.concat
                |> List.map (\p -> ( p, nanobots |> List.filter (\n -> distance p ( n.x, n.y, n.z ) <= n.radius) |> List.length ))
                |> List.sortBy (Tuple.second >> (*) -1)

        b1 =
            H.at 0 points

        b2 =
            H.at 1 newPoints

        _ =
            Debug.log (toString b1) b2
    in
    if Tuple.second b1 < Tuple.second b2 then
        shittyfind newPoints nanobots
        -- else if Tuple.second b1 == Tuple.second b2 then
        --     let
        --         ( x1, y1, z1 ) =
        --             b1 |> Tuple.first
        --
        --         ( x2, y2, z2 ) =
        --             b1 |> Tuple.first
        --
        --         newP ( x3, y3, z3 ) =
        --             ( x3 + (x2 - x1) * 1000, y3 + (y2 - y1) * 1000, z3 + (z2 - z1) * 1000 )
        --     in
        --     shittyfind (newPoints |> List.map (Tuple.mapFirst newP)) nanobots
    else
        points


shittyfind2 : Int -> List ( ( Int, Int, Int ), Int ) -> List Nanobot -> Dict ( Int, Int, Int ) Int -> List ( ( Int, Int, Int ), Int )
shittyfind2 times points nanobots d =
    if times == 0 then
        points
    else
        let
            d2 =
                explore nanobots 1500 ([ points |> H.at 0 |> Tuple.first, points |> H.at 1 |> Tuple.first, points |> H.at 3 |> Tuple.first ] |> List.map (getCornerPoints 100) |> List.concat) d
        in
        d2
            |> Dict.toList
            |> List.sortBy (Tuple.second >> (*) -1)
            |> Debug.log "asd"
            |> (\l -> shittyfind2 (times - 1) l nanobots d2)


draw2 : List Nanobot -> String
draw2 ls =
    let
        nls =
            ls |> List.map (\n -> ( ( n.x // 133343, n.y // 133343 ), n.z // 133343 ))

        set =
            nls |> Dict.fromList

        set2 =
            ls |> List.map (\n -> ( n.x // 133343, n.y // 133343, n.z // 133343 )) |> Set.fromList

        _ =
            Debug.log "size" <| Set.size set2

        _ =
            Debug.log "origsize" <| List.length ls

        _ =
            ls |> List.sortBy .radius |> H.at 0 |> Debug.log "smallest"

        ( minx, maxx, miny, maxy ) =
            nls
                |> List.foldl
                    (\( ( x, y ), z ) ( minx, maxx, miny, maxy ) ->
                        ( min minx x, max maxx x, min miny y, max maxy y )
                    )
                    ( 1000, -1000, 1000, -1000 )
                |> Debug.log "min"
    in
    List.range miny maxy
        |> List.map
            (\y ->
                List.range minx maxx
                    |> List.map
                        (\x ->
                            case Dict.get ( x, y ) set of
                                Just a ->
                                    if a < 10 && a >= 0 then
                                        "  " ++ (a |> toString)
                                    else if a > 10 then
                                        " " ++ (a |> toString)
                                    else if a > -10 then
                                        " " ++ toString a
                                    else
                                        toString a

                                _ ->
                                    "..."
                        )
                    |> String.join ""
            )
        |> String.join "\n"


test4 =
    input
        |> String.lines
        |> List.map (Parser.run parseNanobot >> H.uR)
        |> debug
        |> (\nanobots ->
                let
                    points =
                        findBestCorners nanobots |> Debug.log "points"

                    -- |> List.map Tuple.first
                    _ =
                        points
                            |> List.map Tuple.first
                            |> List.map (\p -> ( p, nanobots |> List.filter (\n -> distance p ( n.x, n.y, n.z ) <= n.radius) |> List.length ))
                            |> List.sortBy (Tuple.second >> (*) -1)
                            |> (\res -> shittyfind res nanobots)
                            |> Debug.log "state"
                in
                -- points
                --     |> List.map (\p -> ( p, nanobots |> List.filter (\n -> distance p ( n.x, n.y, n.z ) <= n.radius) |> List.length ))
                --     |> List.sortBy (Tuple.second >> (*) -1)
                --     |> (\res -> shittyfind res nanobots)
                --     |> Debug.log "state"
                shittyfind2 1 points nanobots Dict.empty
                    |> Debug.log "state"
           )


nanobots =
    input
        |> String.lines
        |> List.map (Parser.run parseNanobot >> H.uR)


inpute =
    """pos=<0,0,0>, r=4
pos=<-1,-2,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1"""


inpute2 =
    """pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5"""


input =
    """pos=<61981295,70302690,50074583>, r=86817788
pos=<-39574082,11593440,52034024>, r=86226353
pos=<-22613800,41560771,22762338>, r=83067593
pos=<-12414843,51978293,68300191>, r=85018769
pos=<-12727191,34870716,52975971>, r=52899633
pos=<-32063168,40924402,49558575>, r=74871596
pos=<18752046,16372097,-9651854>, r=70068867
pos=<3474631,29804741,98417262>, r=77072884
pos=<18354169,57026789,74027645>, r=65025959
pos=<22104105,74882695,61927007>, r=67031041
pos=<-42133805,38530007,57591711>, r=90581205
pos=<-47869746,22599340,42855147>, r=77956291
pos=<-27020782,-4791281,34203354>, r=93149809
pos=<-18487970,40655084,49627122>, r=61095611
pos=<10137288,54171533,97007570>, r=93367254
pos=<21836035,43430190,47217893>, r=78376026
pos=<8216569,21003453,101684850>, r=78676489
pos=<-40466865,34952266,70038151>, r=97782720
pos=<72670731,-8427298,58419388>, r=97862747
pos=<-26904810,19437248,39548346>, r=63460381
pos=<-14862223,38537164,6158587>, r=88895667
pos=<-4093645,28809183,65079634>, r=50307904
pos=<-13276886,43613687,45971502>, r=55187766
pos=<49089034,29003918,14560017>, r=57321614
pos=<-13807521,-29104885,50131321>, r=99255387
pos=<48476757,37188242,72540714>, r=62665037
pos=<7140081,51751715,-8561127>, r=94827577
pos=<-43249072,32737116,53752441>, r=82064216
pos=<6641528,98591490,47271959>, r=91547386
pos=<-23305462,10475650,56073326>, r=75115096
pos=<6053316,21990329,84119652>, r=62287689
pos=<7023331,60698763,57949984>, r=63950985
pos=<-36608889,31399128,35446137>, r=74217136
pos=<6817093,34966536,98116734>, r=78591704
pos=<4957605,61740316,52702032>, r=61810336
pos=<-2269508,-8851147,43341014>, r=63321024
pos=<12839874,52090204,58609561>, r=50185956
pos=<-24117273,33915884,49793440>, r=60152075
pos=<74759590,28034712,42456610>, r=54126256
pos=<-48197949,37668470,48713802>, r=86905688
pos=<-465248,41562365,60498029>, r=54851108
pos=<10694430,32208520,48027772>, r=61692668
pos=<-8548512,14246214,83247198>, r=83761331
pos=<43950964,33755026,90173201>, r=72338199
pos=<-14628694,43441676,58236468>, r=68632319
pos=<106381567,34503222,46141825>, r=91485627
pos=<-42403687,53189612,46049581>, r=93968393
pos=<3940021,35797440,82233626>, r=66416478
pos=<-12215361,4082478,43661351>, r=60012674
pos=<19860123,68466722,60165468>, r=61097501
pos=<70777186,32758921,32118468>, r=65206073
pos=<-38591125,46522298,54596946>, r=92035892
pos=<22636563,105345403,45688673>, r=80723037
pos=<-3951303,29434316,22353040>, r=52688125
pos=<-176419819,31194464,52039201>, r=76587195
pos=<-3596358,48726226,78390625>, r=83038642
pos=<7182613,37282188,20126279>, r=51628082
pos=<-25240363,51483969,55323702>, r=84373768
pos=<21428319,64401806,57994375>, r=53293330
pos=<-5939048,7790261,21935573>, r=71754303
pos=<-14391272,30291007,70042112>, r=67049881
pos=<-22323827,29920141,55916312>, r=60485891
pos=<13475129,52193445,63897921>, r=54942071
pos=<-3994272,15505795,89454649>, r=84155011
pos=<475865,1537450,69193888>, r=73392255
pos=<-10815135,44879964,69035166>, r=77056155
pos=<11306358,28884419,64442081>, r=91273574
pos=<14678811,33694651,48185622>, r=71901550
pos=<3756099,71276526,21743645>, r=87431573
pos=<40262582,59466852,61256380>, r=65445301
pos=<11616060,70966185,64722364>, r=76397938
pos=<3872060,39650278,107716165>, r=95819913
pos=<9586133,45083261,78735554>, r=66558242
pos=<77865670,42917012,73695024>, r=98936729
pos=<-1745961,55327607,42088395>, r=56640112
pos=<26855240,67981174,26878283>, r=61746856
pos=<-14224458,28877026,31495228>, r=53261041
pos=<15850501,66941649,70246547>, r=73663230
pos=<-22483771,25856673,49504583>, r=52343508
pos=<9442908,53159770,55875115>, r=51917541
pos=<1959101,903989,50510107>, r=53859027
pos=<7569188,-24881655,45995388>, r=69519533
pos=<1336123,50534199,50773018>, r=52296589
pos=<-6780305,30188574,97998637>, r=87293233
pos=<-27347231,32710526,48539527>, r=60922750
pos=<-54109318,45563567,46755290>, r=98753653
pos=<-23931716,34591305,51177382>, r=62025865
pos=<1842700,54313361,57870403>, r=62666497
pos=<9906862,47176518,46588813>, r=49926624
pos=<-5650338,36212767,99081721>, r=93270578
pos=<-9210222,73861103,51071528>, r=86469031
pos=<-48605170,28058111,39410284>, r=78908074
pos=<6277738,31698726,83207681>, r=60954109
pos=<5935953,25376812,52194560>, r=66482724
pos=<-33349449,32090950,46515684>, r=64281651
pos=<-1144497,68829923,72349587>, r=94649606
pos=<68150356,106204720,129297951>, r=83514523
pos=<8426203,98908672,49758875>, r=92566786
pos=<16538168,48418035,65737293>, r=49942650
pos=<19573131,82515397,58801909>, r=74069662
pos=<98906639,46422007,45986912>, r=95774562
pos=<12340098,62609442,59505078>, r=62099865
pos=<-28251146,31055125,25461712>, r=75499853
pos=<19758085,59632651,60443124>, r=52643379
pos=<-63046046,25663836,40121278>, r=92801960
pos=<-37359413,31532323,71313730>, r=92530910
pos=<53387378,31226416,70902974>, r=59975761
pos=<-111343725,90010574,65438544>, r=96905196
pos=<39565178,36023094,48981621>, r=96285986
pos=<-18425354,35959496,54078093>, r=60788536
pos=<4117058,66844365,17924670>, r=86457430
pos=<4823188,-14314294,64385895>, r=80088678
pos=<20323219,61060408,10757887>, r=71634602
pos=<89860271,38268711,51332073>, r=83920073
pos=<6829746,30401653,100430131>, r=76327501
pos=<-18163621,73955629,42658058>, r=91116552
pos=<-9868198,28983545,82688052>, r=73865474
pos=<-3955635,32776886,7789254>, r=70598099
pos=<22115720,88382733,50485824>, r=69078561
pos=<-25241164,31938266,56917737>, r=66422608
pos=<14985873,-15753032,55823238>, r=62802267
pos=<16643466,61628684,29386869>, r=57253552
pos=<21580242,42328478,123256438>, r=96330398
pos=<-25199672,13587085,61865724>, r=79689986
pos=<25598120,-21677261,57401363>, r=63021973
pos=<20683805,43416073,80207144>, r=55264851
pos=<-12206963,28525784,76317729>, r=69375956
pos=<66517337,40023722,63765750>, r=74765959
pos=<21046583,32632182,66997632>, r=92540759
pos=<-13138431,65411819,48286547>, r=79162230
pos=<-33524737,76477821,155964131>, r=73103463
pos=<-33331228,9961241,69359006>, r=98940886
pos=<39564979,28572472,53811503>, r=61407169
pos=<-48869811,21665479,51245298>, r=84661484
pos=<-8710194,53780076,47547665>, r=62363657
pos=<-7314943,55087956,46146524>, r=60875047
pos=<-61837954,25859003,56641091>, r=98831768
pos=<-5695240,58983811,70577156>, r=87581649
pos=<-25981379,35101300,31842886>, r=70894580
pos=<3669996,59130810,1907555>, r=95208068
pos=<34776272,-4188188,70499131>, r=67808809
pos=<-35530241,30475154,50076449>, r=68407314
pos=<-8155622,43149031,72681791>, r=76311918
pos=<6666752,64359993,46401448>, r=56420132
pos=<14935236,11018971,47709423>, r=95072999
pos=<-31457362,28416643,49636691>, r=61836519
pos=<-16109927,48371152,51014763>, r=67821289
pos=<42694611,33011663,86560963>, r=66726260
pos=<10684191,49511278,21069239>, r=59412987
pos=<-423180,40569321,15553431>, r=67093941
pos=<-32350990,31916431,66781337>, r=83374335
pos=<-40417062,33333028,20729982>, r=94675000
pos=<-5784232,29640751,62496121>, r=50246685
pos=<-15428974,17471422,52568175>, r=56737766
pos=<-2932788,33823267,68046768>, r=57128261
pos=<63579456,43455645,46047101>, r=57541191
pos=<15403015,31974701,112979009>, r=81876224
pos=<-24087841,19769551,56050682>, r=66580826
pos=<-11649390,82772147,39945831>, r=96130620
pos=<20266288,41030544,49627298>, r=52471876
pos=<15271488,104802139,55747920>, r=97604129
pos=<-17294454,29095937,38121063>, r=49924113
pos=<-28047432,15028531,34900811>, r=73659132
pos=<63036558,50019938,71871561>, r=89387198
pos=<-34801957,36642230,53036734>, r=76806612
pos=<-38144076,30407351,68440798>, r=89317810
pos=<3512495,54767308,49076598>, r=52656960
pos=<14274088,34274498,46377106>, r=89548213
pos=<39329258,25136254,46456590>, r=73770931
pos=<-43496380,31317940,30852295>, r=85616814
pos=<14184631,31298574,54769039>, r=52386260
pos=<80496193,20583845,74488856>, r=92746549
pos=<-23389071,63823214,32693625>, r=96173548
pos=<-25690209,44737890,68493032>, r=91246752
pos=<-27606431,29594954,78055471>, r=87582356
pos=<72441600,37663099,5449732>, r=98443349
pos=<64033149,70075201,56633740>, r=95201164
pos=<-41252112,41584295,58764742>, r=93926622
pos=<-12824950,60182885,70449830>, r=95783330
pos=<63858499,1902299,56897189>, r=77198605
pos=<-35302433,28371559,42263514>, r=63065421
pos=<-50193553,13029004,178032706>, r=87155339
pos=<-10241339,30475433,65035094>, r=58077303
pos=<27176667,88255399,39199151>, r=70021298
pos=<3789738,63200642,58556644>, r=70293009
pos=<-29026962,17065408,63626804>, r=81800634
pos=<-15528112,28599049,55171449>, r=51624044
pos=<67553889,-12734884,46684710>, r=85318716
pos=<-22058789,25579659,47800035>, r=50491043
pos=<65502868,77153034,48367762>, r=95482653
pos=<52012555,29111973,69892338>, r=55475892
pos=<7094796,39275693,83735416>, r=68242166
pos=<1860284,40455320,92877446>, r=83797940
pos=<11928924,-2446863,70048662>, r=66778312
pos=<7667696,94196419,41409153>, r=86774357
pos=<-47571004,37469854,46817463>, r=84183789
pos=<-4156736,84010739,52365610>, r=92858630
pos=<6056924,12888978,84519517>, r=71785287
pos=<-47345906,30387796,45986301>, r=76045882
pos=<-14305750,38815805,53464135>, r=58911123
pos=<-5509681,53176769,46879448>, r=57891332
pos=<-12499446,35923605,56390435>, r=57139055
pos=<-52764985,37485922,49356853>, r=91933283
pos=<-995676,157395073,-3073485>, r=52828911
pos=<20007537,18034710,53163539>, r=53689323
pos=<14489164,16396768,90904833>, r=66230860
pos=<-56656961,29846250,59765242>, r=98593988
pos=<12580077,72518760,77049199>, r=89313340
pos=<100504676,32770164,49001641>, r=86735485
pos=<56363428,40982042,51325603>, r=53130348
pos=<67579027,45292106,26873551>, r=79786057
pos=<26991023,51464352,68172728>, r=51087111
pos=<-2477721,38040811,90549029>, r=83393046
pos=<39812282,49420172,27946843>, r=55074069
pos=<-7592987,58715561,48320434>, r=66954427
pos=<7777052,28459937,51985707>, r=53419415
pos=<-34825568,30793001,25736210>, r=81537269
pos=<-24303136,54965408,48394944>, r=79988927
pos=<-23117889,8738217,68644161>, r=89235517
pos=<-25024083,8306905,47947646>, r=70876621
pos=<2007442,56985136,59636415>, r=66939562
pos=<-13687550,71578246,57121637>, r=94712935
pos=<-16345760,32983233,89061217>, r=90715694
pos=<86368374,35824210,61445425>, r=88097056
pos=<-35743614,53334234,57645839>, r=99049305
pos=<-15191744,62377081,28606960>, r=90616727
pos=<15180924,74088799,62177344>, r=73410680
pos=<87747671,31399471,31905651>, r=81030076
pos=<490300,35168974,87629502>, r=74633619
pos=<-7639730,47107985,53037366>, r=60110529
pos=<-15154000,47565888,46724248>, r=61769578
pos=<-15930553,8949295,49890450>, r=63083820
pos=<-2085646,59550216,190147611>, r=69262241
pos=<7225777,50890764,62861738>, r=58852281
pos=<90275849,20142660,46920592>, r=75399045
pos=<-26764544,42182428,46746825>, r=68019674
pos=<76321467,29233298,55996378>, r=66010239
pos=<-72399950,54572749,137176788>, r=75623903
pos=<20511710,69381130,54777605>, r=55972482
pos=<11171812,-1838585,60251989>, r=57130513
pos=<-15322219,34977106,95397324>, r=98022178
pos=<45186029,75515109,54316351>, r=79476635
pos=<-33243023,32685580,54621141>, r=72875433
pos=<-61584462,35077699,48778369>, r=97765970
pos=<-2080973,32701858,77315532>, r=64423903
pos=<-28378799,39591108,22227847>, r=87396852
pos=<-8099746,62641467,55946146>, r=79012806
pos=<-24720179,45274695,48132554>, r=70452861
pos=<20784250,45013508,95099754>, r=71654519
pos=<-30064753,33988072,46097252>, r=62475758
pos=<34563348,30488217,55672227>, r=86287854
pos=<-16742541,7677207,74209817>, r=89487059
pos=<48717911,60141573,64839376>, r=78157946
pos=<11980231,77201897,12503508>, r=94373076
pos=<-35767666,29757894,59267775>, r=77118779
pos=<-22126819,32362172,682297>, r=95461509
pos=<-20183006,18654168,80542091>, r=88282618
pos=<29158741,90530365,43109268>, r=70368234
pos=<78589607,47357015,57145573>, r=87551217
pos=<53822697,51713673,83991488>, r=93986944
pos=<-37287747,28688663,72807970>, r=91110215
pos=<13542941,37989744,51992083>, r=61754737
pos=<14525371,30291586,55624740>, r=74352172
pos=<65594982,25847880,72811378>, r=70903881
pos=<85028254,32319009,59775298>, r=81581589
pos=<-47952231,24568531,49936283>, r=79531659
pos=<-21321958,21913518,49475661>, r=55095834
pos=<-48453939,34233126,49830769>, r=84843339
pos=<99391685,35202974,43142729>, r=85240537
pos=<22710331,34170304,55092906>, r=91207796
pos=<-12135705,32243191,63938331>, r=60642766
pos=<-12338157,43480193,54115729>, r=62259632
pos=<8622864,24881999,3805702>, r=58230495
pos=<34000374,4364405,71395390>, r=59376643
pos=<-19712623,50327429,47038637>, r=69404671
pos=<-8596487,17062414,87164926>, r=84910663
pos=<-18913938,45008114,51843283>, r=68090880
pos=<19472967,181579393,106030809>, r=56142937
pos=<-17173370,-3226764,55066991>, r=81678816
pos=<63850431,62835473,23871657>, r=96602589
pos=<-3798894,30299579,101412001>, r=87836232
pos=<9068586,62089120,57368844>, r=62714847
pos=<-22474207,29499886,47923819>, r=52223751
pos=<-45651602,35036414,46323892>, r=79337438
pos=<-12419250,32111741,56514558>, r=53370983
pos=<12664542,52854651,103268523>, r=95784065
pos=<22357114,12328694,108875611>, r=80401468
pos=<-6746842,50055566,49180574>, r=58308458
pos=<-27320265,33686937,48286622>, r=61619350
pos=<-5359976,29458721,92619986>, r=79764181
pos=<10052919,30248699,46342959>, r=64500134
pos=<31603672,24458394,109318746>, r=74809324
pos=<15363324,39635782,101274262>, r=77872183
pos=<13846270,53578231,29556061>, r=51830690
pos=<-15354477,46149883,46658194>, r=60488156
pos=<-569362,85626876,46006904>, r=84528844
pos=<10749296,42284246,82711135>, r=66571646
pos=<-14856420,53289609,77345425>, r=97817094
pos=<-23825436,15929136,64937891>, r=79045870
pos=<89568342,30383137,46009564>, r=70420359
pos=<13145554,32649278,85217834>, r=57047183
pos=<554467,16643477,85598183>, r=74612189
pos=<36666635,41828463,88364574>, r=71318747
pos=<12602990,89529451,67821002>, r=97072935
pos=<-33112360,18329619,59694200>, r=80688742
pos=<-15561603,38236853,67667639>, r=73791835
pos=<3519076,58523179,5256274>, r=91402635
pos=<37058015,73816180,6497217>, r=98165326
pos=<-485820,34664283,76480969>, r=63956703
pos=<8046540,50222182,60417372>, r=54918488
pos=<6698732,2095928,57478145>, r=54895175
pos=<-110267625,103715108,49404770>, r=83596582
pos=<14488717,37517183,47868787>, r=90880734
pos=<11594929,31481958,92334208>, r=64546675
pos=<13142664,33433973,51871828>, r=78783673
pos=<14725843,21079396,84588725>, r=54995145
pos=<17991922,28746415,53178977>, r=62665596
pos=<21345476,109893878,54658351>, r=95532225
pos=<99514509,32213492,45962288>, r=82149323
pos=<37772543,40405568,108402968>, r=91040181
pos=<16477866,1889108,-9755558>, r=86930037
pos=<6066766,45074819,93307079>, r=84640752
pos=<63259627,39790194,63298460>, r=70807277
pos=<85870661,25697065,48142683>, r=66661876
pos=<-20063299,13270466,81943036>, r=94947530
pos=<9219021,14187810,110350611>, r=93155487
pos=<11396690,79601451,29059476>, r=80800086
pos=<-5992626,63895408,63403216>, r=85617223
pos=<13745746,56178190,15154849>, r=68932414
pos=<-19200598,41415603,54301128>, r=67242872
pos=<66801369,67709049,33790327>, r=94508881
pos=<-58054899,31429373,50412427>, r=92222250
pos=<17725131,19747008,101361033>, r=70100661
pos=<16315702,21937157,109577476>, r=77536345
pos=<-21450279,31429149,47689387>, r=52894343
pos=<16503867,46780903,71008828>, r=53611297
pos=<-10617311,71781674,53581209>, r=88305651
pos=<-18369624,34464166,58808640>, r=63968022
pos=<-44338154,47516775,49329347>, r=93509756
pos=<7546939,23561808,74615854>, r=49718921
pos=<-23553075,43233473,30179868>, r=78262286
pos=<-33728549,33419608,75400040>, r=94873708
pos=<4130401,-415851,60422323>, r=62919529
pos=<12199308,42656684,46658363>, r=62914010
pos=<-10823045,40017720,87358982>, r=90525236
pos=<54855928,43498518,54679995>, r=57493498
pos=<12261573,29515175,45893575>, r=97366173
pos=<15817533,48376146,72425161>, r=57309456
pos=<22499943,43484899,49303866>, r=84870164
pos=<15087449,40820405,15442472>, r=51945357
pos=<2380830,39189079,87564730>, r=76698440
pos=<10292232,61644825,58639264>, r=62317309
pos=<-25063352,29824290,49726684>, r=56939825
pos=<9247300,60592893,82851862>, r=86522910
pos=<34974237,51631378,71206261>, r=62271044
pos=<11115341,39008866,88216499>, r=68435471
pos=<-47347839,33255463,54507917>, r=87436830
pos=<-12385471,35476634,66586332>, r=66774052
pos=<40297014,43609070,76102045>, r=64467152
pos=<-25028316,48459326,70188306>, r=96001387
pos=<29716238,95743100,36047260>, r=83200834
pos=<-2195894,57161709,59974579>, r=71657615
pos=<-34897564,36092514,37195303>, r=75449626
pos=<-34350080,30930206,62762967>, r=80368703
pos=<-13299160,-1305686,52546724>, r=73363335
pos=<74203994,40071747,39943051>, r=68121068
pos=<-30255218,53252625,55149209>, r=90982504
pos=<-23708755,36047177,30518158>, r=70892646
pos=<-42225041,28551593,47086408>, r=70188476
pos=<45093813,91953991,40089158>, r=90747011
pos=<20989497,-9578662,65146013>, r=59947021
pos=<-9469151,9360043,19787978>, r=75862353
pos=<-10455182,76182076,47046337>, r=86009032
pos=<1489674,30920418,49075947>, r=50871653
pos=<-1805815,40895592,59548754>, r=54575822
pos=<22434664,50411195,76723826>, r=57025799
pos=<1351173,-35240030,57415985>, r=97516622
pos=<-32995557,45695405,31582059>, r=88763841
pos=<96147879,48598772,48672050>, r=97877850
pos=<71556712,165083816,80525352>, r=61212474
pos=<-26738516,51821211,51973287>, r=82858872
pos=<-45126452,24625563,45768892>, r=72481504
pos=<-1724534,91435747,47738636>, r=93224354
pos=<-42642617,17009143,50075464>, r=81920769
pos=<52222152,55156460,54199932>, r=66037534
pos=<8639285,85433958,47459393>, r=76579513
pos=<10141717,61817092,48597374>, r=52598556
pos=<47916267,47143215,50290112>, r=49808644
pos=<-1999143,50275699,76700962>, r=81301257
pos=<-41681257,20679616,56340422>, r=83553725
pos=<35884408,41646080,79776061>, r=61765620
pos=<4301038,52856724,21630776>, r=68579717
pos=<-40960612,30439627,37255922>, r=75799466
pos=<35444048,43762938,84706440>, r=68372926
pos=<79975658,29137911,50197120>, r=63769730
pos=<93007,44307904,61961116>, r=58501565
pos=<-32671755,39076539,66101849>, r=90175907
pos=<-19991034,51969552,46041742>, r=70328104
pos=<-35314188,37602800,46861313>, r=72103772
pos=<37201145,71666894,82972323>, r=96299533
pos=<-49234235,28079826,57132180>, r=86771712
pos=<8312877,40022919,72604965>, r=56640475
pos=<39181205,52790049,92153926>, r=88584204
pos=<7068698,47606987,62510305>, r=55374127
pos=<30603349,56814294,106942634>, r=98819430
pos=<16115552,28332758,35601360>, r=54887656
pos=<10074393,95989908,47950308>, r=86191298
pos=<-5591354,49710053,47446079>, r=55073040
pos=<-16059874,70092354,52417407>, r=90895487
pos=<-3286599,20110685,83441267>, r=72828890
pos=<-6936335,22574502,22140566>, r=57762340
pos=<32992478,36116750,76761537>, r=50329801
pos=<-89483882,3641127,125934475>, r=84985845
pos=<45083402,48971622,84039680>, r=82553733
pos=<16543763,-12232511,59927146>, r=61828172
pos=<-128930120,71482570,52773218>, r=59057806
pos=<-3321066,36249470,10553454>, r=70672189
pos=<11035114,-4452527,60224262>, r=59853336
pos=<-26291008,49849603,66835908>, r=95302073
pos=<7937807,81400916,47819246>, r=73607886
pos=<16576233,48187487,42040402>, r=56973312
pos=<4344158,75290424,35788694>, r=76812485
pos=<32189900,-55403075,49857150>, r=95795351
pos=<17696418,36185949,87820170>, r=58635317
pos=<777488,12020182,61326632>, r=54740630
pos=<33863194,35023084,109647702>, r=82992971
pos=<27201238,50703815,113905343>, r=96269385
pos=<13342983,51362786,14113529>, r=65561106
pos=<-3406344,1514525,71500197>, r=79603699
pos=<-4003952,88211664,52599168>, r=97140440
pos=<7364870,66805533,30929588>, r=70166122
pos=<4337235,69741240,49137686>, r=66867686
pos=<39289283,41543331,80086140>, r=65377837
pos=<31061019,37187231,96617615>, r=69325024
pos=<12227872,90873558,49623164>, r=80594302
pos=<-29205869,34879980,48574906>, r=64986227
pos=<-23428908,25034353,70988758>, r=75595080
pos=<31236415,75651801,72043812>, r=83391270
pos=<-35663336,33550704,50545521>, r=72085007
pos=<-25373194,33271630,48939058>, r=59909386
pos=<81186014,21770112,48774152>, r=66535373
pos=<-36266473,21205391,64408172>, r=85681063
pos=<46509791,41930052,-8736829>, r=90965014
pos=<-22028107,15234691,51487555>, r=64492638
pos=<-20542339,31539802,48757530>, r=53165383
pos=<13662571,56928981,15523678>, r=69397563
pos=<-12746344,18867316,85938166>, r=86028894
pos=<-301007,47558794,60352290>, r=60537813
pos=<32207624,60299805,78729522>, r=75695968
pos=<100081770,31328053,46754080>, r=82623316
pos=<-48070787,30652729,47098499>, r=78147703
pos=<3499835,47702960,54008261>, r=50536940
pos=<-30212070,27990135,63986849>, r=74514607
pos=<-15832151,28224015,56445561>, r=52827166
pos=<-67978767,28737500,46365512>, r=95407212
pos=<-30015019,22627881,66498575>, r=80097431
pos=<-3376964,87518399,46889764>, r=90110663
pos=<-12133745,41888151,15297059>, r=80379652
pos=<-3430750,43859378,-4475364>, r=93420295
pos=<10612582,58698878,71450281>, r=71862044
pos=<-16853417,30239412,55512427>, r=54930714
pos=<9856655,-15034177,76105296>, r=87495006
pos=<19700487,14896792,94233323>, r=65848189
pos=<90908533,40366432,47407308>, r=83141710
pos=<53053613,45815347,47231085>, r=50559042
pos=<91606120,30241005,62870344>, r=89176478
pos=<6549177,28253488,45947118>, r=89716871
pos=<-54966251,31984766,51327953>, r=90604410
pos=<11739879,47302011,70593259>, r=58480946
pos=<17782015,-43385757,45900297>, r=77715722
pos=<-22321136,33175591,26785779>, r=70365922
pos=<7447288,64317364,46803776>, r=55999602
pos=<7212096,101522192,49782085>, r=96417911
pos=<9756698,57513943,50726200>, r=50809212
pos=<53938302,42703477,85380074>, r=86480889
pos=<6127018,32634301,81765609>, r=60598446
pos=<16024259,75089269,42148970>, r=58570841
pos=<-14467950,30833664,53148474>, r=50775521
pos=<1055620,-8213227,49647151>, r=63016576
pos=<20109959,34289334,98011242>, r=64516119
pos=<-51406886,30261849,39781808>, r=83541835
pos=<21609187,48166640,103039225>, r=81922186
pos=<-37318020,42076555,46223326>, r=77943551
pos=<-9802864,50149053,57309602>, r=69587023
pos=<8892924,30956296,4508427>, r=59209772
pos=<17951962,38815438,50096954>, r=68485096
pos=<12306030,97501425,46533603>, r=84054466
pos=<-37463875,31083117,49553934>, r=70426366
pos=<92230728,52683980,47015111>, r=96388808
pos=<9364608,56647782,54993450>, r=54602057
pos=<8684942,36838535,105538060>, r=86017275
pos=<-35827611,42789189,57956518>, r=88898907
pos=<5807845,31720781,7757680>, r=59810369
pos=<-25365859,48253374,51239130>, r=77183993
pos=<-13706311,23188045,2328735>, r=83730603
pos=<-1053434,40958215,58697392>, r=53034540
pos=<16686564,43884661,76779893>, r=56303739
pos=<94046858,44171067,48044994>, r=90722205
pos=<-59177933,25088148,47504944>, r=87806389
pos=<-18654288,54793837,56331529>, r=82105172
pos=<113519092,134203172,41554166>, r=95154840
pos=<9436438,34715386,46078979>, r=73810378
pos=<49732913,61362619,75247356>, r=90801939
pos=<-54495041,22320842,50939618>, r=89325794
pos=<12427539,17900057,11259909>, r=53954136
pos=<16888421,45562882,95425764>, r=76425888
pos=<-13394846,17877372,48322242>, r=50051600
pos=<-20925432,28703245,52084455>, r=54038872
pos=<8561924,38380609,101695915>, r=83840427
pos=<76633366,35686884,48280680>, r=65060182
pos=<15355031,32838431,53406732>, r=83822095
pos=<-22241568,21612015,21308708>, r=74861862
pos=<-45540364,39478825,56854957>, r=94199599
pos=<-130761639,-7225545,18556230>, r=71625415
pos=<12371981,60358241,50483820>, r=50795545
pos=<-52881118,38556375,50525246>, r=94288177
pos=<-5508845,44982595,39309821>, r=52836406
pos=<-12223578,42705080,48054452>, r=55308618
pos=<19782229,-391087,65138940>, r=51959480
pos=<11688897,65712575,56641017>, r=62990663
pos=<26728983,4877916,77607591>, r=57803924
pos=<-66749809,31885857,41453884>, r=98836603
pos=<12947494,40474371,72186292>, r=52038612
pos=<22710949,73206326,90156950>, r=92977765
pos=<21418285,58754538,74497516>, r=64159328
pos=<-14006029,39388620,45791459>, r=51511854
pos=<32331392,-9548718,53912995>, r=54138363
pos=<-11103143,48665873,60597818>, r=72692378
pos=<8404016,40264319,-15387292>, r=88902405
pos=<-60697351,19761606,51535474>, r=98682907
pos=<113332017,31654847,49314512>, r=98760760
pos=<-17968649,23774520,26514627>, r=63220547
pos=<-7891448,38548696,51246202>, r=50011819
pos=<-36243928,28580831,31232834>, r=75246710
pos=<2236746,78152395,33336820>, r=84233689
pos=<-42646767,27988371,50996303>, r=73957310
pos=<-12975938,19343337,62716304>, r=62560618
pos=<-6698310,59281250,50138827>, r=68443906
pos=<-29169469,6610144,55165008>, r=83936144
pos=<-859099,48228088,68790664>, r=70203324
pos=<-53259426,43091307,49454062>, r=98130240
pos=<40051035,15273208,52961854>, r=55405641
pos=<8599969,22173989,57555102>, r=96158345
pos=<11561358,34996323,107452493>, r=83213190
pos=<12971841,75769901,53455514>, r=68579059
pos=<-24609077,31137632,12599716>, r=84801836
pos=<39964941,58818274,67271078>, r=70513393
pos=<11590874,44091058,16246818>, r=57908243
pos=<11534010,42492557,73281128>, r=56565109
pos=<46029692,44356665,33034407>, r=51140346
pos=<230818789,28332006,48666836>, r=77009932
pos=<-49307134,28877489,51162598>, r=81672730
pos=<-24858611,61344602,49583599>, r=88112245
pos=<-33971422,31026034,46645662>, r=63968617
pos=<-14920621,60364341,48024490>, r=75635084
pos=<-18886432,44383739,46645651>, r=62241356
pos=<63367787,57050728,56716295>, r=81594314
pos=<-29235501,30020585,46865414>, r=58446946
pos=<-1116549,51739608,46803263>, r=51985095
pos=<-12898335,8922965,49532990>, r=59720175
pos=<11377440,35121435,54012286>, r=63237886
pos=<-28362629,38084006,47961632>, r=66733755
pos=<-32732759,53759806,34185847>, r=93961530
pos=<-2049132,35100887,26872869>, r=51932182
pos=<10307391,63192491,78308824>, r=83519668
pos=<-11173886,50879581,47031969>, r=61411041
pos=<-6714487,69428860,48945046>, r=77413940
pos=<-35145310,126288277,94537390>, r=86514887
pos=<15372059,77344628,57152598>, r=71450857
pos=<-5534049,28501573,47802916>, r=70800310
pos=<30282369,-27855631,57777471>, r=74260879
pos=<27578388,34577846,94631718>, r=61246950
pos=<5377307,28529784,46392652>, r=82994735
pos=<-9288964,40034367,85892017>, r=87541479
pos=<32663376,50111633,96347262>, r=83581281
pos=<64590061,24883190,74402048>, r=72454312
pos=<6011059,52895297,-9651139>, r=98190405
pos=<-40075559,29164760,46796031>, r=68361786
pos=<-27324257,33280398,58797836>, r=71727947
pos=<65478625,51242475,73962244>, r=95142344
pos=<-8161052,44514515,51881665>, r=56882708
pos=<37117480,70516429,65453164>, r=77546136
pos=<-66556175,24793456,46807708>, r=94782274
pos=<6745175,-9697218,46249405>, r=55413236
pos=<-15842202,54067556,49741202>, r=71976478
pos=<-26234670,32761687,62384539>, r=73706383
pos=<27047080,45210798,96262259>, r=72979567
pos=<-54875381,19475504,41999639>, r=88941400
pos=<15588049,51830394,45712337>, r=63164637
pos=<78521098,9084198,53510654>, r=81292772
pos=<-35117158,30884180,40930824>, r=66725315
pos=<38517388,31310367,53377273>, r=93263507
pos=<-34824848,35863485,57039123>, r=80052924
pos=<87999262,21593229,36952685>, r=77127999
pos=<-14970039,39281480,31602555>, r=64303877
pos=<30996868,32710257,-7944148>, r=65439616
pos=<-46544578,32730823,48455778>, r=80056612
pos=<-25655223,54235827,24268664>, r=97277174
pos=<-31661104,34739101,46810221>, r=65535871
pos=<-15725105,33623089,58648908>, r=60322582
pos=<30366481,48524611,105759957>, r=89110268
pos=<7896511,67058144,60224279>, r=71711348
pos=<8886133,36820409,69879921>, r=50139913
pos=<-35009499,24967338,48346229>, r=64600051
pos=<12236743,61232811,36406256>, r=54244674
pos=<32133015,71147471,65672434>, r=73411928
pos=<-4312430,48491142,58819757>, r=63948774
pos=<-11255504,48618070,49019953>, r=61218979
pos=<-36126448,28661597,45905741>, r=63019487
pos=<6995492,-14641677,39110487>, r=64076982
pos=<-494882,62507397,74981307>, r=90309172
pos=<21658971,20515757,32538766>, r=68078733
pos=<-42721200,2165630,50987236>, r=97754561
pos=<-46441522,14810192,60365689>, r=98209160
pos=<48051150,31166319,50307623>, r=70921992
pos=<28821739,29694631,-1122960>, r=53427671
pos=<-54273229,34572094,50463893>, r=91634895
pos=<14673784,34863932,82993530>, r=55509157
pos=<-32934723,17649588,51138183>, r=72635067
pos=<-50994378,41060824,52259311>, r=96639953
pos=<-13412903,29052830,62027839>, r=56819094
pos=<16028543,72922360,86313287>, r=95532757
pos=<-29190239,30088199,43593943>, r=57339282
pos=<-67676350,28809916,49909454>, r=98721154
pos=<-43374507,39597274,53913745>, r=89211024
pos=<-27366900,30364601,50115222>, r=60172177
pos=<16917628,52399768,64291470>, r=52099066
pos=<41279617,21014635,54307699>, r=93725431
pos=<1365818,19740622,70504983>, r=55610202
pos=<-54647289,29012271,49857475>, r=85842468
pos=<44221414,66917783,53886884>, r=69485079
pos=<-9940873,70529264,26788845>, r=95336159
pos=<-2891536,43027095,81243592>, r=79487809
pos=<16856578,70770998,65638794>, r=71878974
pos=<48647431,57474901,3221288>, r=96689472
pos=<18398962,37387340,106423442>, r=77737407
pos=<6875506,40281303,93455690>, r=79187050
pos=<2454291,28249463,81600089>, r=59720809
pos=<-1468549,29220087,631469>, r=71712205
pos=<1576323,77863026,52114347>, r=80726896
pos=<11328724,-1103809,57250256>, r=53237238
pos=<14166873,32412130,88906135>, r=59476952
pos=<68172516,66679724,50794113>, r=90105431
pos=<-4882904,63239852,64521058>, r=84969357
pos=<-39573626,43281992,58908860>, r=94089955
pos=<47082853,25450033,45728957>, r=77686997
pos=<30259034,36081601,79825507>, r=50625134
pos=<13485963,34007713,111156423>, r=84003646
pos=<-103414983,93539548,30098680>, r=85922019
pos=<-6802701,-3790574,59234245>, r=76039201
pos=<82390172,51540341,50478214>, r=88867872
pos=<5396715,-12159909,37977747>, r=64326869
pos=<60082922,38998779,61741375>, r=65282109
pos=<-16364819,10714145,61687053>, r=73549544
pos=<-58301485,30719708,55794510>, r=97141432
pos=<16352057,56953747,89166657>, r=82093799
pos=<-34630538,59464956,51639108>, r=98060041
pos=<490228,30703545,109025168>, r=91563918
pos=<-6013632,8426824,52874814>, r=56673453
pos=<-36209880,25159205,51113229>, r=68375671
pos=<17796723,1411910,80111570>, r=67114697
pos=<-31889116,28510083,45862954>, r=58587653
pos=<19168448,33254095,39684567>, r=98395168
pos=<-6095859,46062067,73594989>, r=78078378
pos=<-304797,59105732,57302395>, r=69038374
pos=<55099745,36449947,86305322>, r=82314003
pos=<33473188,6098096,-4438050>, r=79487709
pos=<-31472777,36089404,51315048>, r=71202749
pos=<15962314,28423814,-7317040>, r=61433573
pos=<7998189,2801039,69890583>, r=65303215
pos=<130870385,104831832,11196492>, r=68126888
pos=<-5327566,58947831,12940865>, r=92989660
pos=<3824920,57094208,49926985>, r=55521839
pos=<-1717656,36673098,102114555>, r=92830763
pos=<-6542148,37528701,53564176>, r=49960544
pos=<-14811298,29054822,83783336>, r=79975290
pos=<-37201984,61523961,41570442>, r=98810391
pos=<20636169,30198837,103026121>, r=64914223
pos=<10371395,31519349,55590694>, r=62337567
pos=<31884149,40789942,72870652>, r=50003827
pos=<-30398435,29198407,42512686>, r=58738952
pos=<43939568,28346223,78157090>, r=54901870
pos=<9128846,67552702,83250788>, r=94000232
pos=<-2588856,30760371,91977867>, r=77653177
pos=<15962420,82995471,76760935>, r=96119428
pos=<-33661280,31365698,48813484>, r=66166075
pos=<-16115499,43852463,46308402>, r=58601848
pos=<-6987272,33841983,84781289>, r=77936013
pos=<-485860,34594456,81980122>, r=69386193
pos=<-15363694,37898885,53146470>, r=58734501
pos=<-17124380,71830052,45774058>, r=87053925
pos=<8219084,70226598,55923252>, r=70256199
pos=<-3903209,29453615,46544603>, r=75288840
pos=<-9159457,25097083,71581481>, r=61856037
pos=<-6436575,16618931,70357346>, r=66386656
pos=<-8806699,29246129,72352585>, r=62730916
pos=<68862343,36688765,49225208>, r=59235344
pos=<-79171684,112113447,67348279>, r=92995215
pos=<2638935,25190324,85791990>, r=64174419
pos=<14827988,29466545,48474491>, r=98818958
pos=<-14381380,32037768,62743833>, r=61488450
pos=<3154619,88011848,41382872>, r=85129150
pos=<51747013,31306559,79606148>, r=67118845
pos=<4782291,34063349,70169217>, r=51776099
pos=<-39091050,45657329,51675336>, r=88749304
pos=<-26634338,29826468,49344705>, r=58131168
pos=<28955639,11886714,83389681>, r=58803847
pos=<-13895377,39956378,49929456>, r=56106810
pos=<-20174467,38776446,18529455>, r=82076331
pos=<11908029,50316295,76200518>, r=66934331
pos=<79036827,16924288,76062556>, r=96520365
pos=<9584540,67893747,64596741>, r=75231780
pos=<1999297,28072699,90660851>, r=69059706
pos=<-4999516,1406744,49697518>, r=59502059
pos=<-12138046,52832626,53760320>, r=71057036
pos=<14911107,50532270,74102432>, r=62049081
pos=<-28076669,-5209886,42939079>, r=85888492
pos=<-1381979,35962916,89448258>, r=79118686
pos=<-27555180,38443112,78090455>, r=96414564
pos=<-45684157,22499678,51249216>, r=80645467
pos=<-33262343,42412693,21018032>, r=96311801
pos=<-24933943,30266769,36206604>, r=60648927
pos=<47087640,55749463,76069992>, r=83366515
pos=<40948562,39725308,77828570>, r=62961879
pos=<-21047756,32265813,60780681>, r=66419686
pos=<37924327,42452651,67877724>, r=52713763
pos=<-58689938,30781189,50489915>, r=92286565
pos=<13216582,64885410,61404468>, r=65398749
pos=<-43382614,29830485,70345833>, r=95884395
pos=<-20520219,36470699,75190567>, r=84507131
pos=<14237854,59338973,85949685>, r=83376237
pos=<-450228,37867875,64907958>, r=55551952
pos=<17760677,61033651,84995936>, r=80594372
pos=<22176975,82022403,46533128>, r=58704060
pos=<22779974,94329178,17857701>, r=95346900
pos=<-38001053,34049218,49600706>, r=73976424
pos=<16848379,29946291,48116237>, r=55533891
pos=<-42631692,35454176,53098888>, r=83510344
pos=<4649340,48178580,58429391>, r=54284128
pos=<847183,51607513,81674534>, r=84760407
pos=<-21437870,4973092,57201053>, r=79877495
pos=<-31919810,30134447,55487065>, r=69866771
pos=<-27341158,67125553,48484104>, r=95276520
pos=<34772295,-8690710,66519732>, r=68327958
pos=<-31969415,39408252,48343122>, r=72046242
pos=<102875003,13271850,46851582>, r=94799961
pos=<-12487583,35034265,55339911>, r=55187220
pos=<9093156,39769707,84017977>, r=67020115
pos=<-16380910,41574541,66165228>, r=76446125
pos=<78145797,37164108,49128463>, r=68897409
pos=<12696275,29248422,93349910>, r=62227566
pos=<3806267,80300805,32983373>, r=85166064
pos=<96882526,36942217,48757938>, r=87041670
pos=<-171249798,39499790,57712028>, r=97294519
pos=<-2013420,51530722,55656270>, r=61525906
pos=<10506453,22608309,43074519>, r=92987198
pos=<-12980027,57643628,73634339>, r=96583452
pos=<9759074,34807718,46383535>, r=76510831
pos=<-46481921,33185083,62928260>, r=94920705
pos=<-41982125,32478135,69194276>, r=95980083
pos=<6066850,16557724,-23317282>, r=96233732
pos=<-27420476,39804614,13027130>, r=95852751
pos=<-35598484,27994414,43416148>, r=61831662
pos=<53497255,74968025,34467126>, r=87786515
pos=<5629165,76030721,52476464>, r=75203884
pos=<-52112524,15110086,40185629>, r=92357875
pos=<-23905340,18683396,67786661>, r=79220265
pos=<1204692,65009004,78621903>, r=94751674
pos=<-10066560,46695198,48547405>, r=57634798
pos=<10289802,-10697128,21104545>, r=74844012
pos=<-40310292,28833507,46959457>, r=68428878
pos=<-13764816,49841026,61895287>, r=77826610
pos=<-19679027,58605601,66152788>, r=96763074
pos=<8809311,33356810,46152100>, r=63432984
pos=<-29709153,29461995,49058531>, r=60555118
pos=<-13129092,-9705209,46828679>, r=75874660
pos=<102118197,31480871,42422516>, r=84965146
pos=<-32517383,30565130,33503263>, r=71234788
pos=<9534874,52896280,70697014>, r=66384094
pos=<16880678,65311318,51702479>, r=52458646
pos=<-36132788,43209378,55775064>, r=87442665
pos=<22744530,34380596,134099136>, r=98061118
pos=<-26547963,41862023,60462772>, r=81198255
pos=<-545734,40978449,60677738>, r=54527403
pos=<16538063,68562231,28273188>, r=65405796
pos=<14494679,58745235,31512009>, r=54393346
pos=<7791169,67262375,35493930>, r=65632071
pos=<8485973,48520508,103076567>, r=95436538
pos=<8214274,28745216,50655896>, r=92680074
pos=<-6937397,27971689,49092605>, r=65350532
pos=<-22481435,9380958,40012423>, r=68629066
pos=<62129884,16597211,12546768>, r=80660497
pos=<6258688,62081024,87730516>, r=95878363
pos=<-32278381,28495170,53585234>, r=66684251
pos=<8040391,72063718,51083757>, r=67432585
pos=<-5673174,25046673,73686241>, r=60524423
pos=<-8001243,63756911,61002978>, r=85086692
pos=<-26577068,41949907,46813510>, r=67665926
pos=<27314118,87945331,52949180>, r=72667621
pos=<-20191543,30416348,7242015>, r=85020749
pos=<-40509210,25805511,61018374>, r=81933846
pos=<-27572422,16102428,56368833>, r=74050704
pos=<-11240828,50248391,27895560>, r=75248609
pos=<34613570,41153477,100115019>, r=80341645
pos=<15579024,9723842,69284527>, r=50193328
pos=<-46066212,37488646,47288596>, r=83168980
pos=<16651399,81480864,60218612>, r=77373965
pos=<-4490123,12256693,29836426>, r=57938091
pos=<-9722027,38414943,53203837>, r=53666284
pos=<-17608773,53047624,65044179>, r=88026024
pos=<-46201175,24018662,58511638>, r=86905837
pos=<83707615,22862590,47033198>, r=66223477
pos=<75278564,31837146,46877582>, r=58452382
pos=<66228245,28297748,1246309>, r=87068293
pos=<-41563889,138361221,22834552>, r=92320836
pos=<-15149458,41692211,57568440>, r=66735557
pos=<-29959760,29199939,50714596>, r=62199729
pos=<42142694,49570710,97616140>, r=93788536
pos=<61202922,61131481,71470186>, r=98263590
pos=<29827492,38051686,24415070>, r=56692483
pos=<17592777,73628928,77556835>, r=85918423
pos=<-20783874,35242533,54629155>, r=62981079
pos=<64237574,1302210,73405037>, r=94685683
pos=<-21188594,47968883,69462691>, r=90945802
pos=<30657831,49880981,-20723938>, r=95051093
pos=<22864844,93114017,56937539>, r=79512153
pos=<36531726,66041045,59031978>, r=66063917
pos=<19846259,-13074428,65015549>, r=64455431
pos=<-7113733,38738967,15168250>, r=72339348
pos=<-25341686,54431754,46334838>, r=78433824
pos=<20656999,57196763,75771625>, r=64636928
pos=<-22838713,34493030,27829494>, r=71157081
pos=<-27835555,38366205,61876949>, r=80404149
pos=<79754868,48144480,60613367>, r=92971746
pos=<20721233,45043536,93948762>, r=70596524
pos=<-46680111,37879564,53763782>, r=90648947
pos=<16009840,86125003,58588086>, r=81028766
pos=<20025736,5391570,85015946>, r=65810395
pos=<33720066,104264468,55486952>, r=97930498
pos=<-63554526,31193768,49947114>, r=97020908
pos=<-55699886,23371204,32454537>, r=95415201
pos=<73230725,36900143,48087090>, r=62677110
pos=<-32963671,28309367,52478944>, r=66077418
pos=<5939165,55024902,62755578>, r=64166779
pos=<-24917309,29082130,46444648>, r=52769524
pos=<7735621,49034186,60918429>, r=54542530
pos=<4877732,8430499,59198278>, r=52101779
pos=<83731770,35839732,49766995>, r=73797528
pos=<-11074931,33743791,62322816>, r=59467077
pos=<47699715,55835903,55060273>, r=63055430
pos=<37657191,-9104085,74776203>, r=79882754
pos=<-46744861,30151111,64292721>, r=93514232
pos=<-13186319,46003906,17086538>, r=83758581
pos=<32840031,56015193,64841941>, r=58156157
pos=<6062205,42632964,47615029>, r=73781872
pos=<-19799971,51835559,58790016>, r=82751306
pos=<-30893469,30656893,46300617>, r=60176438
pos=<-55830501,22240501,57274377>, r=97076042
pos=<15868849,46019629,96676898>, r=79153313
pos=<-16265649,52075245,75590161>, r=96256506
pos=<14624864,56207969,57618910>, r=51527552
pos=<318934,59504614,60032696>, r=71543938
pos=<-23487442,41093331,19776590>, r=86459237
pos=<67237678,41609284,51037661>, r=64343635
pos=<-19179700,33732993,56360250>, r=61598596
pos=<-34730837,5580174,61364913>, r=96727618
pos=<53061020,1668645,85754857>, r=95492537
pos=<3877306,43959248,85023639>, r=77431037
pos=<-10413451,33996655,77197477>, r=73933065
pos=<-15270541,18375898,55749133>, r=58855471
pos=<37599773,64150517,58789624>, r=64998919
pos=<6641382,32884368,86779494>, r=65348099
pos=<-28989825,39572241,63995218>, r=84883334
pos=<12972158,71582537,87975353>, r=98911274
pos=<41852603,48069590,65721204>, r=60103061
pos=<-12895423,34234299,51393776>, r=50848940
pos=<76683281,42692313,48821155>, r=72655746
pos=<9795431,-286238,55311954>, r=52014423
pos=<-12408641,28693619,67844073>, r=61271790
pos=<18776047,80030396,38475557>, r=64433947
pos=<75995398,33442514,24888484>, r=78338116
pos=<-22764877,22172460,58097441>, r=64901579
pos=<10239772,40250323,68821851>, r=51158213
pos=<-6984206,11263546,-3408779>, r=94670545
pos=<-25723496,46861709,48289286>, r=73199930
pos=<5729735,28403922,117678625>, r=92678286
pos=<11079147,38654325,86908021>, r=66808654
pos=<19012903,51490161,13432661>, r=60699507
pos=<-19708767,24665073,51422478>, r=52677853
pos=<2549542,80274103,46746108>, r=76796138
pos=<-48466554,29995042,56395431>, r=87182744
pos=<86083343,46696534,52688635>, r=89927531
pos=<19978558,94248897,26059694>, r=89865438
pos=<21928133,43444423,89266701>, r=63108468
pos=<-32662150,34466095,53616954>, r=73070642
pos=<-4767571,24202491,48306992>, r=52750940
pos=<-6049891,44411137,66256832>, r=69043725
pos=<14845730,-1633797,-11680131>, r=94009220
pos=<65363,73430740,49753549>, r=75444608
pos=<10919543,55374161,60523054>, r=57303105
pos=<-33344704,36874381,61740346>, r=84284898
pos=<90823294,32581102,47524970>, r=75388376
pos=<56762466,39329407,28284461>, r=61595785
pos=<14326905,38412743,-13806339>, r=79547110
pos=<-27408929,29564102,49418580>, r=58717093
pos=<72828690,34540465,56644790>, r=68473289
pos=<-23879984,-80533415,-13926388>, r=52569551
pos=<623363,23117915,46129959>, r=71699945
pos=<-21310687,28343530,48201114>, r=50180986
pos=<-11268087,54197123,60947343>, r=78738009
pos=<62637156,43374891,28845119>, r=70955785
pos=<101486863,29252465,58609523>, r=93807860
pos=<-7121743,-17618647,50972485>, r=81924604
pos=<33712412,33093334,104500328>, r=75765083
pos=<70057598,30520257,59928664>, r=64965508
pos=<13577885,32272356,41344051>, r=98076208
pos=<8955771,46905424,22660149>, r=56944295
pos=<-31671198,35282234,47777117>, r=67056086
pos=<-28701991,29968381,30147369>, r=70177802
pos=<46883174,34145057,15459578>, r=59357035
pos=<-21541824,-3738229,54506228>, r=85997940
pos=<-7165905,-13698956,55634555>, r=82711088
pos=<-42136026,37824576,54914702>, r=87201260
pos=<75444044,34090961,64356096>, r=78350113
pos=<27070208,76993574,17498441>, r=80354253
pos=<-24556265,40605071,29335340>, r=77481284
pos=<7679148,40322729,74549753>, r=59518815
pos=<64743952,34069862,51081505>, r=54354571
pos=<7888777,51012156,-6093924>, r=90872090
pos=<-38312643,6053130,45786004>, r=84257215
pos=<-10907447,30880694,71717384>, r=65831363
pos=<-21415341,33770607,55616361>, r=63127773
pos=<-9268341,12931923,58776983>, r=61325095
pos=<53680780,36878021,92991425>, r=88009224
pos=<16325289,32873290,52486621>, r=50056228
pos=<2379678,52409295,64101601>, r=66456683
pos=<12407570,48986679,9677595>, r=68556317
pos=<-23196672,47847216,47491952>, r=70861580
pos=<17092834,24513176,91350037>, r=55955816
pos=<8752629,37508985,49239757>, r=72585978
pos=<52794942,76928483,47757542>, r=81940023
pos=<12527582,-6613411,50632808>, r=50930382
pos=<-70810892,45621857,-48305364>, r=62008194
pos=<-18412210,36869794,57144258>, r=64752285
pos=<87656632,20763685,66883781>, r=92122439
pos=<61764769,33809145,-512978>, r=89875236
pos=<-21591807,57503330,27237264>, r=93512809
pos=<-8980322,48822102,24813214>, r=74644293
pos=<-13586166,29673545,54387395>, r=49972553
pos=<-21336027,12684379,50240160>, r=65103467
pos=<15312696,61249932,61078049>, r=59340843
pos=<3820682,55264026,85971673>, r=89740512
pos=<-8720944,7901096,48785694>, r=55817204
pos=<46676113,42744663,57067759>, r=50947525
pos=<63180707,54936070,46238435>, r=68814227
pos=<-59988823,31676130,52904181>, r=96894648
pos=<-55767575,34582277,42911408>, r=89093361
pos=<-24727493,22617024,22771261>, r=74880311
pos=<30402823,52581940,78464394>, r=65908330
pos=<-12446120,14245346,47969322>, r=52381926
pos=<13932111,12087077,52306416>, r=71083363
pos=<12361114,5008235,62369519>, r=51211858
pos=<62655763,37235551,16180822>, r=77498932
pos=<5158458,57101119,21414371>, r=72183092
pos=<2908218,33006742,52557556>, r=82743046
pos=<57439166,18173939,101222852>, r=98833513
pos=<52238468,31780002,45678724>, r=78491149
pos=<-45793920,10537217,50965371>, r=92433881
pos=<-13743952,64107054,54632417>, r=84808943
pos=<-170006460,33686904,57787874>, r=58950081
pos=<-26862218,29083237,47085032>, r=55356188
pos=<794530,52505491,57994889>, r=62031400
pos=<62469765,31544111,56755533>, r=55228669
pos=<33597386,32945456,56175425>, r=60893404
pos=<67662625,38142990,51575060>, r=61839764
pos=<-48808932,29332298,47264670>, r=77731645
pos=<-13897506,80448804,52473065>, r=99144940
pos=<-41499426,33500208,34694650>, r=81959864
pos=<21500730,1820261,108639134>, r=91529831
pos=<37273144,59821099,54577329>, r=56130643
pos=<-21429926,6902774,61057229>, r=81796239
pos=<3340434,50630706,26961101>, r=61983979
pos=<20112657,-7766263,32920789>, r=50273913
pos=<66869619,52342243,67598868>, r=91270061
pos=<6129880,42603189,67116575>, r=55915349
pos=<6398878,1688559,79467734>, r=77591971
pos=<14501207,33194081,103787164>, r=74805573
pos=<-41363813,41379031,51239687>, r=86308022
pos=<-5072359,37511009,73502277>, r=68411104
pos=<9979161,47203323,76860857>, r=66410591
pos=<8272810,70008182,48415971>, r=62477175
pos=<16131738,39072448,57020881>, r=66650334
pos=<-18513223,15536711,73318828>, r=82507011
pos=<-53344764,29150642,46822282>, r=81643567
pos=<-19488382,39476575,49309757>, r=60600153
pos=<17795545,86761153,34350936>, r=76269651
pos=<-39860391,37740470,42830687>, r=76425286
pos=<75357927,42659157,48122833>, r=70599033
pos=<67060727,20786860,43203283>, r=50745152
pos=<11307310,64243278,77782998>, r=83044472"""
