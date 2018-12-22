module E20 exposing (..)

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


type Direction
    = North
    | East
    | West
    | South


type RouteInput
    = RNode (List RouteInput)
    | RLeaf (List Direction)
    | Divider


type Route
    = Node (List Route)
    | Or (List Route)
    | Leaf (List Direction)


parseDirection : Parser Direction
parseDirection =
    Parser.oneOf
        [ succeed North
            |. keep (Parser.Exactly 1) (\c -> c == 'N')
        , succeed East
            |. keep (Parser.Exactly 1) (\c -> c == 'E')
        , succeed West
            |. keep (Parser.Exactly 1) (\c -> c == 'W')
        , succeed South
            |. keep (Parser.Exactly 1) (\c -> c == 'S')
        ]


parseRoute : Parser RouteInput
parseRoute =
    Parser.oneOf
        [ succeed RNode
            |. symbol "("
            |= Parser.lazy (\_ -> Parser.repeat Parser.oneOrMore parseRoute)
            |. symbol ")"
        , succeed RLeaf
            |= Parser.repeat Parser.oneOrMore parseDirection
        , succeed Divider
            |. symbol "|"
        ]


split : a -> List a -> List (List a) -> List a -> List (List a)
split a h res list =
    case list of
        [] ->
            (List.reverse h :: res) |> List.reverse

        x :: xs ->
            if x == a then
                split a [] (List.reverse h :: res) xs
            else
                split a (x :: h) res xs


testSPlit =
    [ 2, 1, 3, 4, 1, 5, 6, 1 ] |> split 1 [] [] |> Debug.log "split"


parseInput : Parser RouteInput
parseInput =
    succeed RNode
        |. symbol "^"
        |= Parser.repeat Parser.oneOrMore parseRoute
        |. symbol "$"


splitRoute : RouteInput -> Route
splitRoute routeInput =
    case routeInput of
        Divider ->
            Debug.crash ""

        RLeaf dirs ->
            Leaf dirs

        RNode nodes ->
            split Divider [] [] nodes
                |> List.map (List.map splitRoute)
                |> List.sortBy List.length
                |> List.map Node
                |> (\x ->
                        case x of
                            x_ :: [] ->
                                x_

                            x_ :: rest ->
                                Or (x_ :: rest)

                            _ ->
                                Debug.crash ""
                   )


test1 =
    inputtest
        |> Parser.run parseInput
        |> H.uR
        |> splitRoute
        |> Debug.log ""


type alias State =
    { currentPoint : ( Int, Int )
    , doors : Set ( Int, Int )
    , lengths : Dict ( Int, Int ) Int
    }


traverse : Route -> State -> State
traverse route state =
    case route of
        Leaf [] ->
            -- { state
            --     | points =
            --         state.points
            --             |> Set.insert state.currentPoint
            -- }
            state

        Leaf (d :: rest) ->
            let
                ( newPoint, newDoor ) =
                    case d of
                        North ->
                            ( ( state.currentPoint |> Tuple.first, (state.currentPoint |> Tuple.second) - 2 ), ( state.currentPoint |> Tuple.first, (state.currentPoint |> Tuple.second) - 1 ) )

                        East ->
                            ( ( (state.currentPoint |> Tuple.first) + 2, state.currentPoint |> Tuple.second ), ( (state.currentPoint |> Tuple.first) + 1, state.currentPoint |> Tuple.second ) )

                        West ->
                            ( ( (state.currentPoint |> Tuple.first) - 2, state.currentPoint |> Tuple.second ), ( (state.currentPoint |> Tuple.first) - 1, state.currentPoint |> Tuple.second ) )

                        South ->
                            ( ( state.currentPoint |> Tuple.first, (state.currentPoint |> Tuple.second) + 2 ), ( state.currentPoint |> Tuple.first, (state.currentPoint |> Tuple.second) + 1 ) )
            in
            traverse (Leaf rest)
                { state
                    | doors =
                        state.doors
                            |> Set.insert newDoor
                    , currentPoint = newPoint
                }

        Node rs ->
            rs
                |> List.foldl
                    (\route state ->
                        traverse route state
                    )
                    state

        Or rs ->
            rs
                |> List.foldl
                    (\route state_ ->
                        let
                            state__ =
                                { state_ | currentPoint = state.currentPoint }
                        in
                        traverse route state__
                    )
                    state


getPossiblePoints : ( Int, Int ) -> State -> List ( Int, Int )
getPossiblePoints ( x, y ) state =
    [ ( ( x - 1, y ), ( x - 2, y ) ), ( ( x + 1, y ), ( x + 2, y ) ), ( ( x, y - 1 ), ( x, y - 2 ) ), ( ( x, y + 1 ), ( x, y + 2 ) ) ]
        |> List.filterMap
            (\( door, newPoint ) ->
                if state.doors |> Set.member door then
                    Just newPoint
                else
                    Nothing
            )


makeLengths : List ( ( Int, Int ), Int ) -> State -> State
makeLengths intIntList state =
    case intIntList of
        [] ->
            state

        ( p, l ) :: rest ->
            case state.lengths |> Dict.get p of
                Nothing ->
                    let
                        newPoints =
                            getPossiblePoints p state |> List.map (\p -> ( p, l + 1 ))
                    in
                    makeLengths (rest ++ newPoints) { state | lengths = Dict.insert p l state.lengths }

                Just _ ->
                    makeLengths rest state


getAnswer1 : State -> Int
getAnswer1 state =
    state.lengths |> Dict.values |> List.maximum |> Maybe.withDefault 0


getAnswer2 : State -> Int
getAnswer2 state =
    state.lengths |> Dict.filter (\_ v -> v >= 1000) |> Dict.size


test2 =
    inputtest
        |> Parser.run parseInput
        |> H.uR
        |> splitRoute
        |> (\r -> traverse r { doors = Set.empty, currentPoint = ( 0, 0 ), lengths = Dict.empty })
        |> makeLengths [ ( ( 0, 0 ), 0 ) ]
        |> (\s -> ( getAnswer1 s, getAnswer2 s ))
        |> Debug.log "answers"


inputtest : String
inputtest =
    "^N(NNSS|)NNN$"


inputtest2 =
    "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"


input : String
input =
    """^NWWWNNNNNWNWNWSWNNNNENWNWSSWWSWWSEEENESSSWSSEESWSEENNE(NWWWEEES|)ESSSW(WSESE(N|ESWWWWSESWWWWWWNENNESEE(SWWEEN|)NWNN(ESE(SEWN|)N|WWNWSWNWNEENNWSWNWSWWNNWSWNNNWNEENEEESSENNE(ENNNNWNENEENWNNNEESESW(WNSE|)SEESENENWWNEENESESWSEESSENESSWSSSEEESSENNESENNEESWSSWSEEEENNESSENENNEESSW(N|SWSSWWWSWWSSWSSWWSWSSSESEENENWNN(EEEENNEEENESSSESSENNNNNW(NNENWNN(WSWSSE(SWWSWWN(NE(E|S)|WWSESWS(WNSE|)E)|N)|NNEESWSEESSSSEENNESESWSWWWW(SSEEN(EESENESENNW(NNNNEEEEENEESESWW(WWSSWNWN(WSSESESSSW(WWSSWWSWNNWN(WWSSE(N|ESSSSWWNNWWN(EEESSNNWWW|)WNNWWSESSESSENESSEEEEENNN(WSSNNE|)EESSSEEESWWSESWSEESSWNWSWWNENNNWWWSEESWWWWNENN(EEE(NNN|E)|WSWNWSSWNWNENWWSWNNN(ESNW|)WNWWNW(NN(W|ESENE(NEN(ESNW|)W|SS(ESNW|)W))|SSESE(N|ESWWSWSSWWSSENESSWWWWNWNENWWSSSESWWSSENEEEESWSWSESSWNWNN(NEWS|)WSSSSE(N|SENEESWSWWSSESWWWNENNNWNWSWWNNE(ENWWNEEE(NWNWSWNNEENNNNNNNES(SSSS|ENEES(W|E(NNESSENNENWWWWN(EEENSWWW|)WWN(E|NWSWWSESWWWSWWSSSESSENNNN(WSNE|)ESSE(NNN(EEEE(NWWNSEES|)E|W)|SWSESSWNWWSWNNNWNWNWSSWSSENE(SE(N|SWSWSWNWN(EE|NWNNNE(SS|NNNNEESWS(SS|EEE(NWNN(WWWWWWWSWNNWNWSSSSWWSSEEEEEEE(SWSWSWSEEE(SESSESSENESSSWNWSWSSSSWWNNE(NNNWNNE(ES(W|S)|NNWNWWWSSSSESE(SS(ENSW|)WSESSWWSEEESENESEENWNNESEEEENWNNESESSSSENEENN(EESWSSSSESSESSSWNNWNWSSWSSSENE(SSESSENESSWSSEEN(W|EENWNEEESWSSESEESWWWSSENEEENEEESENEEESWWSESWSEESESWSESSENNENESSS(WNSE|)EEEENEEES(EEEEENNWWWW(SEEEWWWN|)WWWNWSWSWWNNE(S|ENNEEES(EEENEESWS(EENESESWSSEEN(NENNW(WNNNENNWSWNNENWWSWWWWSWSWNNWSSSEES(SENEEENWNEE(NWWWSWSE(WNENEEWWSWSE|)|SESSWN)|WWWWWNENWNEE(NNNNESEES(S|EEEENNNENWWSWNWNWNNEEENWWWNEENWNNESESENNEEESENNWWNWSWNWNEEENWNWWNNNWNNESESSENNNWNWNNWSSWSWNWSSWNWNWSWNWNNENN(EEEEESWWSWS(E(ENESENNEEEESWSESEEENWNENEESWSSENEENNW(S|NNNWWSWS(EENSWW|)WW(S(SS|E)|WWNNNNNWWNWNWWSSSW(SSSEEEES(W|ENNWNENWWSSWN(WSNE|)NN(N|E))|WNENNNNWSWWNNE(ENNWN(EESSSESSEEEES(W|EENENESSWSSEEN(NESEENEENNNNNESSENNNEESWSEEENNENESSSSENNE(NWNNNE(NWWSSWWWNENWNWNWWNNWWWNWWNENNNEEENNEENNWWWS(EE|WWWNWNWWSSWWNNNWWWNWWWWNWNNWWWNWWWSWSWSWSESEEESESWWWWN(WNWNWSWNWSWWSESSSENENN(WSNE|)EESSE(N|ESEESENEN(WWW|ENNNWNWNW(WSEWNE|)NEE(NWES|)SESEE(ESSWNWSSEESSSSESWWNWWN(EN(NESSNNWS|)W|WSSSESS(EENN(ESE(SWEN|)NENNNNW(NEEEESSESESSWWS(SENESESWSSEEESWWWWN(WSWWN(WSSEESSWNWSW(SESEEEN(EENWNEEEN(WWWWSS|EESWSWWSSEESESENNWN(WW|EESEE(EENWNNENN(WSWNNWWNE(EESENSWNWW|)NWWSW(SSE(N|SEE(NWES|)SS(SENN|WNWSW))|WNNNW(SSSWENNN|)NEESS(S|ENNENWNWWS(WW(NENWWN(WNNN(ESSNNW|)WSWWWWNNESEENW(ESWWNWESEENW|)|EEEEE(NWNSES|)ES(EEENWW|WWW|S))|SS)|E)))|EESWSS(W|EE(S(WW|E(SS|E))|N(N|W))))|SWSESSWWWS(WNN(WNWNWWW(NEWS|)SWWSEEEE(NWES|)(E|SW(WWWWNNWNEEE(WWWSESNWNEEE|)|S))|EE(E|NN))|E(EE|S)))))|WW)|NN)|E)|NN)|WNNN(ESEWNW|)W(NENW|SSSW))|S)|W(NWES|)S)|WWS(ESNW|)WNW(NEE(E|NNWN(WNWSWWS(WNNEENWW(WSWWS(WNNWSWNNNE(ESWENW|)NNWSWWSSWNNNENNEENESENNNNNENNNNNESEENWNNEEESW(SSEEENNNW(SSWENN|)NWWNWNWNWWWWSEEESES(WSSWNWNN(ESNW|)WWWNWWSSSSWSWNWNENWWWNNWWSESSWWN(WWNWWSESEESWWWNWWWWSSSWSSWSSWWSSSSSSWNWWSESEEENNNNEES(ENNNESSSEES(ENNWNEENNEENWNNWWSS(ENSW|)SWW(NNE(NNN(NWSSSW(WSESWS(E|W(SSWNWS|N))|NNN)|ESEEN(ESSENNNEESEE(NNWSNESS|)SE(N|ESWSSWWSEESESWSSESWSESWWWNNNWSSWS(WNNWNEE(NWWW(SWNSEN|)NNESEENE(NNWSW(S|NN(W|EEEN(ESNW|)NWSWNN))|ESWSEE(SSS|NN))|S)|SE(N|EEESWSW(SESS(WWNEWSEE|)ENNEN(E(NWNENNENNEENWWW(W|NEEEESENNWNNNNWNNENNWSW(SSW(WSEESWSESWW(WNENWESWSE|)SEEENNN(SSSWWWEEENNN|)|N)|NN(EEESSSS(SSE(SWSNEN|)NNNN|W)|N))|SS)|SEEE(NN|S(WWWWSNEEEE|)SS(SSWNSENN|)ENESES))|W)|N))))|W))|S)|S(EE|SS))|W)|WSSSSWSWWWN(EE|WNWNWSSSWWSESWWNWNENWNNWWWSSWWWNNWSWWSWNNWSWWNENEEEE(SWEN|)NWNNNNEENWNWWWWWWWNNNNNWWSWWSWNWWSWNW(SSSSEEEEESSWSSENEESESSWNW(N|WWSSWWNENWNWNENN(WSW(N|SSSESSSEESEEESSWWWN(WNWSSW(NNNNN|SESSSW(NN|SEEENESSWWWSESSWW(N(NN|E)|SEEEENNESSSWWSEEENEEESESWSSWSWWWNWNEESENENNWSWWWWSWWW(NNNEESSWN(SENNWWEESSWN|)|SESENESESWWWSW(SSESSW(N|SSESWSEEEENWNW(S|N(NENESESW(SESENESSSSESSENNNESSEESSENESESSSESWWWNENWWSSWSEESWSSWWWSWNNNWWSESWSSWSESSWSSSWNNNN(WWWSSEE(NWES|)SSWNWSSSW(NNNNNNNNNEESWSEENNNWNNWSW(SEWN|)NNNNEENENESSSW(N|WS(WNSE|)EEE(NENEEEE(SWS(WNWSWENESE|)SE(ESWWEENW|)N|NE(S|N(EEE|WWWWWS(EEE|W(NNWNENE(ESS(WNSE|)EEENES|NNW(NEWS|)SWSWSSWWNNW(SSSE(SWEN|)E|NEE(SS|EN(E|WWW))))|S)))))|SWWSEESSW(N|SS)))|SSESEEESWWSWW(SEEESWWWSSESSSW(SESSSW(NN|SESEES(WWWNSEEE|)EENNW(S|WWNEEEESSEENNNESEESSEEEENENWWSWNW(NWNNNWNEEEENNWNENWWNNENESS(EESWSSEEESESWWNWWSSSEN(EEEESWSSSENEN(W|NNNNW(S|NEEEEESWWWSSEEEN(WW|ESESWSESWSEEENNW(S|NEEESWSSESWWWWSWS(EEEN(ESEEEENNWW(NN(W|NNNESEENEENWWNWWWNEEENENESSS(WNSE|)EEENWWNNENWWWWWS(E|SWNNNNEEES(ENNNWSWWWNEENEEENWWNWNEENNWSWWSWSES(WWS(SWSWSESE(SSSWNNWNWNWSSSWWS(WWWNWWNENWWSSWNWSS(E(S|EEE)|W(W|NNNENNNNEESWSES(WSNE|)EENENEENENNWWNNEENNNNNWSWWSEESWS(WNWWWNEENNEENENNNEEESWSW(N|SESSEENNE(NW(WSSNNE|)NNNEE(E|NWNWWWWSES(ENEWSW|)WWNNNNESEENNNENNWWWSE(E|SWWS(EESNWW|)WSWWSSSSWNNNWNNNWSWWSS(SSEESE(NNWNW(NEWS|)S|SSSENEEN(NE(SSSWSW(WSWWN(E|NNWSWWNNN(EES(WSNE|)E|WWSESWWSSSESENEN(WWNEWSEE|)EESSW(SEE(NEEWWS|)SSSESWWWSS(SSSSWSWNNWNNNNENNWWS(WSESSWSWWWWWSESWWW(NENNNESENEES(W|EN(NWNNE(NNNEE(SS(W(N|S)|ENNEE(SS(WNSE|)S(SSSSW(SESNWN|)N|EENNWS)|N))|NWNNWWSESWWWN(E|W(SSSSEE(SSSWNNWSW(NN|S(E|WWSESWWNNWSS(SEEEWWWN|)W))|ENNWWSE(WNEESSNNWWSE|))|N)))|S)|E))|SESWW(NWES|)SESWSSWW(NENWWEESWS|)SEEENEEEE(SWSESS(WNWNNWSWSWWN(E|WSSSENEES(EE(NWNSES|)SS|W))|E(N|S))|NWWW(W|NEEEEE(ESWWSEEE(WWWNEEWWSEEE|)|NWWWNEN(WWSSWENNEE|)NEN(W|ES(SSWN|ENEN))))))|E)|EEN(W|EENNN(W|ESSE(N|SWSS(ENE(S|E)|SW(NNWESS|)S)))))|N)))|N)|NNW(NEWS|)S)|WW))|WNNNNW(S|NEENNNWNNWWWNWWWNENNWSWNWNWSWWS(EEESESSSSS(ENNNESSSEENEE(NWWW(N|S)|SWS(EENSWW|)WWWSEESEN(SWNWWNSEESEN|))|W(NNNWWNN(ESEWNW|)W|S))|WNWN(WS(W|S(S|E))|EEEN(W|EEENESENEEEEENNWWWS(EE|WWWNEENNW(NWNEEENENWWNENWNNWNWSSSSWNNWNNE(NNWNNEE(NNNW(NEE(EEENNWNENWWN(WSWSSS(ENNESSE(WNNWSSNNESSE|)|WNNWNWNE(NWNENN(N|WSW(S(WWN(W(WWSEEWWNEE|)N|E)|SSS)|N))|E(E|S)))|EEE)|S)|WWWS(W(NNEWSS|)W|ES(ENESNWSW|)SSSS(SWSSW(SES(WWWSSNNEEE|)EN(NN|ESSWWSEES(ENEE(N(NNN|WW)|E)|WW))|NNWNWN(EESENSWNWW|)W(SS(S|E)|N))|E)))|EEESSWWN(W(W|SSEEEENEESSESESWSWSSW(NNNN(ESNW|)WS(SS|WNN(EENSWW|)W)|SEESWSSENEENWNENENEENENENEEN(WWNWWWNN(WWWW(W(NEWS|)WW|SEESS(WNWESE|)E(NN|SSS(ENNNES(ENSW|)S|W(NN|WS(SWNSEN|)E))))|EE(NWNE|SW))|ESEENNW(S|NEE(NWNSES|)SSSEENNE(NWWSSNNEES|)SSE(N|SWWWWWWWS(WSWW(NENEWSWS|)WSWSSESSSWNWW(SESWWNW(NEWS|)SSSW(NNNWWNWN(ENWESW|)W(SSEWNN|)W|SSSWWSESS(WWN(E|WWNEENWWN(WWW|EEN(EESWENWW|)WW))|ESENNN(WSNE|)EENWNENNN(WSSWSS|ESSEENWNNESENNEENE(NWNN(ESNW|)WSW(SSENSWNN|)N(NEEWWS|)W|SEE(NWES|)EESWWWWWWSEESENESSENNEE(NN|ESESS(ENNEEWWSSW|)WW(SEESWENWWN|)W(NE(NWES|)E|WWWSESSSWNNWNWWSESSWWWNWW(WWSEESWWSSW(NWES|)SSESWS(WSEWNE|)EENENESSWSES(WSNE|)ENENWNEE(NWWNWWNEEESEENNN(E(NN|SSSEES(ENEE(SWSSW(N|SSWSSEN(SWNNENSWSSEN|))|E)|WWW))|WWWS(EESNWW|)W(WWSS(W|S)|NN))|S)|NENEN(WWSNEE|)E(SS(SEWN|)W|ENE(NWWSNEES|)E(SWEN|)E))))))))|NE(NWNENSWSES|)E)|E))))))|E))|S)|S))))))))|SS(WN|SE))|SSSWWSESWW(SEESSESWWW(SSENEE(SWSWWW(N|WSSS(EENWNE|SWNNN(WSSNNE|)N))|ENNENWNW(S|NNENEE(NWWEES|)SES(WW(SEWN|)(N|W)|ENN(W|ESENESSESSW(N|S(EENESSWSESEESE(ESWSESWSWNNNWNWWNW(WSESWSEESSENN(NWES|)ESSSW(WW|SESSW(SESSEEENENNWNEESSSSSESSWWN(E|N(WSWNWWWWWSSWSS(WNNWW(SSENSWNN|)NWNW(NEEES(W|EE(SWWEEN|)NENE(ESWENW|)NNWWS(SW(W|S)|E))|S)|EEN(ESENEN(NWWWSEE(WWNEEEWWWSEE|)|EESWS(EENEEES(ENNNW(S|NENESSSSEEENWWNNNESEES(WW|EEESESS(EEEENWNNEN(ESS(SSENSWNN|)W|WWWS(SSENNSSWNN|)WNNEENWN(EESSNNWW|)W(NEWS|)SWSSWNNW(NEEWWS|)WS(ESNW|)WNWWNENWNWSWWSW(NNEEN(EEE(NWES|)(S|E)|WWWWNW(NENNE(NWNSES|)SESWSE|WWSSSE(NNEWSS|)S(SWNSEN|)E))|SE(EENW|SW)))|WWWW(WWWW|NEE(NWWEES|)E))))|WW)|W))|W))|N))|N))|NN)|NNWNNE(NNWSW(NNEENWWN(EEEE(NWES|)SS(E|W(N|SS))|WWNN(EESWENWW|)WSSSEESSWN(SENNWWEESSWN|))|SSS)|S))|W))))))|NN(ESNW|)WW)|NN)))|E)))|ESS(WNSE|)EEESS(EN(EEESNWWW|)NNWNNW(SSWNSENN|)N|WWN(E|W)))|NN)|EE)|E)|WW)))|S(E|W))|W)|WWWWWWWNWW(NE(NNW(S|N(NESNWS|)WSWNWW(SEWN|)WNEENWW(EESWWSNEENWW|))|EEES(W|EENESEN(NWWW(NENWNEEESSWN(SENNWWEESSWN|)|S|W)|E)))|WS(EE|WWWWWWW(NENWESWS|)WWW)))))))|N)|W)|S)))|NN)|N(E|N)))|E)|W)|W)))|NN)))))|EE))|E(E|SSS)))|NNNENESSS(WNSE|)ENNEENEENEEESWWSES(WWW(NE|SWWNE)|ESESSW(SSEEEEENWWW(W|NNEES(W|ENNENENNNWWNWSSSWNNWNW(SSS(W|SE(ES(W|ENENNES(NWSSWSNENNES|))|NN))|WWWWWSWNWWWS(EESWENWW|)WNNW(NNENNEENNENNWWS(E|WSS(ENSW|)W(SS|NNNEN(W|EEEEEEESWSWSW(NNEWSS|)SESWW(N|SWSW(N|S(W|ES(W|EENN(WSNE|)NEENESSWWSES(ENEES(W|EENEENEENNNESENNWNEEEENWN(EEEESEESSWNWSWW(NENWESWS|)WSESESSESSWNWWWNEENWNWW(NENWESWS|)S(WWSEESSWS(ESESWSESSWNW(NNN|SSESWWN(WSSSENESS(W|SEEEESESSSEESS(SSESENNWNNENWNNEEENNWSWNWWS(E|SW(SEWN|)NNWNNWWWS(WNNENNEENESSS(WWNEWSEE|)ESSENENNNNWW(SSE(S|N)|NWNNNEN(WWW(WNEEWWSE|)SSSE(SWSNEN|)NN|ENNEEENEESWSSENENNNWWNNNNNEN(WWW(WW|SSE(N|SSW(N|WWSW(NWES|)SEES(W|ENN(ESNW|)W))))|EEEEESWSWNWWSSE(N|EESSWW(SSSSSEENWNEEENNNW(SSWWNE|NNNESSENNESSSWSSEEENW(W|NENNNEEESWSSENESSENEESEEENWWNNENNN(WWSESWWNNWWS(WWN(E|WSWW(N(WSWNWS|E)|SSS))|SSE(EESWWEENWW|)NN)|ESEEN(EEEESEESSWSESENNESEESSSSSESS(WNWWWWNNWSSWS(S|WNWNEENNNE(SEEEE(SSWNWS|NNWSWW)|NWNNN(WSWSWWSW(SESES(ENE(S|NN(WWSEWNEE|)N)|SSWS(EE|WWNNE(ENWNWSWNWSWNW(N|WSWSSES(EENESE(NNWWW(N|S|W)|SSEEE(NWWEES|)SS)|SSWNWN(E|WNNWN(WWSESSWSE(E(E|NN)|SSWNWWWS(EE|SSSWNWNENNWNWWN(W(N|WWWSSWNWW(NENE(ENWWEESW|)S|WSEES(W|EEEESESSSEESSS(ENNENWNWWNENN(E|W(S|WN(WWNEEWWSEE|)EE))|WSWW(NENWWNWSW(NNEENE(SSEESNWWNN|)NWWW(SE|NEE)|W)|SEESWSS(ENSW|)WNWW(SESWWEENWN|)N(W|EE))))))|EEESENENN(WWSEWNEE|)NESS(SSWENN|)E)))|NN(NNN|ESE(S(W|SS)|E|N))))))|S)))|NW(NNESEEN(EN(E|W)|W)|W))|E)))|SENESES(EENEESSW(N|SSWSW(SESSESSWWS(WWWNEENWWWSWWSSWW(NENNNNEES(WSEWNE|)ENESEEE(SE|NWW)|SSES(ENN(ENE(NWES|)EE(SWSW(WSEWNE|)N|E)|W)|WSWWWWS(NEEEENSWWWWS|)))|EEENEENWWNNWNNEENESSEEENENENWWSWNNENWWNENNESEEENENWWSWNNWNWWNEEESESENENEESEESWSSWW(SEEEEEESSWWN(WSWSEEESWSESEEESENESSENENWNENNEENNWWNNWSWSWWSSW(WSEEENES(SWWWWEEEEN|)ENN(WW|N|EE)|NNNNWNWW(WSEEWWNE|)NENWNNWNENWNEEEESSEEESSSWSEEES(EEESESEENE(NWNENNNWNNNNE(SSS|NWWSSWNWSWNN(WWSSSE(ESESWSWS(SENESESE(NNN(ESNW|)NNNW(W|SSS(W|S))|S)|WWNENENWW(S|NWNNN(ESSNNW|)WWS(ESSWNSENNW|)WNWSWNWSWWSWSESS(SEWN|)WNWNWSWS(EE|W(SEWN|)NNNNWN(WSWSESE(N|SSWNWSWNWSWWNENNWNWSWSSE(NEWS|)SWSESEESE(NNWWEESS|)EES(E|WSWS(E|W(SSSS(WSNE|)EN(ESSSEWNNNW|)NN|WWWW(NEENWN(NWNNWWWW(NNWWNEEENWN(WSWWSWNN(EE|WW)|EESENEE(SWSWSESWWN(WSNE|)N|EEEES(SSE(NNNEWSSS|)E|W)))|SSEESSW(NWES|)SEE(NN(ES|NNWW)|S))|EESSENE(WSWNNWESSENE|))|S)))))|EEESWS(S|EENEN(W|EE))))))|NN)|EE))|SSSSWWSSSSENNE(NWES|)SSSWSWNWSWNNWSSWN(WSSWNWWNWSSWNNWN(WWSSSESE(NNWNSESS|)SESWSSESWWNWNNWWSSE(SSS(W(NWWNNE(SEWN|)NNNWSSWSWWWNNEE(SWEN|)ENWNWNNNW(SSSWNNNWSSSSS(WWN(WSNE|)ENNNWN(W(NWES|)S|E)|SSE(NNNEEWWSSS|)SEEEES)|NENENEENNNN(W(N|SSSWNNNWSW(NW|SE))|E(E|SSSSSSEE(N(NW(NEWS|)S|E)|SSSSW(SEE(ESSNNW|)N|WWNNWNW(SSESNWNN|)NEE(NWES|)SEESWSE(WNENWWEESWSE|))))))|SS)|EN(N|ESEEES(SWNWESEN|)EEENENWNNWWSESSWNW(NNN(W|NENEN(NWSW(S|W)|ESSS(W(N|W)|EESENNWNN(WSSNNE|)NESSENESENEN(WWW|E(SSSSWWWSESESWWWN(NNNEEEN(SWWWSSNNEEEN|)|WSW(NWNSES|)SS(SSESSESWSESENNNNWNEEENNE(SSSWWSEESWWSSEE(NWES|)SWWSSSWSS(EEN(NE(NWNEWSES|)SS|W)|SS|WNNNWS(WWNENEEE(S|NWWWWSWNNWSSWNNN(WSSNNE|)EEENE(N(WWSNEE|)N|SS(E|W)))|S))|N(NNNWESSS|)WWSWNWWSSE(EE|N))|W)|E)|N)))))|WW)))|N)|E)|NNENWNEEE(NNW(N(NWWNE|EESEN)|S)|SS(WNSE|)S)))|WWSWSWNN(E|WNWNNWNNNESSESE(ENWESW|)S(W|S))))|E)|NN(ESNW|)(W|N)))|NNENWW))|WW))|W))))|WN(EE|W(NN|S)))))))|EE))|WWN(WSWNNENNWWWN(WSWWSSENESENE(E|SSSSW(SEWN|)NNWWSE)|EE)|E)))|N))|WNW(W(NEEE|WWS)|S))|E)|WSWWN(WSWNWSSSE(ENWESW|)SWSWWWSS(EEN(EENSWW|)W|S|WNW(NNWN(WSNE|)EENW(NEN(W|ESSSS(WSNE|)EENWNENWNE(WSESWSNENWNE|))|W)|S))|E)))|W))))))))|SSS))))|N)))))|E)|E)|W)|EE)|NN)|EEES(WSNE|)E)|E))|SSS)))|NWN(W|E))))|EE))|SS)|SSSSWSSSWSWNNENNWN(EE|WSS(E|WSESSSSWWSWSEENESSESWWWN(E|WSWNWW(NWNNEES(W|SENNNE(NWNWWN(ENNNEEEN(E(EENNSSWW|)SSWSSS(ESSWNSENNW|)WNWNNES(NWSSESNWNNES|)|W)|WSSS(ENESNWSW|)WWWWWWN(W(NNWWEESS|)SSES(ENEWSW|)W|EEENESEN(SWNWSWENESEN|)))|S))|SSSEN(ESENESSSWNWWWSSESESWSEENENWN(WNWESE|)EESSSSENENWNNESENESE(SSWNWSSSSSWWSESSENNESSE(NNNWNNNESS(NNWSSSNNNESS|)|SWWSESSWNWSWNWWWWWNEENWWNENEENE(NNWWWSE(SWWSWSWWWSWWWN(EENENWNEENWWNEEENWNENN(ESSSEES(WSSWN(WSS(SWNSEN|)E|N)|ENE(N|SEESENN(SSWNWWEESENN|)))|NWNN(NESSNNWS|)WWSWWN(WSSSESEEE(SWS(WSW(SEWN|)NN(WNWWEESE|)E|E)|N(ESNW|)WWN(EENSWW|)W)|E))|WW(NEWS|)SW(W|SESESESSENESSWSS(ESS(WNSE|)SSSESWS(WNSE|)ESEEESWSWNWSSWSEENEEEN(EEEENENENEENE(NNNWSSWSWNWNNESENNNWNNNNN(WSSWWNENNWSWWN(WSWWWSESSWSSSENNESESWSSWNWW(NNNN(W|N(NNNENN(WWS(W(SEWN|)NW(WNSE|)S|E)|NEESSW(SEWN|)N)|E))|SESESWSEENESEENEE(ESWSW(SWWN(WSNE|)E|N)|NWNENNE(E|NWNN(ESNW|)WSWNWW(WNNESENES(NWSWNWESENES|)|SESEESWS(SSW(S|W(W|NNE(N|S)))|E)))))|E)|ESSSSEE(SSWNSENN|)NNW(NEN(NNN|W)|S))|SSSWNWSWSESS(ENNESS|WWWWWW(NEEEE(N|E)|WWW)))|W)|WNNW(SWSEWNEN|)NNWWN(WSWNWESENE|)(N|E))))|E)|SSSW(NWES|)SEEENWNN(SSESWWEENWNN|)))|NNNNWNNE(NNWNENWN(WSW(N|SES(W|S(E|SSSWWW(NEEWWS|)SEEES(ENSW|)WWW)))|EN(W|NNN))|S))|N))))))|W))|WSS(E|WW))|S))))|S)|W(NNEWSS|)W)|WWS(SSSWSSSEEN(WNEWSE|)EEESWWSSWNWWWSSESEN(NWES|)ESSWWSWWNWSWNNWNEENE(NNENNNWSWS(E|WSWNNENNE(EEENNNWWSWNWWWWNWSWNWSWSEEEEESWWWWSSWNWWSSE(N|ESWWSW(NNNW(SS|NENNN(WWW|ESE(NNENNENWN(WSSSNNNE|)NESESSS(ENN(N|ESSEES(EN(EES(ENESNWSW|)W|N(WW|N))|W))|W)|S(E|W))))|SSEEENESSSSEESEEE(SESSSESSSEESWSSEEENW(NENWNENNESEEESEE(SWSWNWWN(WSSSSESSE(SWSWS(EENSWW|)SSSSSS(ENSW|)WWW(S|NWNENESSENNNNWNWW(WNNENESES(E(NENNWW(SEWN|)WWWNNWSWSES(E|SWSS(ENSW|)WNWWNEEN(E|NWSWWSWNWNW(NNWNNWSSS(WNWSW(NNNNW(NENENNNN(NNESSSSSESEESWS(ESSESEEEENNWSWNNNNWSSS(S|WNNNNNENN(WWS(E|WW(SEESWENWWN|)NNE(NWN(EESE(SWEN|)N|W(WW|N))|S))|ESSS(W|EEESSS(EN(ESSS(WW(NEWS|)S(E|SWWWS(WNWESE|)E)|E)|NNNWNWSWN(SENESEWNWSWN|))|WNNWSS))))|WWN(WSSSEN|N|E))|WSSSW(S|NNNW(SS|W)))|SWWSEESW(ENWWNEWSEESW|))|SS)|E)|SS(W|SE(ESE(N|EE)|N)))))|S)|WW)|SES(WSWSNENE|)E))|NNN(NWSNES|)ESES(EE(ES(SEENWESWWN|)WW|NWN(W|N))|W))|E)|NNNNNNWNNENEENN(WN(ENWESW|)WWSS(E(E|N)|SWNNWSSSESSWW(N(E|NN)|WWSESES(ES(W|ENE(NWWN(W|EE)|SS))|WWNWNWSSW(WNENWESWSE|)SEE(SWSWNSENEN|)N)))|ESSS(EE|WWS(SS(SSS|ENNESSES(NWNNWSNESSES|))|W))))|W)|NWWNENNN(EE(SW|EN)|NWNWNW(WSES(WWWSWENEEE|)ESSS(ENNSSW|)W(SEWN|)NN|NNEES(W|E(S(W|S)|EN(NE(NW|EEEN)|W))))))))|S))|SS(ESNW|)W)|E))|WW)|SS))|S)|W)|WWWW)|WW))|WW))|N(N|W))|WSWNNNWNENWNNWNW(SSESSWW(N(NNN|E)|SSW(NWWSE|SE))|NEESE(NN|SS)))|NNWNE(NWES|)E))|S)|N(W|N))|ENWNW(NEESNWWS|)WS(WW(N(NN|E)|WW)|E))|ESENNEN(EE(SSW(WSNE|)N|NE(NWES|)E(SWEN|)EENWNENENWNENWNNESESE(SWSEWNEN|)NE(S|NWWN(N(WWWS(EE|SWSSE(SWWSW(SEENES|NNENNNENWWNN(WNNWNWS(WNWNN(EENESE(S(WWW|SE(SS|N))|N)|WW(NEWS|)SS(ENSW|)WNW(NEWS|)SSEESWWS(NEENWWEESWWS|))|SSSWSEEN(NN|ESS(W|SE(N|S))))|EES(ENSW|)W))|N))|N)|EEEE(SWEN|)EEE)))|W))|S)))))|N)))|SSS)))|SS)|S))))))|E(ESENSWNW|)N)|NN)|E)|N)|W)|W)|N(NNNWESSS|)W))|SS)|WSS(WNSE|)S))|SSSEES(E(NEWS|)SWSSE(SENSWN|)N|WWWWW(N(EE|WN(NES|WS))|S)))))|NN)$"""
