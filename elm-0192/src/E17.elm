module E17 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Html
import Intcode
import Parser exposing (..)
import Set exposing (Set)


main =
    Helpers.monospaceMain
        [ Helpers.makeMain [ Debug.toString result1, Debug.toString result2 ]
        , result3
        ]


parsed =
    String.split "," input |> List.map Helpers.toI |> List.indexedMap Tuple.pair |> Dict.fromList


type Output
    = T Tile
    | S Direction
    | Newline


intToOutput : Int -> Output
intToOutput i =
    case Char.fromCode i of
        '#' ->
            T Scaffold

        '.' ->
            T Empty

        '\n' ->
            Newline

        '^' ->
            S U

        '>' ->
            S R

        '<' ->
            S L

        'v' ->
            S D

        _ ->
            Debug.todo "intToOutput"


type Tile
    = Scaffold
    | Empty


intToTile i =
    case i of
        35 ->
            Scaffold

        _ ->
            Empty


type alias Position =
    ( Int, Int )


type alias Board =
    Dict Position (List Position)



-- where can I come from


type alias State =
    { state : Intcode.State
    , shipPosition : Position
    , shipDirection : Direction
    , board : Board
    , path : Path
    }


type Direction
    = U
    | D
    | L
    | R


directionToInt i =
    case Char.fromCode i of
        '^' ->
            U

        '>' ->
            R

        '<' ->
            L

        'v' ->
            D

        _ ->
            Debug.todo "directionToInt"


initialState : State
initialState =
    { state =
        { array =
            parsed
        , input = []
        , output = []
        , relativeBase = 0
        , status = Intcode.Going 0
        }
            |> Intcode.walk
    , shipPosition = ( 0, 0 )
    , shipDirection = U
    , board = Dict.empty
    , path = []
    }


createInitialBoard : Position -> List Int -> State -> State
createInitialBoard ( x, y ) outputs state =
    case outputs of
        [] ->
            state

        i :: rest ->
            case intToOutput i of
                T tile ->
                    case tile of
                        Empty ->
                            createInitialBoard ( x + 1, y ) rest state

                        Scaffold ->
                            createInitialBoard ( x + 1, y ) rest { state | board = state.board |> Dict.insert ( x, y ) [] }

                S ship ->
                    createInitialBoard ( x + 1, y ) rest { state | board = state.board |> Dict.insert ( x, y ) [], shipPosition = ( x, y ), shipDirection = ship }

                Newline ->
                    createInitialBoard ( 0, y + 1 ) rest state


move : Direction -> Position -> Position
move direction ( x, y ) =
    case direction of
        U ->
            ( x, y - 1 )

        D ->
            ( x, y + 1 )

        L ->
            ( x - 1, y )

        R ->
            ( x + 1, y )


turnAround : Direction -> Direction
turnAround d =
    case d of
        U ->
            D

        D ->
            U

        L ->
            R

        R ->
            L


type Turn
    = Left
    | Right


performTurn : Turn -> Direction -> Direction
performTurn turn direction =
    case turn of
        Left ->
            case direction of
                U ->
                    L

                L ->
                    D

                D ->
                    R

                R ->
                    U

        Right ->
            direction
                |> performTurn Left
                |> performTurn Left
                |> performTurn Left


type Cell
    = M Int
    | T_ Turn


type alias Path =
    List Cell


addToPath : Path -> Path
addToPath p =
    case p of
        (M x) :: rest ->
            M (x + 1) :: rest

        _ ->
            Debug.todo "addToPath"


populateTheBoard : Position -> Position -> Direction -> Board -> Path -> ( Board, Path )
populateTheBoard previousPosition position direction board path =
    case Dict.get position board of
        Just previouss ->
            populateTheBoard position
                (move direction position)
                direction
                (board |> Dict.insert position (previousPosition :: previouss))
                (addToPath path)

        Nothing ->
            let
                possiblePositions =
                    [ Left, Right ]
                        |> List.filterMap
                            (\turn ->
                                let
                                    d =
                                        performTurn turn direction

                                    newpos =
                                        previousPosition |> move d
                                in
                                Dict.get newpos board |> Maybe.map (\_ -> ( d, newpos, turn ))
                            )
            in
            case possiblePositions of
                [] ->
                    ( board, path |> List.reverse )

                [ ( newDirection, p, turn ) ] ->
                    populateTheBoard previousPosition
                        p
                        newDirection
                        board
                        (M 0 :: T_ turn :: path)

                rest ->
                    Debug.todo (Debug.toString ( direction, rest ))


result1 =
    initialState
        |> createInitialBoard ( 0, 0 ) (initialState.state.output |> List.reverse)
        |> (\s ->
                let
                    ( newBoard, path ) =
                        populateTheBoard
                            s.shipPosition
                            (move s.shipDirection s.shipPosition)
                            s.shipDirection
                            s.board
                            s.path

                    _ =
                        Debug.log "path " s
                in
                { s | board = newBoard, path = path }
           )
        |> .board
        |> Dict.filter (\k v -> List.length v > 1)
        |> Dict.toList
        |> List.map (\( ( x, y ), _ ) -> x * y)
        |> List.sum


initialPatternState : PatternState
initialPatternState =
    { patterns = [], potentialPattern = [], applications = [] }


result2 =
    initialState
        |> createInitialBoard ( 0, 0 ) (initialState.state.output |> List.reverse)
        |> (\s ->
                let
                    ( newBoard, path ) =
                        populateTheBoard
                            s.shipPosition
                            (move s.shipDirection s.shipPosition)
                            s.shipDirection
                            s.board
                            s.path

                    _ =
                        Debug.log "path " path
                in
                findPatterns path initialPatternState
                    |> List.map (\ps -> { ps | applications = findApplications ps path })
                    --|> List.filter (\ps -> ps.applications |> List.filter (\a -> List.length a > 3) |> List.length |> (<) 0)
                    |> List.sortBy (.applications >> List.concat >> List.length)
                    |> List.reverse
                    |> Helpers.at 0
                    |> Debug.log "aaa"
                    |> convertPatternState
                    |> Tuple.pair s
           )


result3 =
    { array =
        parsed
            |> Dict.insert 0 2
    , input = Tuple.second result2 |> Debug.log "inpt"
    , output = []
    , relativeBase = 0
    , status = Intcode.Going 0
    }
        |> Intcode.walk
        |> Debug.log "after"
        |> .output
        |> List.reverse
        |> List.map Char.fromCode
        |> String.fromList
        |> String.split "\n"
        |> List.map Html.text
        |> List.intersperse (Html.br [] [])
        |> Html.div []


newPath : Path
newPath =
    [ T_ Right, M 8, T_ Right, M 8, T_ Right, M 4, T_ Right, M 4, T_ Right, M 8, T_ Left, M 6, T_ Left, M 2, T_ Right, M 4, T_ Right, M 4, T_ Right, M 8, T_ Right, M 8, T_ Right, M 8, T_ Left, M 6, T_ Left, M 2 ]


tryMatch : Path -> Path -> Maybe Path
tryMatch pattern path =
    case pattern of
        [] ->
            Just path

        fpattern :: rpattern ->
            case path of
                [] ->
                    Nothing

                fpath :: rpath ->
                    if fpath == fpattern then
                        tryMatch rpattern rpath

                    else
                        Nothing


partialMatch : Path -> PatternState -> List Path
partialMatch path patternState =
    case path of
        [] ->
            [ [] ]

        _ ->
            let
                matches =
                    patternState.patterns
                        |> List.indexedMap
                            (\i pattern ->
                                tryMatch pattern path
                            )
                        |> List.filterMap identity
            in
            case matches of
                [] ->
                    [ path ]

                _ ->
                    matches
                        |> List.map
                            (\rp ->
                                partialMatch rp patternState
                            )
                        |> List.concat


type alias PatternState =
    { potentialPattern : Path
    , patterns : List Path
    , applications : List (List Int)
    }


findPatterns :
    Path -- real path
    -> PatternState
    -> List PatternState
findPatterns path patternState =
    let
        patternStateWithPotential =
            { patternState
                | patterns =
                    patternState.patterns
                        ++ [ patternState.potentialPattern
                                |> List.reverse
                           ]
                , potentialPattern = []
            }
    in
    case patternState.potentialPattern of
        [] ->
            partialMatch path patternState
                |> List.map
                    (\npath ->
                        case npath of
                            [] ->
                                [ patternState ]

                            head :: rest ->
                                case patternState.patterns of
                                    [ _, _, _ ] ->
                                        []

                                    -- no match
                                    _ ->
                                        findPatterns rest { patternState | potentialPattern = [ head ] }
                    )
                |> List.concat

        _ ->
            [ findPatterns path patternStateWithPotential
            , case path of
                [] ->
                    []

                head :: rest ->
                    findPatterns rest { patternState | potentialPattern = head :: patternState.potentialPattern }
            ]
                |> List.concat


findApplications : PatternState -> Path -> List (List Int)
findApplications patternState path =
    case path of
        [] ->
            [ [] ]

        _ ->
            let
                matches =
                    patternState.patterns
                        |> List.indexedMap
                            (\i pattern ->
                                tryMatch pattern path
                                    |> Maybe.map (Tuple.pair i)
                            )
                        |> List.filterMap identity
            in
            matches
                |> List.filterMap
                    (\( i, rest ) ->
                        case findApplications patternState rest of
                            [] ->
                                Nothing

                            x ->
                                x |> List.map ((::) i) |> Just
                    )
                |> List.concat


convertApplications : List Int -> List Int
convertApplications l =
    l
        |> List.map
            (\first ->
                (case first of
                    0 ->
                        'A'

                    1 ->
                        'B'

                    2 ->
                        'C'

                    _ ->
                        Debug.todo "A"
                )
                    |> Char.toCode
            )
        |> List.intersperse 44
        |> (\a -> a ++ [ 10 ])


convertPattern : Path -> List Int
convertPattern p =
    p
        |> List.map
            (\cell ->
                case cell of
                    M x ->
                        x |> String.fromInt

                    T_ Left ->
                        "L"

                    T_ Right ->
                        "R"
            )
        |> String.join ","
        |> String.toList
        |> List.map Char.toCode
        |> (\a -> a ++ [ 10 ])


convertPatternState : PatternState -> List Int
convertPatternState ps =
    (ps.applications
        |> Helpers.at 0
        |> convertApplications
    )
        :: List.map convertPattern ps.patterns
        |> List.concat
        |> (\a -> a ++ [ Char.toCode 'n', 10 ])


ttt =
    [ { applications = [ [ 0, 1, 2, 2, 1, 2 ] ]
      , patterns =
            [ [ M 1, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11 ]
            , [ T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13 ]
            , [ T_ Right, M 13, T_ Left, M 13, T_ Right, M 7 ]
            ]
      , potentialPattern = []
      }
    , { applications = [ [ 0, 1, 1, 2, 1 ] ]
      , patterns =
            [ [ M 1, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13 ]
            , [ T_ Right, M 13, T_ Left, M 13, T_ Right, M 7 ]
            , [ T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13 ]
            ]
      , potentialPattern = []
      }
    , { applications = [ [ 0, 1, 1, 2 ] ]
      , patterns =
            [ [ M 1, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right ]
            , [ M 13, T_ Left, M 13, T_ Right ]
            , [ M 7 ]
            ]
      , potentialPattern = []
      }
    , { applications = [ [ 0, 1, 1, 2 ] ]
      , patterns =
            [ [ M 1, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11 ]
            , [ T_ Right, M 13, T_ Left, M 13 ]
            , [ T_ Right, M 7 ]
            ]
      , potentialPattern = []
      }
    , { applications = [ [ 0, 1, 1, 2 ] ]
      , patterns =
            [ [ M 1, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left ]
            , [ M 11, T_ Right ]
            , [ M 13, T_ Left, M 13, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7 ]
            ]
      , potentialPattern = []
      }
    , { applications = [ [ 0, 1, 1, 2 ] ]
      , patterns =
            [ [ M 1, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11 ]
            , [ T_ Left, M 11 ]
            , [ T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7 ]
            ]
      , potentialPattern = []
      }
    , { applications = [ [ 0, 1, 1, 2 ] ]
      , patterns =
            [ [ M 1, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right, M 11, T_ Left, M 11, T_ Left, M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Left, M 11, T_ Right ]
            , [ M 11, T_ Left ]
            , [ M 11, T_ Right, M 11, T_ Right, M 13, T_ Left, M 13, T_ Right, M 13, T_ Left, M 13, T_ Right, M 7 ]
            ]
      , potentialPattern = []
      }
    ]



--case patternState.patterns of
--    [ _, _, _ ] ->
--        case partialMatch path patternState of
--            [] ->
--                []
--
--            applications ->
--                [ { patternState | applications = applications } ]
--
--    _ ->
--        case patternState.potentialPattern of
--            [] ->
--                case path of
--                    [] ->
--                        []
--
--                    fpath :: rpath ->
--                        findPatterns rpath { patternState | potentialPattern = [ fpath ] }
--
--            _ ->
--
--type Node a
--    = Node Position a a a
--
--
--type NodeRecur
--    = NodeRecur (Node (Maybe NodeRecur))
--
--
--mapNode : (a -> b) -> Node a -> Node b
--mapNode f (Node p l s r) =
--    Node p (f l) (f s) (f r)
--
--
--buildNode :
--    Board
--    -> Set Position
--    -> Direction
--    -> Position
--    -> Maybe NodeRecur
--buildNode board visited direction position =
--    board
--        |> Dict.get position
--        |> Maybe.andThen
--            (\_ ->
--                if visited |> Set.member position then
--                    Nothing
--
--                else
--                    let
--                        newVisited =
--                            visited |> Set.insert position
--                    in
--                    Node position (performTurn Left) identity (performTurn Right)
--                        |> mapNode ((|>) direction)
--                        |> mapNode
--                            (\newDirection ->
--                                move newDirection position
--                                    |> buildNode board newVisited newDirection
--                            )
--                        |> NodeRecur
--                        |> Just
--            )
--
--
--removePartialNodes : Board -> NodeRecur -> Maybe NodeRecur
--removePartialNodes board (NodeRecur (Node position l s r)) =
--    let
--        newBoard =
--            board |> Dict.remove position
--    in
--    case ( l, s, r ) of
--        ( Nothing, Nothing, Nothing ) ->
--            if newBoard |> Dict.isEmpty then
--                Node position l s r
--                    |> NodeRecur
--                    |> Just
--
--            else
--                Nothing
--
--        _ ->
--            Node position l s r
--                |> mapNode (Maybe.andThen <| removePartialNodes newBoard)
--                |> NodeRecur


input =
    """1,330,331,332,109,6690,1102,1,1182,16,1102,1,1505,24,102,1,0,570,1006,570,36,1002,571,1,0,1001,570,-1,570,1001,24,1,24,1106,0,18,1008,571,0,571,1001,16,1,16,1008,16,1505,570,1006,570,14,21102,58,1,0,1105,1,786,1006,332,62,99,21101,333,0,1,21102,73,1,0,1105,1,579,1102,0,1,572,1101,0,0,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,1002,574,1,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1106,0,81,21101,340,0,1,1106,0,177,21102,1,477,1,1106,0,177,21101,0,514,1,21102,1,176,0,1106,0,579,99,21101,0,184,0,1105,1,579,4,574,104,10,99,1007,573,22,570,1006,570,165,1001,572,0,1182,21101,375,0,1,21102,211,1,0,1106,0,579,21101,1182,11,1,21101,222,0,0,1105,1,979,21101,388,0,1,21101,0,233,0,1105,1,579,21101,1182,22,1,21102,1,244,0,1105,1,979,21101,401,0,1,21101,255,0,0,1105,1,579,21101,1182,33,1,21101,0,266,0,1105,1,979,21101,0,414,1,21101,277,0,0,1106,0,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21101,1182,0,1,21102,1,313,0,1106,0,622,1005,575,327,1101,0,1,575,21101,327,0,0,1106,0,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,84,18,0,109,4,2101,0,-3,587,20102,1,0,-1,22101,1,-3,-3,21102,1,0,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1106,0,597,109,-4,2106,0,0,109,5,2102,1,-4,629,21001,0,0,-2,22101,1,-4,-4,21102,1,0,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,652,21001,0,0,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21101,702,0,0,1106,0,786,21201,-1,-1,-1,1106,0,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21101,0,731,0,1106,0,786,1105,1,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21102,756,1,0,1106,0,786,1106,0,774,21202,-1,-11,1,22101,1182,1,1,21102,1,774,0,1106,0,622,21201,-3,1,-3,1105,1,640,109,-5,2105,1,0,109,7,1005,575,802,20101,0,576,-6,21002,577,1,-5,1106,0,814,21102,0,1,-1,21102,0,1,-5,21102,1,0,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,85,-3,22201,-6,-3,-3,22101,1505,-3,-3,1201,-3,0,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21101,0,1,-1,1105,1,924,1205,-2,873,21101,0,35,-4,1105,1,924,2101,0,-3,878,1008,0,1,570,1006,570,916,1001,374,1,374,2102,1,-3,895,1102,1,2,0,2101,0,-3,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,922,20101,0,0,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,85,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,61,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1101,0,1,575,21102,973,1,0,1105,1,786,99,109,-7,2106,0,0,109,6,21101,0,0,-4,21102,0,1,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1106,0,1041,21101,0,-4,-2,1105,1,1041,21101,0,-5,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,2101,0,-2,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,1202,-2,1,0,1105,1,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1106,0,989,21101,0,439,1,1106,0,1150,21102,477,1,1,1106,0,1150,21101,0,514,1,21101,1149,0,0,1106,0,579,99,21101,0,1157,0,1105,1,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,1202,-5,1,1176,1202,-4,1,0,109,-6,2106,0,0,46,7,78,1,84,1,84,1,84,1,84,1,80,13,72,1,3,1,7,1,72,1,3,1,7,1,9,11,52,1,3,1,7,1,9,1,9,1,52,1,3,1,7,1,9,1,9,1,52,1,3,1,7,1,9,1,9,1,44,13,7,1,9,1,9,1,44,1,7,1,11,1,9,1,9,1,44,1,7,1,11,1,9,1,9,1,44,1,7,1,11,1,9,1,9,1,42,11,11,1,9,1,9,1,42,1,1,1,19,1,9,1,9,1,42,1,1,1,19,11,9,11,32,1,1,1,82,1,1,1,82,1,1,1,82,1,1,1,82,1,1,1,72,13,72,1,9,1,74,1,9,11,64,1,19,1,64,1,19,1,64,1,19,1,64,1,19,1,64,1,19,1,64,1,19,1,64,1,19,1,64,11,9,1,74,1,9,1,72,13,72,1,1,1,82,1,1,1,82,1,1,1,82,1,1,1,82,1,1,1,52,11,19,1,1,1,52,1,9,1,19,1,1,1,52,1,9,1,11,11,52,1,9,1,11,1,7,1,54,1,9,1,11,1,7,1,54,1,9,1,11,1,7,1,54,13,5,13,64,1,1,1,5,1,3,1,72,1,1,1,5,1,3,1,72,1,1,1,5,1,3,1,72,1,1,1,5,1,3,1,72,1,1,1,5,1,3,1,72,13,74,1,5,1,78,1,5,1,78,1,5,1,78,1,5,1,78,1,5,1,78,7,66"""
