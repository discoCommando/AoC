module E03 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Intcode
import Parser exposing (..)


main =
    Helpers.makeMain [ Debug.toString result2 ]


type Direction
    = R
    | U
    | D
    | L


type alias Vector =
    { mx : Int, my : Int }


toVector : Direction -> Vector
toVector m =
    case m of
        R ->
            Vector 1 0

        U ->
            Vector 0 -1

        D ->
            Vector 0 1

        L ->
            Vector -1 0


directionParser : Parser Direction
directionParser =
    oneOf
        [ succeed R |. symbol "R"
        , succeed L |. symbol "L"
        , succeed D |. symbol "D"
        , succeed U |. symbol "U"
        ]


type alias Move =
    { direction : Direction, distance : Int }


moveParser : Parser Move
moveParser =
    succeed Move
        |= directionParser
        |= int


movesParser : String -> List Move
movesParser s =
    s |> String.split "," |> List.map (Parser.run moveParser >> Helpers.uR)


parsed : ( List Move, List Move )
parsed =
    ( input1 |> movesParser, input2 |> movesParser )


type Wire
    = First
    | Second


type Cardinality
    = One Wire Int
    | Both Int Int


add : Wire -> Int -> Maybe Cardinality -> Cardinality
add wire l mc =
    case mc of
        Nothing ->
            One wire l

        Just (One anotherWire l2) ->
            if wire == anotherWire then
                One anotherWire l2
                -- we want a smaller length

            else
                Both l2 l

        Just a ->
            a


type alias Board =
    Dict ( Int, Int ) Cardinality


type alias Point =
    { x : Int, y : Int }


type alias State =
    { board : Board, centralPoint : Point, currentPoint : Point, currentLength : Int }


move : Wire -> Move -> State -> State
move wire m state =
    if m.distance == 0 then
        state

    else
        let
            vector : Vector
            vector =
                m.direction |> toVector

            newBoard : Board
            newBoard =
                state.board |> Dict.update ( state.currentPoint.x, state.currentPoint.y ) (add wire state.currentLength >> Just)

            newPoint : Point
            newPoint =
                Point (state.currentPoint.x + vector.mx) (state.currentPoint.y + vector.my)
        in
        { state | board = newBoard, currentPoint = newPoint, currentLength = state.currentLength + 1 }
            |> move wire { m | distance = m.distance - 1 }


initialBoard : Board
initialBoard =
    Dict.empty


startingPoint : Point
startingPoint =
    Point 0 0


initialState =
    State initialBoard startingPoint startingPoint 0


finalState : State
finalState =
    let
        afterFirstWire : State
        afterFirstWire =
            parsed
                |> Tuple.first
                |> List.foldl (move First) initialState

        afterSecondWire : State
        afterSecondWire =
            parsed
                |> Tuple.second
                |> List.foldl (move Second) { afterFirstWire | currentPoint = startingPoint, currentLength = 0 }
    in
    afterSecondWire


distance : Point -> Point -> Int
distance p1 p2 =
    abs (p1.x - p2.x) + abs (p1.y - p2.y)


result1 : Int
result1 =
    finalState.board
        |> Dict.filter
            (\( x, y ) v ->
                case v of
                    Both _ _ ->
                        not (x == startingPoint.x && y == startingPoint.y)

                    _ ->
                        False
            )
        |> Dict.foldl (\( x, y ) _ min_ -> Point x y |> distance startingPoint |> min min_) 10000000000


result2 =
    finalState.board
        |> Dict.foldl
            (\( x, y ) v min_ ->
                if x == 0 && y == 0 then
                    min_

                else
                    case v of
                        Both l1 l2 ->
                            l1
                                |> (+) l2
                                |> min min_

                        _ ->
                            min_
            )
            10000000000


input1 =
    """R990,U803,R777,U157,R629,D493,R498,D606,R344,U241,L708,D403,R943,U961,L107,D755,R145,D77,L654,D297,L263,D904,R405,U676,R674,U139,L746,U935,R186,U433,L739,D774,R470,D459,R865,D209,L217,U525,R747,D218,R432,U769,L876,D477,R606,D161,R991,D338,R647,D958,R777,D148,R593,D873,L95,U707,R468,U518,R845,U285,R221,U771,R989,D107,R44,U833,L343,D420,R468,D954,L604,D270,L691,U401,R850,U70,R441,U461,R638,D743,R65,U673,L999,U110,R266,U759,R768,U569,L250,D577,R247,U420,L227,U437,L80,D647,L778,U935,R585,U35,L735,D201,R694,U635,L597,U215,R743,D542,L701,U946,L503,U589,R836,D687,L444,U409,L473,U132,L570,U374,R193,D908,L800,U294,L252,U851,R947,D647,L37,D20,L27,U620,L534,D356,L291,U611,L128,D670,L364,U200,L749,D708,R776,U99,R606,D999,L810,D373,R212,D138,R856,D966,L206,D23,L860,D731,L914,U716,L212,U225,R766,U348,L220,D69,L766,D15,L557,U71,R734,D295,R884,D822,R300,D152,L986,D170,R764,U24,R394,D710,L860,U830,L305,U431,R201,D44,R882,U667,R37,D727,R916,U460,L834,D771,R373,U96,L707,D576,R607,D351,R577,D200,L402,U364,L32,D512,L152,D283,L232,U804,R827,U352,R104,D323,L254,U273,L451,D967,R739,D53,L908,D866,R998,U897,L581,U538,R206,U644,L70,D17,L481,U912,L377,D922,L286,U547,R35,U292,L318,U256,R79,D52,R92,U160,R763,U428,R663,D634,R212,D325,R460,U142,L375,U382,R20,D321,L220,D578,R915,D465,L797,D849,L281,D491,L911,D624,R800,U629,L675,U428,L219,U694,R680,U350,R113,D903,L22,D683,L787,D1,R93,U315,L562,U756,R622,D533,L587,D216,L933,U972,R506,U536,R797,U828,L12,D965,L641,U165,R937,D675,R259"""


input2 =
    """L998,D197,L301,D874,L221,U985,L213,D288,R142,D635,R333,D328,R405,D988,L23,D917,R412,D971,R876,U527,R987,D884,R39,D485,L971,U200,R931,U79,L271,U183,R354,D18,R346,D866,L752,D204,L863,U784,R292,U676,R811,U721,L53,U983,L993,U822,R871,U539,L782,D749,R417,U667,R882,U467,R321,U894,R912,U756,L102,U154,L57,D316,R200,U372,L44,U406,L426,D613,R847,U977,R303,U469,R509,U839,L633,D267,L487,D976,R325,U399,L359,U161,L305,U935,R522,D848,R784,D273,L337,D55,L266,U406,L858,D650,L176,D124,R231,U513,L462,U328,L674,D598,R568,D742,L39,D438,L643,D254,R577,U519,R325,U124,R91,U129,L79,D52,R480,D46,R129,D997,R452,D992,L721,U490,L595,D666,R372,D198,R813,U624,L469,U59,R578,U184,R117,D749,L745,U302,R398,D951,L683,D360,R476,D788,R70,U693,R295,D547,L61,U782,R440,D818,L330,D321,L968,U622,R160,U571,L886,D43,L855,U272,R530,D267,L312,D519,L741,D697,R206,U148,L445,U857,R983,D192,L788,U826,R805,U932,R888,D250,L682,D52,R406,D176,R984,D637,L947,D416,L687,U699,L544,D710,L933,D171,L357,D134,L968,D538,R496,D240,L730,U771,R554,U708,R265,D748,L839,U668,L333,U335,R526,U809,L653,D6,R234,D130,R871,U911,R538,U372,L960,D535,L196,U236,L966,D185,L166,U789,L885,U453,R627,D586,R501,U222,L280,U124,R755,D159,L759,U78,R669,D889,L150,D888,L71,D917,L126,D97,L138,U726,R160,D971,R527,D988,R455,D413,R539,U923,R258,U734,L459,D954,R877,U613,R343,D98,R238,U478,R514,U814,L274,U119,L958,U698,R761,U693,R367,D111,L800,D531,L91,U616,R208,D255,R169,U145,R671,U969,L468,U566,R589,D455,R323,D303,R374,D890,R377,D262,L40,U85,L719"""
