module E12 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Parser exposing (..)
import Set exposing (Set)


main =
    Helpers.makeMain
        [ Debug.toString (result1 testInput2)
        , Debug.toString (result2 testInput)
        , Debug.toString (result2 testInput2)
        , Debug.toString (result3 input)
        ]


parsed =
    String.split "," input |> List.map Helpers.toI |> Array.fromList


result1 s =
    initialState s |> repeat 10 step |> totalEnergy


result2 s =
    initialState2 s |> repeatUntil |> getCycle |> List.length


join : (a -> a -> a -> b) -> Triple a -> b
join f triple =
    f triple.x triple.y triple.z


nwd : Int -> Int -> Int
nwd i j =
    if j == 0 then
        i
    else
        nwd j (i |> modBy j)


nww : Int -> Int -> Int
nww i j =
    let
        _ =
            Debug.log "i * j" (i * j)

        _ =
            Debug.log "nwd i j" (nwd i j)
    in
    udd (i * j) (nwd i j)


udd : Int -> Int -> Int
udd i j =
    case i |> modBy j of
        0 ->
            round (toFloat i / toFloat j)

        x ->
            Debug.todo (Debug.toString x)


result3 s =
    initialState s
        |> List.map (\pl -> pure Tuple.pair |> andMap pl.coords |> andMap pl.velocity)
        |> traverse
        |> Debug.log "c"
        |> map (\a -> getCycle2 Set.empty (a |> List.map (\( c, v ) -> { coord = c, velocity = v })) 0)
        |> Debug.log "d"
        |> (\triple ->
                let
                    nn =
                        nww (nww triple.x triple.y) triple.z |> Debug.log "s"
                in
                nn
           )


type alias Triple a =
    { x : a, y : a, z : a }


toComparable : Triple comparable -> ( comparable, comparable, comparable )
toComparable { x, y, z } =
    ( x, y, z )


mapBoth_ : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth_ f ( a, b ) =
    ( f a, f b )


type alias ComparableState =
    List ( ( Int, Int, Int ), ( Int, Int, Int ) )


planetToComparable : Planet -> ( ( Int, Int, Int ), ( Int, Int, Int ) )
planetToComparable p =
    Tuple.pair .coords .velocity |> mapBoth_ ((|>) p >> toComparable)


stateToComparable : State -> ComparableState
stateToComparable s =
    List.map planetToComparable s |> List.sort


type alias Planet =
    { velocity : Triple Int, coords : Triple Int }


type alias State =
    List Planet


map : (a -> b) -> Triple a -> Triple b
map f t =
    Triple (f t.x) (f t.y) (f t.z)


andMap : Triple a -> Triple (a -> b) -> Triple b
andMap t2 t1 =
    Triple (t1.x t2.x) (t1.y t2.y) (t1.z t2.z)


pure : a -> Triple a
pure a =
    Triple a a a


traverse : List (Triple a) -> Triple (List a)
traverse l =
    case l of
        [] ->
            pure []

        x :: xs ->
            pure (::)
                |> andMap x
                |> andMap (traverse xs)


initialState : String -> State
initialState s =
    s
        |> String.lines
        |> List.map (parseTriple >> Debug.log "as" >> Planet (pure 0))


type alias State2 =
    { state : State, counter : Int, list : List State, dict : Dict ComparableState State }


initialState2 : String -> State2
initialState2 s =
    { dict = Dict.empty, list = [], counter = 0, state = initialState s }


repeatUntil : State2 -> State2
repeatUntil state2 =
    let
        cs =
            stateToComparable state2.state
    in
    case Dict.get cs state2.dict of
        Just x ->
            state2

        Nothing ->
            let
                newS =
                    step state2.state
            in
            repeatUntil { state = newS, dict = state2.dict |> Dict.insert cs state2.state, list = state2.state :: state2.list, counter = state2.counter + 1 }


getUntil : (a -> Bool) -> List a -> List a
getUntil f l =
    case l of
        [] ->
            []

        a :: rest ->
            if f a then
                a :: getUntil f rest
            else
                []


getCycle : State2 -> List State
getCycle state2 =
    state2.list |> getUntil (\s -> stateToComparable s /= stateToComparable state2.state) |> List.reverse |> (::) state2.state


type alias SingleCoord =
    { coord : Int, velocity : Int }


type alias SingleState =
    List SingleCoord


stepSingle : SingleState -> SingleState
stepSingle state =
    state
        |> List.indexedMap
            (\i single ->
                let
                    newRelationalVelocity =
                        state
                            |> List.indexedMap
                                (\j single2 ->
                                    if i == j then
                                        0
                                    else if single.coord > single2.coord then
                                        -1
                                    else if single.coord == single2.coord then
                                        0
                                    else
                                        1
                                )
                            |> List.sum

                    newVelocity =
                        (+) single.velocity newRelationalVelocity

                    newCoord =
                        (+) single.coord newVelocity
                in
                { coord = newCoord, velocity = newVelocity }
            )


singleStateToComparable : SingleState -> List ( Int, Int )
singleStateToComparable =
    List.map (\x -> ( x.coord, x.velocity ))


getCycle2 : Set (List ( Int, Int )) -> SingleState -> Int -> Int
getCycle2 set state counter =
    let
        comparableState =
            singleStateToComparable state
    in
    case set |> Set.member comparableState of
        True ->
            counter

        False ->
            let
                newState =
                    stepSingle state

                newSet =
                    Set.insert comparableState set
            in
            getCycle2 newSet newState (counter + 1)



--type Planet2 = Center (Triple Int) |


step : State -> State
step state =
    let
        ff =
            state |> Helpers.at 0 |> .coords |> .x

        _ =
            Debug.log "state" (state |> List.map (\x -> ( x.coords.x - ff, x.velocity.x )))
    in
    state
        |> List.indexedMap
            (\i planet ->
                let
                    newRelationalVelocity : Triple Int
                    newRelationalVelocity =
                        state
                            |> List.indexedMap
                                (\j planet2 ->
                                    if i == j then
                                        pure 0
                                    else
                                        pure
                                            (\a b ->
                                                if a > b then
                                                    -1
                                                else if a == b then
                                                    0
                                                else
                                                    1
                                            )
                                            |> andMap planet.coords
                                            |> andMap planet2.coords
                                )
                            |> traverse
                            |> map List.sum

                    newVelocity : Triple Int
                    newVelocity =
                        pure (+) |> andMap planet.velocity |> andMap newRelationalVelocity

                    newCoords : Triple Int
                    newCoords =
                        pure (+) |> andMap planet.coords |> andMap newVelocity
                in
                { coords = newCoords, velocity = newVelocity }
            )


sumEnergy : Triple Int -> Int
sumEnergy { x, y, z } =
    abs x + abs y + abs z


totalEnergy : State -> Int
totalEnergy state =
    state
        |> List.map
            (\planet ->
                sumEnergy planet.coords * sumEnergy planet.velocity
            )
        |> List.sum


totalEnergy2 : State -> Int
totalEnergy2 state =
    state
        |> List.map
            (\planet ->
                (sumEnergy planet.coords + 1400051) * (sumEnergy planet.velocity + 322057) + 232381
            )
        |> List.sum


repeat : Int -> (a -> a) -> a -> a
repeat i f a =
    let
        _ =
            if modBy 10000 i == 0 then
                Debug.log (Debug.toString i) i
            else
                i
    in
    if i == 0 then
        a
    else
        repeat (i - 1) f (f a)


parseTriple : String -> Triple Int
parseTriple s =
    succeed Triple
        |. symbol "<x="
        |= Helpers.parseInt
        |. symbol ", y="
        |= Helpers.parseInt
        |. symbol ", z="
        |= Helpers.parseInt
        |> Parser.run
        |> (|>) s
        |> Debug.log "a"
        |> Helpers.uR


testInput =
    """<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>"""


testInput2 =
    """<x=0, y=0, z=0>
<x=3, y=0, z=0>
<x=5, y=0, z=0>
<x=4, y=0, z=0>"""


testInput3 =
    """<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>"""


input =
    """<x=-10, y=-13, z=7>
<x=1, y=2, z=1>
<x=-15, y=-3, z=13>
<x=3, y=7, z=-4>"""
