module Helpers exposing (..)

import Array
import Browser
import Char
import Dict
import Html
import Parser exposing ((|.), (|=))
import Process
import String
import Task exposing (Task)
import Time exposing (Posix)


at : Int -> List x -> x
at i l =
    case l |> List.drop i |> List.head of
        Just x ->
            x

        _ ->
            Debug.todo "index out of bounds"


replaceAt : Int -> (x -> x) -> List x -> List x
replaceAt i f l =
    List.take i l ++ [ at i l |> f ] ++ List.drop (i + 1) l


uM : Maybe a -> a
uM m =
    case m of
        Just a ->
            a

        _ ->
            Debug.todo "unsafeMaybe"


uR : Result x a -> a
uR r =
    case r of
        Ok x ->
            x

        _ ->
            Debug.todo "unsafeResult"


uG : Int -> Array.Array a -> a
uG i a =
    uM (Array.get i a)


unsafeReplaceAt : Int -> (a -> a) -> Array.Array a -> Array.Array a
unsafeReplaceAt index f arr =
    let
        v =
            uG index arr
    in
    Array.set index (f v) arr


tf : ( a, x ) -> a
tf ( a, _ ) =
    a


ts : ( a, x ) -> x
ts ( _, x ) =
    x


fldl : (a -> x -> x) -> x -> List a -> x
fldl =
    List.foldl


jN : Maybe x
jN =
    Nothing


jJ : x -> Maybe x
jJ =
    Just


makeMain : List String -> Html.Html msg
makeMain =
    List.map (Html.text >> List.singleton >> Html.p []) >> Html.div []


toI : String -> Int
toI =
    String.toInt >> uM


countApp : comparable -> Dict.Dict comparable Int -> Dict.Dict comparable Int
countApp a =
    Dict.update a
        (\x ->
            case x of
                Nothing ->
                    Just 1

                Just a_ ->
                    Just (a_ + 1)
        )


updD : comparable -> b -> (b -> b) -> Dict.Dict comparable b -> Dict.Dict comparable b
updD k a f =
    Dict.update k
        (\x ->
            case x of
                Nothing ->
                    Just a

                Just z ->
                    Just (f z)
        )


parseAtMost : Int -> Parser.Parser a -> Parser.Parser (List a)
parseAtMost len parser =
    case len of
        0 ->
            Parser.succeed []

        _ ->
            Parser.oneOf
                [ Parser.succeed (::)
                    |= parser
                    |= parseAtMost (len - 1) parser
                , Parser.succeed []
                ]


log : a -> a
log =
    Debug.log "XD"


foldState : (a -> b -> ( b, c )) -> b -> List a -> ( b, List c )
foldState f b_ la =
    la
        |> List.foldl
            (\a ( b, lc ) ->
                let
                    ( newB, c ) =
                        f a b
                in
                ( newB, c :: lc )
            )
            ( b_, [] )


logIf : String -> x -> Bool -> ()
logIf m v b =
    if b then
        Debug.log m v |> (\_ -> ())

    else
        ()


type alias Computation =
    { name : String
    , compute : () -> String
    }


type alias ComputationResult =
    { name : String
    , value : String
    }


type alias Measurement =
    { name : String
    , value : String
    , time : Int
    }


type Msg
    = Time Posix
    | Tick


type ComputationState
    = Started Posix Computation
    | Finished Posix ComputationResult
    | NoneRunning


type alias Model =
    { inQueue : List Computation
    , loading : ComputationState
    , loaded : List Measurement
    }


init : List Computation -> ( Model, Cmd Msg )
init computations =
    ( Model computations NoneRunning [], Task.perform (\_ -> Tick) (Process.sleep 0) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            case model.loading of
                NoneRunning ->
                    ( model, Task.perform Time Time.now )

                Started startPosix computation ->
                    let
                        computationResult =
                            ComputationResult computation.name (computation.compute ())
                    in
                    ( Model model.inQueue (Finished startPosix computationResult) model.loaded, Task.perform Time Time.now )

                _ ->
                    Debug.todo "shouldn't happen"

        Time posix ->
            case model.loading of
                NoneRunning ->
                    case model.inQueue of
                        [] ->
                            ( model, Cmd.none )

                        computation :: rest ->
                            ( Model rest (Started posix computation) model.loaded, Task.perform (\_ -> Tick) <| Process.sleep 0 )

                Started startPosix computation ->
                    Debug.todo "shouldn't happen 3"

                Finished startPosix computationResult ->
                    let
                        time =
                            Time.posixToMillis posix - Time.posixToMillis startPosix
                    in
                    ( Model model.inQueue NoneRunning (model.loaded ++ [ Measurement computationResult.name computationResult.value time ])
                    , Task.perform (\_ -> Tick) <| Process.sleep 0
                    )


view : Model -> Html.Html msg
view model =
    Html.div [] <|
        List.concat
            [ model.loaded
                |> List.map
                    (\measurement ->
                        [ "name:"
                        , measurement.name
                        , "value:"
                        , measurement.value
                        , "run in: "
                        , measurement.time |> Debug.toString
                        , "ms"
                        ]
                            |> String.join " "
                            |> Html.text
                            |> List.singleton
                            |> Html.p []
                    )
            , List.singleton <|
                case model.loading of
                    NoneRunning ->
                        Html.text ""

                    Started _ computation ->
                        [ "name:"
                        , computation.name
                        , "has started"
                        ]
                            |> String.join " "
                            |> Html.text
                            |> List.singleton
                            |> Html.p []

                    Finished _ result ->
                        [ "name:"
                        , result.name
                        , "value:"
                        , result.value
                        ]
                            |> String.join " "
                            |> Html.text
                            |> List.singleton
                            |> Html.p []
            , model.inQueue
                |> List.map
                    (\computation ->
                        [ "name:"
                        , computation.name
                        , "is in the queue"
                        ]
                            |> String.join " "
                            |> Html.text
                            |> List.singleton
                            |> Html.p []
                    )
            ]


makeAppWithMeasurements : List Computation -> Program {} Model Msg
makeAppWithMeasurements computations =
    Browser.element
        { init = \_ -> init computations
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
