module Helpers exposing (..)

import Array
import Browser
import Browser.Events
import Char
import Dict
import Html
import Html.Attributes
import Parser exposing ((|.), (|=))
import Process
import String
import Task exposing (Task)
import Time exposing (Posix)


ucons : List a -> ( a, List a )
ucons l =
    case l of
        [] ->
            Debug.todo "ucons"

        a :: rest ->
            ( a, rest )


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

monospaceMain : List (Html.Html a) -> Html.Html a
monospaceMain h =
    Html.div [Html.Attributes.style "font-family" "\"Courier New\""] h

toI : String -> Int
toI =
    String.toInt >> uM


parseInt : Parser.Parser Int
parseInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]


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


parseExactly : Int -> Parser.Parser a -> Parser.Parser (List a)
parseExactly len parser =
    case len of
        0 ->
            Parser.succeed []

        _ ->
            Parser.succeed (::)
                |= parser
                |= parseExactly (len - 1) parser


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
    , waitForTick : Bool
    }


init : List Computation -> ( Model, Cmd Msg )
init computations =
    ( Model computations NoneRunning [] True, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            case model.loading of
                NoneRunning ->
                    ( Model model.inQueue model.loading model.loaded False, Task.perform Time Time.now )

                Started startPosix computation ->
                    let
                        computationResult =
                            ComputationResult computation.name (computation.compute ())
                    in
                    ( Model model.inQueue
                        (Finished startPosix computationResult)
                        model.loaded
                        False
                    , Task.perform Time Time.now
                    )

                _ ->
                    Debug.todo "shouldn't happen"

        Time posix ->
            case model.loading of
                NoneRunning ->
                    case model.inQueue of
                        [] ->
                            ( model, Cmd.none )

                        computation :: rest ->
                            ( Model rest (Started posix computation) model.loaded True, Cmd.none )

                Started startPosix computation ->
                    Debug.todo "shouldn't happen 3"

                Finished startPosix computationResult ->
                    let
                        time =
                            Time.posixToMillis posix - Time.posixToMillis startPosix
                    in
                    ( Model
                        model.inQueue
                        NoneRunning
                        (model.loaded ++ [ Measurement computationResult.name computationResult.value time ])
                        True
                    , Cmd.none
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
            , List.singleton <|
                if model.waitForTick then
                    Html.text " "
                else
                    Html.text "    "
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.waitForTick of
        True ->
            Browser.Events.onAnimationFrame <| \_ -> Tick

        _ ->
            Sub.none


makeAppWithMeasurements : List Computation -> Program {} Model Msg
makeAppWithMeasurements computations =
    Browser.element
        { init = \_ -> init computations
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
