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
    Html.div [ Html.Attributes.style "font-family" "\"Courier New\"" ] h


type Main
    = Main (List String)


makeMain2 : Main -> Html.Html a
makeMain2 (Main x) =
    monospaceMain [ makeMain x ]


initMain : Main
initMain =
    Main []


addToMain : a -> Main -> Main
addToMain a (Main x) =
    Main (x ++ [ a |> Debug.toString ])


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


crash : String -> a -> b
crash s a =
    { reason = s, value = a } |> Debug.toString |> Debug.todo
