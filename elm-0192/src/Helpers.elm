module Helpers exposing (..)

import Array
import Char
import Dict
import Html
import Parser exposing ((|.), (|=))
import String


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
    String.toInt  >> uM


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
