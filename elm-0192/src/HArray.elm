module Helpers.HArray exposing (ForState, for)

import Array exposing (Array)
import Helpers

type alias ForState a x = { array: Array.Array a, state: x }

type alias Loop a x = Int -> Array a -> x -> Maybe (ForState a x)

for : Array a -> x -> Loop a x -> ForState a x
for = forHelper  0

forHelper : Int -> Array a -> x -> Loop a x -> ForState a x
forHelper i a x f =
    if (i >= Array.length a) then ForState a x
    else
        case f i a x of
            Nothing ->
                ForState a x


            Just fs ->
                forHelper (i + 1) fs.array fs.state f


get : Int -> Array a -> ForState a a
get i a =
    Helpers.uG i a |> ForState a

--andThen : (b -> ForState a c) -> ForState a b -> ForState a c
--andThen

for [1,2,3] () (\i () ->
    get i
    |>
)
