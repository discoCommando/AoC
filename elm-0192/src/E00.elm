module E00 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Parser exposing (..)
import Set exposing (Set)


main =
    Helpers.makeMain [ Debug.toString result1, Debug.toString result2 ]


parsed =
    String.split "," input |> List.map Helpers.toI |> Array.fromList


result1 =
    ""


result2 =
    ""


input =
    """"""
