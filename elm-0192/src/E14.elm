module E14 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers
import Parser exposing (..)
import Set exposing (Set)


main =
    Helpers.makeMain
        [ Debug.toString
            (result1 input)
        , Debug.toString result2
        ]


parsed =
    String.split "," input |> List.map Helpers.toI |> Array.fromList


result1 string =
    initialState1 string |> take fuel |> Debug.log "state" |> .used |> Dict.get rootChemical


result2 =
    binSearch low_ high_


fuel =
    Chemical 1 "FUEL"


rootChemical =
    "ORE"


type alias Chemical =
    { quantity : Int, name : String }


type alias Reaction =
    { inputs : List Chemical, output : Chemical }


type alias RDict =
    Dict String Reaction


chemicalParser : Parser Chemical
chemicalParser =
    succeed Chemical
        |= int
        |. spaces
        |= variable
            { inner = Char.isAlphaNum
            , start = Char.isUpper
            , reserved = Set.empty
            }


reactionParser : Parser Reaction
reactionParser =
    succeed Reaction
        |= sequence
            { start = ""
            , separator = ","
            , end = "=> "
            , spaces = spaces
            , item = chemicalParser
            , trailing = Forbidden
            }
        |= chemicalParser


parse : String -> List Reaction
parse s =
    s |> String.lines |> List.map (Parser.run reactionParser >> Helpers.uR)


toRDict : List Reaction -> RDict
toRDict l =
    l |> List.map (\r -> Tuple.pair r.output.name r) |> Dict.fromList


type alias CDict =
    Dict String Int


type alias State =
    { available : CDict, used : CDict, rdict : RDict }


use : Chemical -> State -> State
use chemical state =
    { state
        | available =
            state.available
                |> Dict.update chemical.name
                    (Maybe.withDefault 0
                        >> (-) chemical.quantity
                        >> (*) -1
                        >> Just
                    )
        , used =
            state.used
                |> Dict.update chemical.name
                    (Maybe.withDefault 0
                        >> (+) chemical.quantity
                        >> Just
                    )
    }


take : Chemical -> State -> State
take chemical state =
    let
        amountAvailable =
            state.available |> Dict.get chemical.name |> Maybe.withDefault 0

        stateAfterAdding =
            if amountAvailable >= chemical.quantity then
                state
            else
                replenish { chemical | quantity = chemical.quantity - amountAvailable } state
    in
    stateAfterAdding |> use chemical


add : Chemical -> State -> State
add chemical state =
    { state
        | available =
            state.available
                |> Dict.update chemical.name
                    (Maybe.withDefault 0
                        >> (+) chemical.quantity
                        >> Just
                    )
    }


replenish : Chemical -> State -> State
replenish chemical state =
    if chemical.name == rootChemical then
        add chemical state
    else
        let
            reaction =
                Dict.get chemical.name state.rdict |> Helpers.uM

            quantityNeeded =
                ceiling (toFloat chemical.quantity / toFloat reaction.output.quantity)
        in
        reaction.inputs
            |> List.map (\inputChemical -> { inputChemical | quantity = inputChemical.quantity * quantityNeeded })
            |> List.foldl take state
            |> add { chemical | quantity = quantityNeeded * reaction.output.quantity }


initialState1 string =
    parse string |> toRDict |> State Dict.empty Dict.empty


goal =
    1000000000000


low_ =
    goal // 387001


high_ =
    goal * 3 // (387001 * 2)


ins =
    initialState1 input


binSearch : Int -> Int -> Int
binSearch low high =
    if low + 1 >= high then
        low
    else
        let
            q =
                (low + high) // 2

            res =
                ins |> take { fuel | quantity = q } |> .used |> Dict.get rootChemical |> Helpers.uM

            _ =
                Debug.log "bs" { res = res, low = low, high = high }
        in
        if res > goal then
            binSearch low q
        else
            binSearch q high


testInput1 =
    """10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"""


testInput2 =
    """157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"""


testInput3 =
    """171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX"""


input =
    """13 WDSR, 16 FXQB => 6 BSTCB
185 ORE => 9 BWSCM
1 WDSR => 9 RLFSK
5 LCGL, 7 BWSCM => 9 BSVW
6 NLSL => 3 MJSQ
1 JFGM, 7 BSVW, 7 XRLN => 6 WDSR
3 WZLFV => 3 BZDPT
5 DTHZH, 12 QNTH, 20 BSTCB => 4 BMXF
18 JSJWJ, 6 JLMD, 6 TMTF, 3 XSNL, 3 BWSCM, 83 LQTJ, 29 KDGNL => 1 FUEL
1 LWPD, 28 RTML, 16 FDPM, 8 JSJWJ, 2 TNMTC, 20 DTHZH => 9 JLMD
1 SDVXW => 6 BPTV
180 ORE => 7 JFGM
13 RLFSK, 15 HRKD, 1 RFQWL => 5 QNTH
1 RFQWL, 3 NZHFV, 18 XRLN => 9 HRKD
2 NLSL, 2 JXVZ => 5 GTSJ
19 SDVXW, 2 BSVW, 19 XRLN => 6 QMFV
1 CSKP => 8 LQTJ
4 ZSZBN => 5 RBRZT
8 WZLFV, 3 QNWRZ, 1 DTHZH => 4 RTRN
1 CGXBG, 1 PGXFJ => 3 TNMTC
4 CGCSL => 7 RNFW
9 CGCSL, 1 HGTL, 3 BHJXV => 8 RSVR
5 NGJW => 8 HTDM
21 FPBTN, 1 TNMTC, 2 RBRZT, 8 BDHJ, 28 WXQX, 9 RNFW, 6 RSVR => 1 XSNL
2 WZLFV => 5 BHJXV
10 BSTCB, 4 NLSL => 4 HQLHN
1 JFGM => 7 SDVXW
6 CSKP => 8 FXQB
6 TNMTC, 4 BZDPT, 1 BPTV, 18 JSJWJ, 2 DTHZH, 1 LWPD, 8 RTML => 8 KDGNL
6 XFGWZ => 7 CGCSL
3 GTSJ => 4 LWPD
1 WDSR, 1 QNWRZ => 5 XFGWZ
11 CSKP, 10 SDVXW => 4 QNWRZ
7 BSVW, 4 QMFV => 1 RFQWL
12 QNTH, 10 HTDM, 3 WXQX => 3 FDPM
2 HGTL => 7 PGXFJ
14 SDVXW => 6 CSKP
11 HQLHN, 1 GTSJ, 1 QNTH => 5 TMTF
173 ORE => 9 LCGL
4 WXQX => 9 BDHJ
5 BZDPT => 7 NGJW
1 GTSJ, 23 QNWRZ, 6 LQTJ => 7 JSJWJ
23 NZHFV, 3 HQLHN => 6 DTHZH
2 JFGM => 4 XRLN
20 CGCSL => 9 WXQX
2 BSTCB, 3 HRKD => 9 NLSL
1 MJSQ, 1 BPTV => 8 CGXBG
1 RTRN, 1 RSVR => 3 ZSZBN
2 NZHFV, 1 BSTCB, 20 HRKD => 1 JXVZ
2 BZDPT => 5 HGTL
1 ZSZBN, 14 FDPM => 9 RTML
3 BMXF => 8 FPBTN
1 SDVXW, 8 XRLN => 9 NZHFV
18 QNWRZ, 7 RLFSK => 1 WZLFV"""
