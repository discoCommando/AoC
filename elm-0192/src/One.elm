module One exposing (..)

import Array exposing (..)
import Dict exposing (Dict)
import Helpers exposing (..)
import Html exposing (..)
import Set


main =
    makeMain [ Debug.toString result1, Debug.toString (calc 1969) ]


result1 =
    input |> String.lines |> List.map toI |> List.map (\i -> calc (i // 3 - 2)) |> List.sum


calc : Int -> Int
calc i =
    if i < 0 then
        0

    else
        let
            _ =
                Debug.log "a" i
        in
        i + calc (i // 3 - 2)



--result2 =
--    input
--        |> String.lines
--        |> List.map toI
--        |> Array.fromList
--        |> (\a ->
--                walk a Set.empty 0 0
--           )


input : String
input =
    """94794
58062
112067
139512
147400
99825
142617
107263
86294
97000
140204
72573
134981
111385
88303
79387
129111
122976
130685
75100
146566
73191
107641
109940
65518
102028
57370
144556
64017
64384
145114
115853
87939
90791
133443
139050
140657
85738
133749
92466
142918
96679
125035
127629
87906
104478
105147
121741
70312
73732
60838
82292
102931
103000
135903
78678
86314
50772
115673
106179
60615
105152
76550
140591
120916
62094
111273
63542
102974
78837
94840
89126
63150
52503
108530
101458
59660
116913
66440
83306
50693
58377
62005
130663
124304
79726
63001
73380
64395
124277
69742
63465
93172
142068
120081
119872
52801
100693
79229
90365"""
